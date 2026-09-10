// Process request and generate response

///|
pub fn process_request(input : Bytes) -> Bytes {
  // Parse GenerateRequest from protobuf
  let reader = @protobuf.BytesReader::from_bytes(input)
  let request : @plugin.GenerateRequest = @protobuf.Read::read(reader) catch {
    _ => @plugin.GenerateRequest::default()
  }

  // Parse plugin options
  let options = parse_plugin_options(request.plugin_options)

  // sqlc can leave sqlc.arg() calls inside ON CONFLICT DO UPDATE bodies
  // without corresponding Query.params entries. Normalize before emitting
  // types, SQL constants, bind expressions, validators, and schemas.
  normalize_sqlc_args_in_request(request, options.backend)

  // Generate code files
  let files : Array[@plugin.File] = []

  // Generate moon.pkg (package config)
  let moon_pkg_content = generate_moon_pkg(options)
  files.push(generated_file("moon.pkg", moon_pkg_content))

  // Generate types (backend-specific for error types)
  let types_content = generate_types(request, options)
  files.push(generated_file("sqlc_types.mbt", types_content))

  // Generate queries (backend-specific)
  let queries_content = match options.backend {
    Sqlite => generate_queries_sqlite(request, options.overrides)
    SqliteJs => generate_queries_sqlite_js(request, options.overrides)
    D1 => generate_queries_d1(request, options.overrides)
    Postgres => generate_queries_postgres(request, options.overrides)
    PostgresJs => generate_queries_postgres_js(request, options.overrides)
    MysqlJs => generate_queries_mysql_js(request, options.overrides)
  }
  files.push(generated_file("sqlc_queries.mbt", queries_content))

  // Generate validators (optional)
  if options.validators {
    let validators_content = generate_validators(request)
    files.push(generated_file("sqlc_validators.mbt", validators_content))
  }

  // Generate JSON Schema (optional). Keep it last so no following file can
  // expose stale wasm/protobuf response-size bugs as content corruption.
  if options.json_schema {
    let schema_content = generate_json_schema(request)
    files.push(generated_file("sqlc_schema.json", schema_content))
  }

  // Self-validation: scan every generated MoonBit file for U+001F / other
  // C0 control characters that have been observed leaking into the output
  // through the wasm-host boundary (issue #17). The JSON schema file is
  // excluded — JSON does permit a wider character set and isn't compiled.
  for f in files {
    let name = f.name
    if name.has_suffix(".mbt") {
      // Reconstruct String from the file bytes. The bytes are already UTF-8
      // (string_to_utf8 above), so a plain ASCII subset round-trips cleanly
      // for the validation purpose — we only inspect C0 control bytes.
      let buf = StringBuilder()
      let contents = f.contents
      for i = 0; i < contents.length(); i = i + 1 {
        let b = contents[i].to_int()
        if b < 128 {
          buf.write_char(b.unsafe_to_char())
        } else {
          // Multi-byte UTF-8 sequence — emit as space for validation
          // purposes (we only care about C0 controls in the ASCII range).
          buf.write_char(' ')
        }
      }
      let view = buf.to_string()
      match validate_no_control_chars(name, view) {
        Some(reason) => abort(reason)
        None => ()
      }
    }
  }

  let response = @plugin.GenerateResponse::new(files)

  // Serialize response to protobuf
  serialize_response(response)
}

///|
fn generated_file(name : String, content : String) -> @plugin.File {
  @plugin.File::new(name, string_to_utf8(trim_trailing_blank_lines(content)))
}

///|
fn trim_trailing_blank_lines(content : String) -> String {
  let mut end = content.length()
  while end >= 2 && content[end - 1] == '\n' && content[end - 2] == '\n' {
    end = end - 1
  }
  if end == content.length() {
    content
  } else {
    substring(content, 0, end)
  }
}

///|
fn generate_moon_pkg(options : Options) -> String {
  let buf = StringBuilder()
  buf.write_string("import {\n")
  match options.backend {
    Sqlite | SqliteJs => {
      buf.write_string("  \"mizchi/sqlite\",\n")
      buf.write_string("  \"moonbitlang/x/encoding\",\n")
    }
    D1 => {
      buf.write_string("  \"mizchi/js/core\",\n")
      buf.write_string("  \"mizchi/cloudflare\",\n")
    }
    Postgres => {
      buf.write_string("  \"moonbit-community/postgres/client\" @pgclient,\n")
      buf.write_string("  \"kaashyapan/postgresx\" @pghelpers,\n")
      buf.write_string("  \"moonbitlang/x/decimal\",\n")
      buf.write_string("  \"moonbitlang/x/uuid\",\n")
      buf.write_string("  \"moonbitlang/x/time\",\n")
      buf.write_string("  \"moonbitlang/core/buffer\",\n")
      buf.write_string("  \"moonbitlang/core/double\",\n")
      buf.write_string("  \"moonbitlang/core/float\",\n")
      buf.write_string("  \"moonbitlang/core/json\",\n")
      buf.write_string("  \"moonbitlang/core/ref\",\n")
      buf.write_string("  \"moonbitlang/core/string\",\n")
    }
    PostgresJs => {
      buf.write_string("  \"mizchi/js/core\",\n")
      buf.write_string("  \"mizchi/npm_typed/pg\",\n")
    }
    MysqlJs => buf.write_string("  \"mizchi/js/core\",\n")
  }
  buf.write_string("}\n\n")

  // Add supported-targets based on backend
  match options.backend {
    D1 | SqliteJs | PostgresJs | MysqlJs =>
      buf.write_string("supported_targets = \"js\"\n\n")
    Sqlite | Postgres => buf.write_string("supported_targets = \"native\"\n\n")
  }

  match options.backend {
    Postgres => buf.write_string("warnings = \"-20-27\"\n\n")
    _ => buf.write_string("")
  }

  // Exclude generated files from formatter
  buf.write_string(
    "formatter(ignore: [ \"sqlc_types.mbt\", \"sqlc_queries.mbt\", \"sqlc_validators.mbt\" ])\n",
  )
  buf.to_string()
}

///|
fn generate_types(
  request : @plugin.GenerateRequest,
  options : Options,
) -> String {
  let buf = StringBuilder()
  buf.write_string("// Generated by sqlc-gen-moonbit\n")
  buf.write_string("// sqlc version: ")
  buf.write_string(request.sqlc_version)
  buf.write_string("\n\n")

  // Define error type only for SQLite backends
  // D1 backend uses @cloudflare.D1Error directly
  // Postgres native uses @postgres.PgError directly
  // PostgresJs/MysqlJs use JS errors directly
  match options.backend {
    Sqlite | SqliteJs => {
      buf.write_string("///| SQL execution error\n")
      buf.write_string("pub suberror SqlError {\n")
      buf.write_string("  SqlError(String)\n")
      buf.write_string("} derive(Debug)\n\n")
    }
    D1 | Postgres | PostgresJs | MysqlJs => ()
  }

  // Collect unique result structs from queries
  let generated_structs : Map[String, Bool] = Map([])

  // Generate result structs for queries that return data
  for query in request.queries {
    if query.cmd == ":one" || query.cmd == ":many" {
      let struct_name = to_pascal_case(query.name) + "Row"
      if !generated_structs.contains(struct_name) && query.columns.length() > 0 {
        generated_structs[struct_name] = true
        match options.backend {
          Postgres =>
            generate_postgres_result_struct(
              buf,
              struct_name,
              query.columns,
              options,
              query,
            )
          _ =>
            generate_result_struct(
              buf,
              struct_name,
              query.columns,
              options.overrides,
              query,
            )
        }
      }
    }
  }

  // API experiment - For postgres native only
  // Pass parameters as labelled parameters directly to the function. This makes it easy to skip null/optional columns
  // Dont generate datastructures for params.
  match options.backend {
    Postgres => ()
    _ =>
      // Generate parameter structs for queries with parameters
      for query in request.queries {
        if query.params.length() > 0 {
          let struct_name = to_pascal_case(query.name) + "Params"
          generate_params_struct(
            buf,
            struct_name,
            query.params,
            options.overrides,
            query,
          )
        }
      }
  }

  // API experiment - For postgres native only
  // Constant literals may have compiler optimisations. Use const instead of let.
  buf.write_string("// SQL queries\n")
  match options.backend {
    Postgres =>
      for query in request.queries {
        let const_name = to_snake_case(query.name) + "_sql"
        buf.write_string("const ")
        buf.write_string(const_name.to_upper())
        buf.write_string(" : String = ")
        buf.write_string(escape_string(query.text))
        buf.write_string("\n\n")
      }
    _ =>
      for query in request.queries {
        let const_name = to_snake_case(query.name) + "_sql"
        buf.write_string("pub let ")
        buf.write_string(const_name)
        buf.write_string(" : String = ")
        buf.write_string(escape_string(query.text))
        buf.write_string("\n\n")
      }
  }

  buf.to_string()
}
