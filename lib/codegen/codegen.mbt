// Backend types

///|
pub enum Backend {
  Sqlite
  D1
  Postgres
}

///|
pub fn Backend::to_string(self : Backend) -> String {
  match self {
    Sqlite => "sqlite"
    D1 => "d1"
    Postgres => "postgres"
  }
}

// Plugin options

///|
pub struct TypeOverride {
  column : String? // "table.column" 形式
  db_type : String? // "INTEGER", "TEXT" など
  moonbit_type : String // 上書き先の型
  nullable : Bool? // 明示的にnullable指定
}

///|
pub struct Options {
  backend : Backend
  validators : Bool
  json_schema : Bool
  overrides : Array[TypeOverride]
}

///|
pub fn Options::default() -> Options {
  { backend: Sqlite, validators: false, json_schema: false, overrides: [] }
}

///|
pub fn parse_plugin_options(data : Bytes) -> Options {
  if data.length() == 0 {
    return Options::default()
  }
  // Parse JSON-like format: {"backend":"d1","validators":true,"json_schema":true,"overrides":[...]}
  let json_str = bytes_to_string_simple(data)
  let backend = if json_str.contains("\"postgres\"") {
    Postgres
  } else if json_str.contains("\"d1\"") {
    D1
  } else {
    Sqlite
  }
  let validators = json_str.contains("\"validators\":true") ||
    json_str.contains("\"validators\": true")
  let json_schema = json_str.contains("\"json_schema\":true") ||
    json_str.contains("\"json_schema\": true")
  let overrides = parse_overrides(json_str)
  { backend, validators, json_schema, overrides }
}

///|
fn find_substring(haystack : String, needle : String) -> Int? {
  let h_len = haystack.length()
  let n_len = needle.length()
  if n_len > h_len {
    return None
  }
  for i = 0; i <= h_len - n_len; i = i + 1 {
    let mut found = true
    for j = 0; j < n_len; j = j + 1 {
      if haystack[i + j] != needle[j] {
        found = false
        break
      }
    }
    if found {
      return Some(i)
    }
  }
  None
}

///|
fn substring(s : String, start : Int, end : Int) -> String {
  let buf = StringBuilder::new()
  for i = start; i < end && i < s.length(); i = i + 1 {
    buf.write_char(s[i].to_int().unsafe_to_char())
  }
  buf.to_string()
}

///|
fn parse_overrides(json_str : String) -> Array[TypeOverride] {
  let overrides : Array[TypeOverride] = []
  // Find "overrides":[ ... ]
  let start_marker = "\"overrides\":["
  let start_marker_alt = "\"overrides\": ["
  let start_idx = match find_substring(json_str, start_marker) {
    Some(idx) => idx + start_marker.length()
    None =>
      match find_substring(json_str, start_marker_alt) {
        Some(idx) => idx + start_marker_alt.length()
        None => return overrides
      }
  }
  // Find matching ]
  let mut depth = 1
  let mut end_idx = start_idx
  for i = start_idx; i < json_str.length() && depth > 0; i = i + 1 {
    let c = json_str[i]
    if c == '[' {
      depth = depth + 1
    } else if c == ']' {
      depth = depth - 1
    }
    end_idx = i
  }
  if depth != 0 {
    return overrides
  }
  let array_content = substring(json_str, start_idx, end_idx)
  // Parse each override object
  parse_override_objects(array_content, overrides)
  overrides
}

///|
fn parse_override_objects(
  content : String,
  overrides : Array[TypeOverride],
) -> Unit {
  // Split by },{ pattern to get individual objects
  let mut start = 0
  let mut brace_depth = 0
  for i = 0; i < content.length(); i = i + 1 {
    let c = content[i]
    if c == '{' {
      if brace_depth == 0 {
        start = i + 1
      }
      brace_depth = brace_depth + 1
    } else if c == '}' {
      brace_depth = brace_depth - 1
      if brace_depth == 0 {
        let obj_content = substring(content, start, i)
        match parse_single_override(obj_content) {
          Some(override_) => overrides.push(override_)
          None => ()
        }
      }
    }
  }
}

///|
fn parse_single_override(obj_content : String) -> TypeOverride? {
  let column = extract_json_string(obj_content, "column")
  let db_type = extract_json_string(obj_content, "db_type")
  let moonbit_type = extract_json_string(obj_content, "moonbit_type")
  match moonbit_type {
    Some(mbt) => {
      let nullable = if obj_content.contains("\"nullable\":true") ||
        obj_content.contains("\"nullable\": true") {
        Some(true)
      } else if obj_content.contains("\"nullable\":false") ||
        obj_content.contains("\"nullable\": false") {
        Some(false)
      } else {
        None
      }
      Some({ column, db_type, moonbit_type: mbt, nullable })
    }
    None => None
  }
}

///|
fn extract_json_string(content : String, key : String) -> String? {
  let pattern = "\"" + key + "\":\""
  let pattern_alt = "\"" + key + "\": \""
  let start_idx = match find_substring(content, pattern) {
    Some(idx) => idx + pattern.length()
    None =>
      match find_substring(content, pattern_alt) {
        Some(idx) => idx + pattern_alt.length()
        None => return None
      }
  }
  // Find closing quote
  let mut end_idx = start_idx
  for i = start_idx; i < content.length(); i = i + 1 {
    if content[i] == '"' {
      end_idx = i
      break
    }
  }
  if end_idx > start_idx {
    Some(substring(content, start_idx, end_idx))
  } else {
    None
  }
}

///|
fn bytes_to_string_simple(b : Bytes) -> String {
  let buf = StringBuilder::new()
  for i = 0; i < b.length(); i = i + 1 {
    let byte = b[i]
    if byte >= 32 && byte < 127 {
      buf.write_char(byte.to_int().unsafe_to_char())
    }
  }
  buf.to_string()
}

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

  // Generate code files
  let files : Array[@plugin.File] = []

  // Generate moon.pkg (package config)
  let moon_pkg_content = generate_moon_pkg(options)
  files.push(@plugin.File::new("moon.pkg", string_to_utf8(moon_pkg_content)))

  // Generate types (backend-specific for error types)
  let types_content = generate_types(request, options)
  files.push(@plugin.File::new("sqlc_types.mbt", string_to_utf8(types_content)))

  // Generate queries (backend-specific)
  let queries_content = match options.backend {
    Sqlite => generate_queries_sqlite(request, options.overrides)
    D1 => generate_queries_d1(request, options.overrides)
    Postgres => generate_queries_postgres(request, options.overrides)
  }
  files.push(
    @plugin.File::new("sqlc_queries.mbt", string_to_utf8(queries_content)),
  )

  // Generate JSON Schema (optional) - moved before validators to avoid serialization bug
  if options.json_schema {
    let schema_content = generate_json_schema(request)
    files.push(
      @plugin.File::new("sqlc_schema.json", string_to_utf8(schema_content)),
    )
  }

  // Generate validators (optional) - last to avoid first-byte corruption
  if options.validators {
    let validators_content = generate_validators(request)
    files.push(
      @plugin.File::new(
        "sqlc_validators.mbt",
        string_to_utf8(validators_content),
      ),
    )
  }
  let response = @plugin.GenerateResponse::new(files)

  // Serialize response to protobuf
  serialize_response(response)
}

///|
fn generate_moon_pkg(options : Options) -> String {
  let buf = StringBuilder::new()
  buf.write_string("import {\n")
  match options.backend {
    Sqlite => {
      buf.write_string("  \"mizchi/sqlite\",\n")
      buf.write_string("  \"moonbitlang/x/encoding\",\n")
    }
    D1 => {
      buf.write_string("  \"mizchi/js/core\",\n")
      buf.write_string("  \"mizchi/cloudflare\",\n")
    }
    Postgres => buf.write_string("  \"mattn/postgres\",\n")
  }
  buf.write_string("}\n\n")

  // Options block
  buf.write_string("options(\n")

  // Add supported-targets based on backend
  match options.backend {
    D1 => buf.write_string("  \"supported-targets\": [ \"js\" ],\n")
    Postgres => buf.write_string("  \"supported-targets\": [ \"native\" ],\n")
    Sqlite => ()
  }

  // Exclude generated files from formatter
  buf.write_string("  \"formatter\": {\n")
  buf.write_string("    \"ignore\": [\n")
  buf.write_string("      \"sqlc_types.mbt\",\n")
  buf.write_string("      \"sqlc_queries.mbt\",\n")
  buf.write_string("      \"sqlc_validators.mbt\"\n")
  buf.write_string("    ]\n")
  buf.write_string("  },\n")
  buf.write_string(")\n")
  buf.to_string()
}

///|
fn generate_types(
  request : @plugin.GenerateRequest,
  options : Options,
) -> String {
  let buf = StringBuilder::new()
  buf.write_string("// Generated by sqlc-gen-moonbit\n")
  buf.write_string("// sqlc version: ")
  buf.write_string(request.sqlc_version)
  buf.write_string("\n\n")

  // Define error type only for SQLite backend
  // D1 backend uses @cloudflare.D1Error directly
  // Postgres backend uses @postgres.ProtocolError and @postgres.SqlError directly
  match options.backend {
    Sqlite => {
      buf.write_string("///| SQL execution error\n")
      buf.write_string("pub suberror SqlError {\n")
      buf.write_string("  SqlError(String)\n")
      buf.write_string("} derive(Show)\n\n")
    }
    D1 => ()
    Postgres => ()
  }

  // Collect unique result structs from queries
  let generated_structs : Map[String, Bool] = {}

  // Generate result structs for queries that return data
  for query in request.queries {
    if query.cmd == ":one" || query.cmd == ":many" {
      let struct_name = to_pascal_case(query.name) + "Row"
      if not(generated_structs.contains(struct_name)) &&
        query.columns.length() > 0 {
        generated_structs[struct_name] = true
        generate_result_struct(
          buf,
          struct_name,
          query.columns,
          options.overrides,
        )
      }
    }
  }

  // Generate parameter structs for queries with parameters
  for query in request.queries {
    if query.params.length() > 0 {
      let struct_name = to_pascal_case(query.name) + "Params"
      generate_params_struct(buf, struct_name, query.params, options.overrides)
    }
  }

  // Generate SQL constants
  buf.write_string("// SQL queries\n")
  for query in request.queries {
    let const_name = to_snake_case(query.name) + "_sql"
    buf.write_string("pub let ")
    buf.write_string(const_name)
    buf.write_string(" : String = ")
    buf.write_string(escape_string(query.text))
    buf.write_string("\n\n")
  }
  buf.to_string()
}

///|
fn generate_queries_sqlite(
  request : @plugin.GenerateRequest,
  overrides : Array[TypeOverride],
) -> String {
  let buf = StringBuilder::new()
  buf.write_string("// Generated by sqlc-gen-moonbit (backend: sqlite)\n\n")

  // Generate helper functions for SQLite
  buf.write_string("///|\nfn cstring(s : String) -> Bytes {\n")
  buf.write_string("  @encoding.encode(@encoding.UTF8, s)\n")
  buf.write_string("}\n\n")
  buf.write_string("///|\nfn bytes_to_string(b : Bytes) -> String {\n")
  buf.write_string("  let decoder = @encoding.decoder(@encoding.UTF8)\n")
  buf.write_string("  decoder.decode_lossy(b[0:b.length()])\n")
  buf.write_string("}\n\n")

  // Generate helper function for checking SQLite errors
  buf.write_string(
    "///|\nfn check_sqlite_error(db : @sqlite.Sqlite3, rc : Int) -> Unit raise SqlError {\n",
  )
  buf.write_string(
    "  if rc != @sqlite.SQLITE_ROW && rc != @sqlite.SQLITE_DONE {\n",
  )
  buf.write_string("    let msg = bytes_to_string(@sqlite.sqlite_errmsg(db))\n")
  buf.write_string("    raise SqlError(msg)\n")
  buf.write_string("  }\n")
  buf.write_string("}\n\n")

  // Generate query functions
  buf.write_string("// Query functions\n")
  for query in request.queries {
    generate_query_function_sqlite(buf, query, overrides)
  }
  buf.to_string()
}

///|
fn generate_queries_d1(
  request : @plugin.GenerateRequest,
  overrides : Array[TypeOverride],
) -> String {
  let buf = StringBuilder::new()
  buf.write_string("// Generated by sqlc-gen-moonbit (backend: d1)\n")
  buf.write_string("// Requires: mizchi/cloudflare\n\n")

  // Generate query functions using cloudflare.mbt D1 types
  buf.write_string("// Query functions\n")
  for query in request.queries {
    generate_query_function_d1(buf, query, overrides)
  }
  buf.to_string()
}

///|
fn generate_queries_postgres(
  request : @plugin.GenerateRequest,
  overrides : Array[TypeOverride],
) -> String {
  let buf = StringBuilder::new()
  buf.write_string("// Generated by sqlc-gen-moonbit (backend: postgres)\n")
  buf.write_string("// Requires: mattn/postgres\n\n")

  // Generate SQL escape helper function
  buf.write_string("///| Escape single quotes for SQL\n")
  buf.write_string("fn sql_escape(s : String) -> String {\n")
  buf.write_string("  s.replace(old=\"'\", new=\"''\")\n")
  buf.write_string("}\n\n")

  // Generate query functions using mattn/postgres types
  buf.write_string("// Query functions\n")
  for query in request.queries {
    generate_query_function_postgres(buf, query, overrides)
  }
  buf.to_string()
}

///|
fn generate_query_function_postgres(
  buf : StringBuilder,
  query : @plugin.Query,
  _overrides : Array[TypeOverride],
) -> Unit {
  let func_name = to_snake_case(query.name)
  let sql_const = to_snake_case(query.name) + "_sql"
  let row_type = to_pascal_case(query.name) + "Row"

  // Return type (sync API with Result)
  let return_type = match query.cmd {
    ":one" => "Result[" + row_type + "?, @postgres.PgError]"
    ":many" => "Result[Array[" + row_type + "], @postgres.PgError]"
    ":execrows" => "Result[Int, @postgres.PgError]"
    ":execlastid" => "Result[Int64, @postgres.PgError]"
    _ => "Result[Unit, @postgres.PgError]"
  }

  // Doc comment
  buf.write_string("///| ")
  buf.write_string(query.name)
  buf.write_string("\n")
  buf.write_string("/// ")
  buf.write_string(sanitize_newlines(query.text))
  buf.write_string("\n")

  // Function signature - sync with Connection type from mattn/postgres
  buf.write_string("pub fn ")
  buf.write_string(func_name)
  buf.write_string("(conn : @postgres.Connection")
  if query.params.length() > 0 {
    buf.write_string(", params : ")
    buf.write_string(to_pascal_case(query.name))
    buf.write_string("Params")
  }
  buf.write_string(") -> ")
  buf.write_string(return_type)
  buf.write_string(" {\n")

  // Build SQL with parameters substituted (string replacement for now)
  if query.params.length() > 0 {
    buf.write_string("  let sql = ")
    buf.write_string(sql_const)
    buf.write_string("\n")
    // Replace $1, $2, etc. with actual values
    for i, param in query.params {
      let col = param.column
      let param_name = if col.name.length() > 0 {
        to_snake_case(col.name)
      } else {
        "p" + param.number.to_string()
      }
      let placeholder = "$" + (i + 1).to_string()
      buf.write_string("  let sql = sql.replace(old=\"")
      buf.write_string(placeholder)
      buf.write_string("\", new=")
      buf.write_string(param_to_postgres_string(col, "params." + param_name))
      buf.write_string(")\n")
    }
    buf.write_string("  let result = conn.query(sql)\n")
  } else {
    buf.write_string("  let result = conn.query(")
    buf.write_string(sql_const)
    buf.write_string(")\n")
  }
  buf.write_string("  match result {\n")
  buf.write_string("    Err(e) => Err(e)\n")
  buf.write_string("    Ok(qr) => {\n")

  // Execute based on command type
  match query.cmd {
    ":one" => {
      buf.write_string("      let rows = qr.rows()\n")
      buf.write_string("      qr.free()\n")
      buf.write_string("      if rows.length() == 0 {\n")
      buf.write_string("        Ok(None)\n")
      buf.write_string("      } else {\n")
      buf.write_string("        let row = rows[0]\n")
      buf.write_string("        Ok(Some({\n")
      for i, col in query.columns {
        buf.write_string("          ")
        buf.write_string(to_snake_case(col.name))
        buf.write_string(": ")
        buf.write_string(column_from_postgres_row(col, i))
        buf.write_string(",\n")
      }
      buf.write_string("        }))\n")
      buf.write_string("      }\n")
    }
    ":many" => {
      buf.write_string("      let rows = qr.rows()\n")
      buf.write_string("      qr.free()\n")
      buf.write_string("      let results : Array[")
      buf.write_string(row_type)
      buf.write_string("] = []\n")
      buf.write_string("      for row in rows {\n")
      buf.write_string("        results.push({\n")
      for i, col in query.columns {
        buf.write_string("          ")
        buf.write_string(to_snake_case(col.name))
        buf.write_string(": ")
        buf.write_string(column_from_postgres_row(col, i))
        buf.write_string(",\n")
      }
      buf.write_string("        })\n")
      buf.write_string("      }\n")
      buf.write_string("      Ok(results)\n")
    }
    ":execrows" => {
      buf.write_string("      let count = qr.affected_rows()\n")
      buf.write_string("      qr.free()\n")
      buf.write_string("      Ok(count)\n")
    }
    ":execlastid" => {
      buf.write_string(
        "      // For INSERT RETURNING, get the returned id from first row\n",
      )
      buf.write_string("      let rows = qr.rows()\n")
      buf.write_string("      qr.free()\n")
      buf.write_string("      if rows.length() > 0 && rows[0].length() > 0 {\n")
      buf.write_string(
        "        Ok(try { @strconv.parse_int64(rows[0][0]) } catch { _ => 0L })\n",
      )
      buf.write_string("      } else {\n")
      buf.write_string("        Ok(0L)\n")
      buf.write_string("      }\n")
    }
    _ => {
      // :exec
      buf.write_string("      qr.free()\n")
      buf.write_string("      Ok(())\n")
    }
  }
  buf.write_string("    }\n")
  buf.write_string("  }\n")
  buf.write_string("}\n\n")
}

///|
fn param_to_postgres_string(col : @plugin.Column, expr : String) -> String {
  let type_name = col.type_.name.to_upper()
  let is_nullable = not(col.not_null)
  if is_nullable {
    // For nullable types, handle Option
    match type_name {
      "TEXT"
      | "VARCHAR"
      | "CHAR"
      | "CHARACTER"
      | "CHARACTER VARYING"
      | "BPCHAR"
      | "CLOB" =>
        "(match " +
        expr +
        " { Some(v) => \"'\" + sql_escape(v) + \"'\"; None => \"NULL\" })"
      "INTEGER"
      | "INT"
      | "INT4"
      | "INT2"
      | "SMALLINT"
      | "INT8"
      | "BIGINT"
      | "SERIAL"
      | "BIGSERIAL" =>
        "(match " + expr + " { Some(v) => v.to_string(); None => \"NULL\" })"
      "REAL"
      | "FLOAT4"
      | "DOUBLE PRECISION"
      | "FLOAT8"
      | "FLOAT"
      | "NUMERIC"
      | "DECIMAL" =>
        "(match " + expr + " { Some(v) => v.to_string(); None => \"NULL\" })"
      "BOOLEAN" | "BOOL" =>
        "(match " +
        expr +
        " { Some(v) => if v { \"true\" } else { \"false\" }; None => \"NULL\" })"
      _ =>
        "(match " +
        expr +
        " { Some(v) => \"'\" + sql_escape(v.to_string()) + \"'\"; None => \"NULL\" })"
    }
  } else {
    match type_name {
      "TEXT"
      | "VARCHAR"
      | "CHAR"
      | "CHARACTER"
      | "CHARACTER VARYING"
      | "BPCHAR"
      | "CLOB" => "\"'\" + sql_escape(" + expr + ") + \"'\""
      "INTEGER"
      | "INT"
      | "INT4"
      | "INT2"
      | "SMALLINT"
      | "INT8"
      | "BIGINT"
      | "SERIAL"
      | "BIGSERIAL" => expr + ".to_string()"
      "REAL"
      | "FLOAT4"
      | "DOUBLE PRECISION"
      | "FLOAT8"
      | "FLOAT"
      | "NUMERIC"
      | "DECIMAL" => expr + ".to_string()"
      "BOOLEAN" | "BOOL" => "(if " + expr + " { \"true\" } else { \"false\" })"
      _ => "\"'\" + sql_escape(" + expr + ".to_string()) + \"'\""
    }
  }
}

///|
fn column_from_postgres_row(col : @plugin.Column, idx : Int) -> String {
  let type_name = col.type_.name.to_upper()
  let idx_str = idx.to_string()
  let is_nullable = not(col.not_null)

  // mattn/postgres returns Array[Array[String]], so we access row[idx]
  // and convert from String to the appropriate type
  // Note: parse_int64/parse_double raise exceptions, so we use try/catch with default
  let conversion = match type_name {
    "INTEGER" | "INT" | "INT4" | "INT2" | "SMALLINT" =>
      "(try { @strconv.parse_int(row[" + idx_str + "]) } catch { _ => 0 })"
    "INT8" | "BIGINT" | "SERIAL" | "BIGSERIAL" =>
      "(try { @strconv.parse_int64(row[" + idx_str + "]) } catch { _ => 0L })"
    "REAL" | "FLOAT4" =>
      "(try { @strconv.parse_double(row[" + idx_str + "]) } catch { _ => 0.0 })"
    "DOUBLE PRECISION" | "FLOAT8" | "FLOAT" | "NUMERIC" | "DECIMAL" =>
      "(try { @strconv.parse_double(row[" + idx_str + "]) } catch { _ => 0.0 })"
    "TEXT"
    | "VARCHAR"
    | "CHAR"
    | "CHARACTER"
    | "CHARACTER VARYING"
    | "BPCHAR" => "row[" + idx_str + "]"
    "BOOLEAN" | "BOOL" =>
      "(row[" + idx_str + "] == \"t\" || row[" + idx_str + "] == \"true\")"
    "TIMESTAMP"
    | "TIMESTAMPTZ"
    | "TIMESTAMP WITHOUT TIME ZONE"
    | "TIMESTAMP WITH TIME ZONE" => "row[" + idx_str + "]"
    "DATE" => "row[" + idx_str + "]"
    "TIME" | "TIMETZ" | "TIME WITHOUT TIME ZONE" | "TIME WITH TIME ZONE" =>
      "row[" + idx_str + "]"
    "UUID" => "row[" + idx_str + "]"
    "JSONB" | "JSON" => "row[" + idx_str + "]"
    "BYTEA" => "row[" + idx_str + "]"
    "INET" | "CIDR" | "MACADDR" | "MACADDR8" => "row[" + idx_str + "]"
    _ => "row[" + idx_str + "]"
  }

  // Wrap in Option if nullable
  if is_nullable {
    "(if row[" +
    idx_str +
    "].length() == 0 { None } else { Some(" +
    conversion +
    ") })"
  } else {
    conversion
  }
}

///|
fn generate_result_struct(
  buf : StringBuilder,
  name : String,
  columns : Array[@plugin.Column],
  overrides : Array[TypeOverride],
) -> Unit {
  buf.write_string("///|\npub struct ")
  buf.write_string(name)
  buf.write_string(" {\n")
  for col in columns {
    buf.write_string("  ")
    buf.write_string(to_snake_case(col.name))
    buf.write_string(" : ")
    buf.write_string(sql_type_to_moonbit(col, overrides))
    buf.write_string("\n")
  }
  buf.write_string("} derive(Show, Eq)\n\n")
}

///|
fn generate_params_struct(
  buf : StringBuilder,
  name : String,
  params : Array[@plugin.Parameter],
  overrides : Array[TypeOverride],
) -> Unit {
  buf.write_string("///|\npub struct ")
  buf.write_string(name)
  buf.write_string(" {\n")
  for param in params {
    let col = param.column
    let param_name = if col.name.length() > 0 {
      to_snake_case(col.name)
    } else {
      "p" + param.number.to_string()
    }
    buf.write_string("  ")
    buf.write_string(param_name)
    buf.write_string(" : ")
    buf.write_string(sql_type_to_moonbit(col, overrides))
    buf.write_string("\n")
  }
  buf.write_string("} derive(Show, Eq)\n\n")

  // Generate constructor function
  buf.write_string("///|\npub fn ")
  buf.write_string(name)
  buf.write_string("::new(")
  for i, param in params {
    let col = param.column
    let param_name = if col.name.length() > 0 {
      to_snake_case(col.name)
    } else {
      "p" + param.number.to_string()
    }
    if i > 0 {
      buf.write_string(", ")
    }
    buf.write_string(param_name)
    buf.write_string(" : ")
    buf.write_string(sql_type_to_moonbit(col, overrides))
  }
  buf.write_string(") -> ")
  buf.write_string(name)
  buf.write_string(" {\n")
  buf.write_string("  { ")
  for i, param in params {
    let col = param.column
    let param_name = if col.name.length() > 0 {
      to_snake_case(col.name)
    } else {
      "p" + param.number.to_string()
    }
    if i > 0 {
      buf.write_string(", ")
    }
    buf.write_string(param_name)
  }
  buf.write_string(", }\n")
  buf.write_string("}\n\n")
}

///|
fn generate_query_function_sqlite(
  buf : StringBuilder,
  query : @plugin.Query,
  _overrides : Array[TypeOverride],
) -> Unit {
  let func_name = to_snake_case(query.name)
  let sql_const = to_snake_case(query.name) + "_sql"
  let row_type = to_pascal_case(query.name) + "Row"

  // Return type with error handling
  let return_type = match query.cmd {
    ":one" => row_type + "?"
    ":many" => "Array[" + row_type + "]"
    ":execrows" => "Int"
    ":execlastid" => "Int64"
    _ => "Unit"
  }

  // Doc comment
  buf.write_string("///| ")
  buf.write_string(query.name)
  buf.write_string("\n")
  buf.write_string("/// ")
  buf.write_string(sanitize_newlines(query.text))
  buf.write_string("\n")

  // Function signature - use low-level Sqlite3 type with raise SqlError
  buf.write_string("pub fn ")
  buf.write_string(func_name)
  buf.write_string("(db : @sqlite.Sqlite3")
  if query.params.length() > 0 {
    buf.write_string(", params : ")
    buf.write_string(to_pascal_case(query.name))
    buf.write_string("Params")
  }
  buf.write_string(") -> ")
  buf.write_string(return_type)
  buf.write_string(" raise SqlError {\n")

  // Prepare statement using low-level API
  buf.write_string("  let stmt = @sqlite.sqlite_prepare(db, cstring(")
  buf.write_string(sql_const)
  buf.write_string("))\n")

  // Bind parameters using low-level sqlite_bind_* functions
  if query.params.length() > 0 {
    for i, param in query.params {
      let col = param.column
      let param_name = if col.name.length() > 0 {
        to_snake_case(col.name)
      } else {
        "p" + param.number.to_string()
      }
      buf.write_string("  ")
      buf.write_string(param_to_bind_call(col, i + 1, "params." + param_name))
      buf.write_string(" |> ignore\n")
    }
  }

  // Execute based on command type using low-level API
  match query.cmd {
    ":one" => {
      buf.write_string("  let rc = @sqlite.sqlite_step(stmt)\n")
      buf.write_string("  if rc == @sqlite.SQLITE_ROW {\n")
      buf.write_string("    let row : ")
      buf.write_string(row_type)
      buf.write_string(" = {\n")
      for i, col in query.columns {
        buf.write_string("      ")
        buf.write_string(to_snake_case(col.name))
        buf.write_string(": ")
        buf.write_string(column_to_moonbit(col, i))
        buf.write_string(",\n")
      }
      buf.write_string("    }\n")
      buf.write_string("    @sqlite.sqlite_finalize(stmt)\n")
      buf.write_string("    Some(row)\n")
      buf.write_string("  } else if rc == @sqlite.SQLITE_DONE {\n")
      buf.write_string("    @sqlite.sqlite_finalize(stmt)\n")
      buf.write_string("    None\n")
      buf.write_string("  } else {\n")
      buf.write_string("    @sqlite.sqlite_finalize(stmt)\n")
      buf.write_string("    check_sqlite_error(db, rc)\n")
      buf.write_string("    None\n")
      buf.write_string("  }\n")
    }
    ":many" => {
      buf.write_string("  let results : Array[")
      buf.write_string(row_type)
      buf.write_string("] = []\n")
      buf.write_string("  while true {\n")
      buf.write_string("    let rc = @sqlite.sqlite_step(stmt)\n")
      buf.write_string("    if rc == @sqlite.SQLITE_ROW {\n")
      buf.write_string("      let row : ")
      buf.write_string(row_type)
      buf.write_string(" = {\n")
      for i, col in query.columns {
        buf.write_string("        ")
        buf.write_string(to_snake_case(col.name))
        buf.write_string(": ")
        buf.write_string(column_to_moonbit(col, i))
        buf.write_string(",\n")
      }
      buf.write_string("      }\n")
      buf.write_string("      results.push(row)\n")
      buf.write_string("    } else if rc == @sqlite.SQLITE_DONE {\n")
      buf.write_string("      break\n")
      buf.write_string("    } else {\n")
      buf.write_string("      @sqlite.sqlite_finalize(stmt)\n")
      buf.write_string("      check_sqlite_error(db, rc)\n")
      buf.write_string("      break\n")
      buf.write_string("    }\n")
      buf.write_string("  }\n")
      buf.write_string("  @sqlite.sqlite_finalize(stmt)\n")
      buf.write_string("  results\n")
    }
    ":execrows" => {
      buf.write_string("  let rc = @sqlite.sqlite_step(stmt)\n")
      buf.write_string("  @sqlite.sqlite_finalize(stmt)\n")
      buf.write_string("  check_sqlite_error(db, rc)\n")
      buf.write_string("  @sqlite.sqlite_changes(db)\n")
    }
    ":execlastid" => {
      buf.write_string("  let rc = @sqlite.sqlite_step(stmt)\n")
      buf.write_string("  @sqlite.sqlite_finalize(stmt)\n")
      buf.write_string("  check_sqlite_error(db, rc)\n")
      buf.write_string("  @sqlite.sqlite_last_insert_rowid(db)\n")
    }
    _ => {
      // :exec
      buf.write_string("  let rc = @sqlite.sqlite_step(stmt)\n")
      buf.write_string("  @sqlite.sqlite_finalize(stmt)\n")
      buf.write_string("  check_sqlite_error(db, rc)\n")
    }
  }
  buf.write_string("}\n\n")
}

///|
fn generate_query_function_d1(
  buf : StringBuilder,
  query : @plugin.Query,
  _overrides : Array[TypeOverride],
) -> Unit {
  let func_name = to_snake_case(query.name)
  let sql_const = to_snake_case(query.name) + "_sql"
  let row_type = to_pascal_case(query.name) + "Row"
  let param_count = query.params.length()

  // Return type with async
  let return_type = match query.cmd {
    ":one" => row_type + "?"
    ":many" => "Array[" + row_type + "]"
    ":execrows" => "Int"
    ":execlastid" => "Int64"
    _ => "Unit"
  }

  // Doc comment
  buf.write_string("///| ")
  buf.write_string(query.name)
  buf.write_string("\n")
  buf.write_string("/// ")
  buf.write_string(sanitize_newlines(query.text))
  buf.write_string("\n")

  // Function signature - async with D1Database type from cloudflare.mbt
  // Uses @cloudflare.D1Error directly (no SqlError wrapper)
  buf.write_string("pub async fn ")
  buf.write_string(func_name)
  buf.write_string("(db : @cloudflare.D1Database")
  if param_count > 0 {
    buf.write_string(", params : ")
    buf.write_string(to_pascal_case(query.name))
    buf.write_string("Params")
  }
  buf.write_string(") -> ")
  buf.write_string(return_type)
  buf.write_string(" raise @cloudflare.D1Error {\n")

  // Prepare and bind statement using cloudflare.mbt API
  // Errors from cloudflare.mbt D1 methods propagate directly
  buf.write_string("  let stmt = db.prepare(")
  buf.write_string(sql_const)
  buf.write_string(")")
  if param_count > 0 {
    buf.write_string(".bind([")
    for i, param in query.params {
      let col = param.column
      let param_name = if col.name.length() > 0 {
        to_snake_case(col.name)
      } else {
        "p" + param.number.to_string()
      }
      if i > 0 {
        buf.write_string(", ")
      }
      buf.write_string(param_to_core_any(col, "params." + param_name))
    }
    buf.write_string("])")
  }
  buf.write_string("\n")

  // Execute based on command type
  match query.cmd {
    ":one" => {
      buf.write_string("  match stmt.first() {\n")
      buf.write_string("    None => None\n")
      buf.write_string("    Some(result) => Some({\n")
      for col in query.columns {
        buf.write_string("      ")
        buf.write_string(to_snake_case(col.name))
        buf.write_string(": ")
        buf.write_string(column_from_core_any(col, "result"))
        buf.write_string(",\n")
      }
      buf.write_string("    })\n")
      buf.write_string("  }\n")
    }
    ":many" => {
      buf.write_string("  let d1_result = stmt.all()\n")
      buf.write_string("  let results_arr = d1_result.get_results()\n")
      buf.write_string("  let results : Array[")
      buf.write_string(row_type)
      buf.write_string("] = []\n")
      buf.write_string("  for row in results_arr {\n")
      buf.write_string("    results.push({\n")
      for col in query.columns {
        buf.write_string("      ")
        buf.write_string(to_snake_case(col.name))
        buf.write_string(": ")
        buf.write_string(column_from_core_any(col, "row"))
        buf.write_string(",\n")
      }
      buf.write_string("    })\n")
      buf.write_string("  }\n")
      buf.write_string("  results\n")
    }
    ":execrows" => {
      buf.write_string("  let result = stmt.run()\n")
      buf.write_string("  match result.meta() {\n")
      buf.write_string("    Some(m) => m.changes().unwrap_or(0)\n")
      buf.write_string("    None => 0\n")
      buf.write_string("  }\n")
    }
    ":execlastid" => {
      buf.write_string("  let result = stmt.run()\n")
      buf.write_string("  match result.meta() {\n")
      buf.write_string(
        "    Some(m) => m.last_row_id().unwrap_or(0).to_int64()\n",
      )
      buf.write_string("    None => 0L\n")
      buf.write_string("  }\n")
    }
    _ =>
      // :exec
      buf.write_string("  stmt.run() |> ignore\n")
  }
  buf.write_string("}\n\n")
}

///|
fn generate_validators(request : @plugin.GenerateRequest) -> String {
  let buf = StringBuilder::new()
  buf.write_string("// Generated by sqlc-gen-moonbit\n")
  buf.write_string("// Validation functions for Params structs\n\n")

  // Generate validators for each Params struct
  for query in request.queries {
    if query.params.length() > 0 {
      let struct_name = to_pascal_case(query.name) + "Params"
      generate_validator_function(buf, struct_name, query.params)
    }
  }
  buf.to_string()
}

///|
fn has_string_validations(params : Array[@plugin.Parameter]) -> Bool {
  for param in params {
    let col = param.column
    let type_name = col.type_.name.to_upper()
    match type_name {
      "TEXT" | "VARCHAR" | "CHAR" | "CHARACTER" | "CLOB" =>
        if col.not_null || col.length > 0 {
          return true
        }
      _ => ()
    }
  }
  false
}

///|
fn generate_validator_function(
  buf : StringBuilder,
  struct_name : String,
  params : Array[@plugin.Parameter],
) -> Unit {
  let has_validations = has_string_validations(params)
  buf.write_string("///| Validate ")
  buf.write_string(struct_name)
  buf.write_string("\n")
  buf.write_string("pub fn ")
  buf.write_string(struct_name)
  if has_validations {
    buf.write_string("::validate(self : ")
  } else {
    buf.write_string("::validate(_self : ")
  }
  buf.write_string(struct_name)
  buf.write_string(") -> Result[Unit, String] {\n")
  for param in params {
    let col = param.column
    let param_name = if col.name.length() > 0 {
      to_snake_case(col.name)
    } else {
      "p" + param.number.to_string()
    }
    let type_name = col.type_.name.to_upper()

    // Generate validation based on constraints
    if col.not_null {
      // For NOT NULL String types, check for empty string
      match type_name {
        "TEXT" | "VARCHAR" | "CHAR" | "CHARACTER" | "CLOB" => {
          buf.write_string("  if self.")
          buf.write_string(param_name)
          buf.write_string(".length() == 0 {\n")
          buf.write_string("    return Err(\"")
          buf.write_string(param_name)
          buf.write_string(" is required\")\n")
          buf.write_string("  }\n")
        }
        _ => ()
      }
    }

    // Length validation for VARCHAR(n)
    if col.length > 0 {
      match type_name {
        "TEXT" | "VARCHAR" | "CHAR" | "CHARACTER" | "CLOB" => {
          let field_expr = if col.not_null {
            "self." + param_name
          } else {
            // For nullable, wrap in match
            ""
          }
          if col.not_null {
            buf.write_string("  if ")
            buf.write_string(field_expr)
            buf.write_string(".length() > ")
            buf.write_string(col.length.to_string())
            buf.write_string(" {\n")
            buf.write_string("    return Err(\"")
            buf.write_string(param_name)
            buf.write_string(" must be at most ")
            buf.write_string(col.length.to_string())
            buf.write_string(" characters\")\n")
            buf.write_string("  }\n")
          } else {
            buf.write_string("  match self.")
            buf.write_string(param_name)
            buf.write_string(" {\n")
            buf.write_string("    Some(v) => if v.length() > ")
            buf.write_string(col.length.to_string())
            buf.write_string(" {\n")
            buf.write_string("      return Err(\"")
            buf.write_string(param_name)
            buf.write_string(" must be at most ")
            buf.write_string(col.length.to_string())
            buf.write_string(" characters\")\n")
            buf.write_string("    }\n")
            buf.write_string("    None => ()\n")
            buf.write_string("  }\n")
          }
        }
        _ => ()
      }
    }

    // Unsigned validation for numeric types
    if col.unsigned {
      match type_name {
        "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" | "SMALLINT" =>
          if col.not_null {
            buf.write_string("  if self.")
            buf.write_string(param_name)
            buf.write_string(" < 0L {\n")
            buf.write_string("    return Err(\"")
            buf.write_string(param_name)
            buf.write_string(" must be non-negative\")\n")
            buf.write_string("  }\n")
          } else {
            buf.write_string("  match self.")
            buf.write_string(param_name)
            buf.write_string(" {\n")
            buf.write_string("    Some(v) => if v < 0L {\n")
            buf.write_string("      return Err(\"")
            buf.write_string(param_name)
            buf.write_string(" must be non-negative\")\n")
            buf.write_string("    }\n")
            buf.write_string("    None => ()\n")
            buf.write_string("  }\n")
          }
        _ => ()
      }
    }
  }
  buf.write_string("  Ok(())\n")
  buf.write_string("}\n\n")
}

///|
fn generate_json_schema(request : @plugin.GenerateRequest) -> String {
  let buf = StringBuilder::new()
  buf.write_string("{\n")
  buf.write_string(
    "  \"$schema\": \"http://json-schema.org/draft-07/schema#\",\n",
  )
  buf.write_string("  \"definitions\": {\n")
  let mut first_def = true

  // Generate schema for each Params struct
  for query in request.queries {
    if query.params.length() > 0 {
      if not(first_def) {
        buf.write_string(",\n")
      }
      first_def = false
      let struct_name = to_pascal_case(query.name) + "Params"
      generate_json_schema_object(buf, struct_name, query.params, "    ")
    }
  }

  // Generate schema for each Row struct
  for query in request.queries {
    if query.cmd == ":one" || query.cmd == ":many" {
      if query.columns.length() > 0 {
        if not(first_def) {
          buf.write_string(",\n")
        }
        first_def = false
        let struct_name = to_pascal_case(query.name) + "Row"
        generate_json_schema_row(buf, struct_name, query.columns, "    ")
      }
    }
  }
  buf.write_string("\n  }\n")
  buf.write_string("}\n")
  buf.to_string()
}

///|
fn generate_json_schema_object(
  buf : StringBuilder,
  name : String,
  params : Array[@plugin.Parameter],
  indent : String,
) -> Unit {
  buf.write_string(indent)
  buf.write_string("\"")
  buf.write_string(name)
  buf.write_string("\": {\n")
  buf.write_string(indent)
  buf.write_string("  \"type\": \"object\",\n")

  // Required fields
  let required : Array[String] = []
  for param in params {
    if param.column.not_null {
      let param_name = if param.column.name.length() > 0 {
        to_snake_case(param.column.name)
      } else {
        "p" + param.number.to_string()
      }
      required.push(param_name)
    }
  }
  if required.length() > 0 {
    buf.write_string(indent)
    buf.write_string("  \"required\": [")
    for i, r in required {
      if i > 0 {
        buf.write_string(", ")
      }
      buf.write_string("\"")
      buf.write_string(r)
      buf.write_string("\"")
    }
    buf.write_string("],\n")
  }

  // Properties
  buf.write_string(indent)
  buf.write_string("  \"properties\": {\n")
  for i, param in params {
    if i > 0 {
      buf.write_string(",\n")
    }
    let col = param.column
    let param_name = if col.name.length() > 0 {
      to_snake_case(col.name)
    } else {
      "p" + param.number.to_string()
    }
    generate_json_schema_property(buf, param_name, col, indent + "    ")
  }
  buf.write_string("\n")
  buf.write_string(indent)
  buf.write_string("  }\n")
  buf.write_string(indent)
  buf.write_string("}")
}

///|
fn generate_json_schema_row(
  buf : StringBuilder,
  name : String,
  columns : Array[@plugin.Column],
  indent : String,
) -> Unit {
  buf.write_string(indent)
  buf.write_string("\"")
  buf.write_string(name)
  buf.write_string("\": {\n")
  buf.write_string(indent)
  buf.write_string("  \"type\": \"object\",\n")

  // Required fields
  let required : Array[String] = []
  for col in columns {
    if col.not_null {
      required.push(to_snake_case(col.name))
    }
  }
  if required.length() > 0 {
    buf.write_string(indent)
    buf.write_string("  \"required\": [")
    for i, r in required {
      if i > 0 {
        buf.write_string(", ")
      }
      buf.write_string("\"")
      buf.write_string(r)
      buf.write_string("\"")
    }
    buf.write_string("],\n")
  }

  // Properties
  buf.write_string(indent)
  buf.write_string("  \"properties\": {\n")
  for i, col in columns {
    if i > 0 {
      buf.write_string(",\n")
    }
    generate_json_schema_property(
      buf,
      to_snake_case(col.name),
      col,
      indent + "    ",
    )
  }
  buf.write_string("\n")
  buf.write_string(indent)
  buf.write_string("  }\n")
  buf.write_string(indent)
  buf.write_string("}")
}

///|
fn generate_json_schema_property(
  buf : StringBuilder,
  name : String,
  col : @plugin.Column,
  indent : String,
) -> Unit {
  buf.write_string(indent)
  buf.write_string("\"")
  buf.write_string(name)
  buf.write_string("\": {")
  let type_name = col.type_.name.to_upper()
  let json_type = match type_name {
    "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" | "SMALLINT" => "integer"
    "REAL" | "FLOAT" | "DOUBLE" | "NUMERIC" | "DECIMAL" => "number"
    "BOOLEAN" | "BOOL" => "boolean"
    _ => "string"
  }

  // Handle nullable types
  if col.not_null {
    buf.write_string(" \"type\": \"")
    buf.write_string(json_type)
    buf.write_string("\"")
  } else {
    buf.write_string(" \"type\": [\"")
    buf.write_string(json_type)
    buf.write_string("\", \"null\"]")
  }

  // Add constraints
  if col.length > 0 && json_type == "string" {
    buf.write_string(", \"maxLength\": ")
    buf.write_string(col.length.to_string())
  }
  if col.not_null && json_type == "string" {
    buf.write_string(", \"minLength\": 1")
  }
  if col.unsigned && (json_type == "integer" || json_type == "number") {
    buf.write_string(", \"minimum\": 0")
  }
  buf.write_string(" }")
}

///|
fn param_to_core_any(col : @plugin.Column, expr : String) -> String {
  let type_name = col.type_.name.to_upper()
  // For nullable types, we need to handle Option
  let is_nullable = not(col.not_null)
  if is_nullable {
    // For nullable params, use match to handle None - @core.null() for null value
    match type_name {
      "INTEGER" | "INT" | "INT4" | "SMALLINT" =>
        "(match " +
        expr +
        " { Some(v) => @core.any(v.to_int()); None => @core.null() })"
      _ =>
        "(match " + expr + " { Some(v) => @core.any(v); None => @core.null() })"
    }
  } else {
    match type_name {
      "INTEGER" | "INT" | "INT4" | "SMALLINT" =>
        "@core.any(" + expr + ".to_int())"
      "INT8" | "BIGINT" => "@core.any(" + expr + ")"
      "REAL" | "FLOAT" | "DOUBLE" | "NUMERIC" | "DECIMAL" =>
        "@core.any(" + expr + ")"
      "TEXT" | "VARCHAR" | "CHAR" | "CHARACTER" | "CLOB" =>
        "@core.any(" + expr + ")"
      "BOOLEAN" | "BOOL" => "@core.any(" + expr + ")"
      _ => "@core.any(" + expr + ".to_string())"
    }
  }
}

///|
fn column_from_core_any(col : @plugin.Column, obj_expr : String) -> String {
  let type_name = col.type_.name.to_upper()
  let field_name = "\"" + col.name + "\""
  let field_access = obj_expr + "[" + field_name + "]"

  // Handle nullable columns
  if not(col.not_null) {
    // For nullable columns, check for null/undefined first
    match type_name {
      "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" | "SMALLINT" =>
        "(if @core.is_null(" +
        field_access +
        ") || @core.typeof_(" +
        field_access +
        ") == \"undefined\" { None } else { Some(@cloudflare.js_number_to_int64(" +
        field_access +
        ")) })"
      _ =>
        "(if @core.is_null(" +
        field_access +
        ") || @core.typeof_(" +
        field_access +
        ") == \"undefined\" { None } else { Some(" +
        field_access +
        ".cast()) })"
    }
  } else {
    // Non-nullable columns
    match type_name {
      // Use @cloudflare.js_number_to_int64 for INTEGER types
      // D1/SQLite returns JavaScript numbers which need conversion to MoonBit Int64 ({hi, lo} format)
      "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" | "SMALLINT" =>
        "@cloudflare.js_number_to_int64(" + field_access + ")"
      "REAL" | "FLOAT" | "DOUBLE" | "NUMERIC" | "DECIMAL" =>
        field_access + ".cast()"
      "TEXT" | "VARCHAR" | "CHAR" | "CHARACTER" | "CLOB" =>
        field_access + ".cast()"
      "BOOLEAN" | "BOOL" => field_access + ".cast()"
      _ => field_access + ".cast()"
    }
  }
}

///|
fn param_to_bind_call(col : @plugin.Column, idx : Int, expr : String) -> String {
  let type_name = col.type_.name.to_upper()
  let idx_str = idx.to_string()
  let is_nullable = not(col.not_null)

  // Get the base bind call expression
  let base_bind = fn(val_expr : String) -> String {
    match type_name {
      "INTEGER" | "INT" | "INT4" | "SMALLINT" =>
        "@sqlite.sqlite_bind_int(stmt, " +
        idx_str +
        ", " +
        val_expr +
        ".to_int())"
      "INT8" | "BIGINT" =>
        "@sqlite.sqlite_bind_int64(stmt, " + idx_str + ", " + val_expr + ")"
      "REAL" | "FLOAT" | "DOUBLE" | "NUMERIC" | "DECIMAL" =>
        "@sqlite.sqlite_bind_double(stmt, " + idx_str + ", " + val_expr + ")"
      "TEXT" | "VARCHAR" | "CHAR" | "CHARACTER" | "CLOB" =>
        "@sqlite.sqlite_bind_text(stmt, " +
        idx_str +
        ", cstring(" +
        val_expr +
        "))"
      "BLOB" | "BYTEA" =>
        "@sqlite.sqlite_bind_blob(stmt, " + idx_str + ", " + val_expr + ")"
      "BOOLEAN" | "BOOL" =>
        "@sqlite.sqlite_bind_int(stmt, " +
        idx_str +
        ", if " +
        val_expr +
        " { 1 } else { 0 })"
      _ =>
        "@sqlite.sqlite_bind_text(stmt, " +
        idx_str +
        ", cstring(" +
        val_expr +
        "))"
    }
  }

  // Handle nullable parameters with match
  if is_nullable {
    "(match " +
    expr +
    " { None => @sqlite.sqlite_bind_null(stmt, " +
    idx_str +
    "); Some(v) => " +
    base_bind("v") +
    " })"
  } else {
    base_bind(expr)
  }
}

///|
fn column_to_moonbit(col : @plugin.Column, idx : Int) -> String {
  let type_name = col.type_.name.to_upper()
  let idx_str = idx.to_string()
  let is_nullable = not(col.not_null)

  // Get the base value expression
  let base_expr = match type_name {
    "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" | "SMALLINT" =>
      "@sqlite.sqlite_column_int64(stmt, " + idx_str + ")"
    "REAL" | "FLOAT" | "DOUBLE" | "NUMERIC" | "DECIMAL" =>
      "@sqlite.sqlite_column_double(stmt, " + idx_str + ")"
    "TEXT" | "VARCHAR" | "CHAR" | "CHARACTER" | "CLOB" =>
      "bytes_to_string(@sqlite.sqlite_column_text(stmt, " + idx_str + "))"
    "BLOB" | "BYTEA" => "@sqlite.sqlite_column_blob(stmt, " + idx_str + ")"
    "BOOLEAN" | "BOOL" =>
      "@sqlite.sqlite_column_int(stmt, " + idx_str + ") != 0"
    _ => "bytes_to_string(@sqlite.sqlite_column_text(stmt, " + idx_str + "))"
  }

  // Wrap in Option if nullable
  if is_nullable {
    "(if @sqlite.sqlite_column_type(stmt, " +
    idx_str +
    ") == @sqlite.SQLITE_NULL { None } else { Some(" +
    base_expr +
    ") })"
  } else {
    base_expr
  }
}

///|
fn find_column_override(
  col : @plugin.Column,
  overrides : Array[TypeOverride],
) -> TypeOverride? {
  let table_name = col.table.name
  let col_name = col.name
  let full_col_name = table_name + "." + col_name
  let type_name = col.type_.name.to_upper()

  // First, check for column-specific override
  for override_ in overrides {
    match override_.column {
      Some(c) if c == full_col_name => return Some(override_)
      _ => ()
    }
  }
  // Then, check for db_type override
  for override_ in overrides {
    match override_.db_type {
      Some(dt) if dt.to_upper() == type_name => return Some(override_)
      _ => ()
    }
  }
  None
}

///|
fn sql_type_to_moonbit(
  col : @plugin.Column,
  overrides : Array[TypeOverride],
) -> String {
  // Check for override first
  match find_column_override(col, overrides) {
    Some(override_) => {
      let base_type = override_.moonbit_type
      // Determine nullability: explicit override > column definition
      let is_nullable = match override_.nullable {
        Some(n) => n
        None => not(col.not_null)
      }
      if is_nullable {
        base_type + "?"
      } else {
        base_type
      }
    }
    None => {
      // Default type mapping
      let type_name = col.type_.name.to_upper()
      let base_type = match type_name {
        // Integer types
        "INTEGER" | "INT" | "INT4" | "INT2" | "SMALLINT" => "Int"
        "INT8" | "BIGINT" | "SERIAL" | "BIGSERIAL" => "Int64"
        // Floating point types
        "REAL" | "FLOAT4" => "Double"
        "DOUBLE PRECISION" | "FLOAT8" | "FLOAT" | "NUMERIC" | "DECIMAL" =>
          "Double"
        // String types
        "TEXT"
        | "VARCHAR"
        | "CHAR"
        | "CHARACTER"
        | "CHARACTER VARYING"
        | "BPCHAR"
        | "CLOB" => "String"
        // Binary types
        "BLOB" | "BYTEA" => "String"
        // Boolean types
        "BOOLEAN" | "BOOL" => "Bool"
        // Date/Time types (represented as String)
        "TIMESTAMP"
        | "TIMESTAMPTZ"
        | "TIMESTAMP WITHOUT TIME ZONE"
        | "TIMESTAMP WITH TIME ZONE" => "String"
        "DATE" => "String"
        "TIME" | "TIMETZ" | "TIME WITHOUT TIME ZONE" | "TIME WITH TIME ZONE" =>
          "String"
        // UUID (represented as String)
        "UUID" => "String"
        // JSON types (represented as String)
        "JSONB" | "JSON" => "String"
        // Network types (represented as String)
        "INET" | "CIDR" | "MACADDR" | "MACADDR8" => "String"
        // Geometric types (represented as String)
        "POINT" | "LINE" | "LSEG" | "BOX" | "PATH" | "POLYGON" | "CIRCLE" =>
          "String"
        // Range types (represented as String)
        "INT4RANGE"
        | "INT8RANGE"
        | "NUMRANGE"
        | "TSRANGE"
        | "TSTZRANGE"
        | "DATERANGE" => "String"
        _ => "String" // Default to String for unknown types
      }
      if col.not_null {
        if col.is_array {
          "Array[" + base_type + "]"
        } else {
          base_type
        }
      } else if col.is_array {
        "Array[" + base_type + "]?"
      } else {
        base_type + "?"
      }
    }
  }
}

///|
fn to_pascal_case(s : String) -> String {
  let buf = StringBuilder::new()
  let mut capitalize_next = true
  for c in s {
    if c == '_' || c == '-' {
      capitalize_next = true
    } else if capitalize_next {
      buf.write_char(to_upper_char(c))
      capitalize_next = false
    } else {
      buf.write_char(c)
    }
  }
  buf.to_string()
}

///|
fn to_snake_case(s : String) -> String {
  let buf = StringBuilder::new()
  let mut prev_lower = false
  for c in s {
    if is_upper(c) {
      if prev_lower {
        buf.write_char('_')
      }
      buf.write_char(to_lower_char(c))
      prev_lower = false
    } else {
      buf.write_char(c)
      prev_lower = is_lower(c)
    }
  }
  buf.to_string()
}

///|
fn to_upper_char(c : Char) -> Char {
  if c >= 'a' && c <= 'z' {
    (c.to_int() - 32).unsafe_to_char()
  } else {
    c
  }
}

///|
fn to_lower_char(c : Char) -> Char {
  if c >= 'A' && c <= 'Z' {
    (c.to_int() + 32).unsafe_to_char()
  } else {
    c
  }
}

///|
fn is_upper(c : Char) -> Bool {
  c >= 'A' && c <= 'Z'
}

///|
fn is_lower(c : Char) -> Bool {
  c >= 'a' && c <= 'z'
}

///|
fn sanitize_newlines(s : String) -> String {
  let buf = StringBuilder::new()
  for c in s {
    match c {
      '\n' | '\r' => buf.write_char(' ')
      _ => buf.write_char(c)
    }
  }
  buf.to_string()
}

///|
fn escape_string(s : String) -> String {
  let buf = StringBuilder::new()
  buf.write_char('"')
  for c in s {
    match c {
      '"' => buf.write_string("\\\"")
      '\\' => buf.write_string("\\\\")
      '\n' => buf.write_string("\\n")
      '\r' => buf.write_string("\\r")
      '\t' => buf.write_string("\\t")
      _ => buf.write_char(c)
    }
  }
  buf.write_char('"')
  buf.to_string()
}

///|
fn serialize_response(response : @plugin.GenerateResponse) -> Bytes {
  // Use Array to accumulate bytes dynamically (no fixed size)
  let bytes : Array[Byte] = []

  // Write each file
  for file in response.files {
    // Tag for files field (field 1, wire type 2 = length-delimited)
    bytes.push(b'\x0a')

    // Calculate file message size
    let file_size = @protobuf.size_of(file)

    // Write file size as varint
    let mut sz = file_size
    while sz >= 0x80U {
      bytes.push(((sz & 0x7FU) | 0x80U).to_byte())
      sz = sz >> 7
    }
    bytes.push(sz.to_byte())

    // Write name field (field 1)
    bytes.push(b'\x0a')
    let name_bytes = string_to_utf8(file.name)
    let name_len = name_bytes.length()
    let mut nl = name_len.reinterpret_as_uint()
    while nl >= 0x80U {
      bytes.push(((nl & 0x7FU) | 0x80U).to_byte())
      nl = nl >> 7
    }
    bytes.push(nl.to_byte())
    for i in 0..<name_len {
      bytes.push(name_bytes[i])
    }

    // Write contents field (field 2)
    bytes.push(b'\x12')
    let contents = file.contents
    let contents_len = contents.length()
    let mut cl = contents_len.reinterpret_as_uint()
    while cl >= 0x80U {
      bytes.push(((cl & 0x7FU) | 0x80U).to_byte())
      cl = cl >> 7
    }
    bytes.push(cl.to_byte())
    for i in 0..<contents_len {
      bytes.push(contents[i])
    }
  }
  Bytes::from_array(bytes)
}

///|
pub fn string_to_utf8(s : String) -> Bytes {
  let arr : Array[Byte] = []
  for c in s {
    let code = c.to_uint()
    if code < 0x80U {
      arr.push(code.to_byte())
    } else if code < 0x800U {
      arr.push((0xC0U | (code >> 6)).to_byte())
      arr.push((0x80U | (code & 0x3FU)).to_byte())
    } else if code < 0x10000U {
      arr.push((0xE0U | (code >> 12)).to_byte())
      arr.push((0x80U | ((code >> 6) & 0x3FU)).to_byte())
      arr.push((0x80U | (code & 0x3FU)).to_byte())
    } else {
      arr.push((0xF0U | (code >> 18)).to_byte())
      arr.push((0x80U | ((code >> 12) & 0x3FU)).to_byte())
      arr.push((0x80U | ((code >> 6) & 0x3FU)).to_byte())
      arr.push((0x80U | (code & 0x3FU)).to_byte())
    }
  }
  Bytes::from_iter(arr.iter())
}
