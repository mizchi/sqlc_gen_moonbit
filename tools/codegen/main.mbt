///|
/// Standalone SQL to MoonBit code generator
/// Usage: moon run tools/codegen --target native -- [options] <input.sql>
///
/// Options:
///   -o, --output <file>   Output file (default: stdout)
///   -c, --config <file>   Config file (JSON)
///   -b, --backend <type>  Backend: sqlite, sqlite_js, d1, postgres, postgres_js, mysql_js
///   --validators          Generate validators
///   --json-schema         Generate JSON schema
///   --check-deps          Check dependencies in moon.mod.json
///   -h, --help            Show help

// Config structure matching sqlc options
struct Config {
  mut backend : String
  mut validators : Bool
  mut json_schema : Bool
  overrides : Array[Override]
}

///|
fn Config::default() -> Config {
  { backend: "sqlite", validators: false, json_schema: false, overrides: [] }
}

struct Override {
  column : String?
  db_type : String?
  moonbit_type : String
  nullable : Bool?
}

// Query definition parsed from comments
struct QueryDef {
  name : String
  cmd : String // :one, :many, :exec
  params : Array[(String, String)] // (name, type)
  returns : Array[(String, String)] // (name, type)
  sql : String
}

// CLI arguments
struct CliArgs {
  mut input_file : String?
  mut output_file : String?
  mut config_file : String?
  mut backend : String?
  mut validators : Bool
  mut json_schema : Bool
  mut check_deps : Bool
  mut show_help : Bool
}

///|
fn CliArgs::default() -> CliArgs {
  {
    input_file: None,
    output_file: None,
    config_file: None,
    backend: None,
    validators: false,
    json_schema: false,
    check_deps: false,
    show_help: false,
  }
}

///|
fn main {
  let raw_args = @sys.get_cli_args()
  let args = parse_cli_args(raw_args)
  if args.show_help {
    print_help()
    return
  }
  match args.input_file {
    None => {
      eprintln("Error: No input file specified")
      eprintln("Use --help for usage information")
      return
    }
    Some(input_file) => {
      // Load config
      let config = match args.config_file {
        Some(config_path) =>
          match load_config(config_path) {
            Ok(c) => c
            Err(e) => {
              eprintln("Error loading config: " + e)
              return
            }
          }
        None => Config::default()
      }

      // Override config with CLI args
      match args.backend {
        Some(b) => config.backend = b
        None => ()
      }
      if args.validators {
        config.validators = true
      }
      if args.json_schema {
        config.json_schema = true
      }

      // Check dependencies
      if args.check_deps {
        check_dependencies(config.backend)
      }

      // Read input file
      let content = @fs.read_file_to_string(input_file) catch {
        e => {
          eprintln("Error reading file: " + e.to_string())
          return
        }
      }

      // Parse and generate
      let queries = parse_queries(content)
      let code = generate_code(queries, config)

      // Output
      match args.output_file {
        Some(output_path) => {
          @fs.write_string_to_file(output_path, code) catch {
            e => {
              eprintln("Error writing file: " + e.to_string())
              return
            }
          }
          eprintln("Generated: " + output_path)
        }
        None => println(code)
      }
    }
  }
}

///|
fn print_help() -> Unit {
  println("sqlc-gen-moonbit standalone code generator")
  println("")
  println("Usage: moon run tools/codegen --target native -- [options] <input.sql>")
  println("")
  println("Options:")
  println("  -o, --output <file>   Output file (default: stdout)")
  println("  -c, --config <file>   Config file (JSON)")
  println("  -b, --backend <type>  Backend type (default: sqlite)")
  println("                        sqlite, sqlite_js, d1, postgres, postgres_js, mysql_js")
  println("  --validators          Generate validation functions")
  println("  --json-schema         Generate JSON schema file")
  println("  --check-deps          Check required dependencies")
  println("  -h, --help            Show this help")
  println("")
  println("Config file format (JSON):")
  println("  {")
  println("    \"backend\": \"sqlite\",")
  println("    \"validators\": true,")
  println("    \"json_schema\": false,")
  println("    \"overrides\": [")
  println("      { \"column\": \"users.id\", \"moonbit_type\": \"UserId\" },")
  println("      { \"db_type\": \"uuid\", \"moonbit_type\": \"@uuid.UUID\" }")
  println("    ]")
  println("  }")
  println("")
  println("SQL file format:")
  println("  -- @query GetUser :one")
  println("  -- @param id INTEGER")
  println("  -- @returns id INTEGER, name TEXT, email TEXT")
  println("  SELECT * FROM users WHERE id = ?;")
}

///|
fn parse_cli_args(args : Array[String]) -> CliArgs {
  let result = CliArgs::default()
  let mut i = 1 // Skip program name
  while i < args.length() {
    let arg = args[i]
    if arg == "-h" || arg == "--help" {
      result.show_help = true
    } else if arg == "-o" || arg == "--output" {
      i = i + 1
      if i < args.length() {
        result.output_file = Some(args[i])
      }
    } else if arg == "-c" || arg == "--config" {
      i = i + 1
      if i < args.length() {
        result.config_file = Some(args[i])
      }
    } else if arg == "-b" || arg == "--backend" {
      i = i + 1
      if i < args.length() {
        result.backend = Some(args[i])
      }
    } else if arg == "--validators" {
      result.validators = true
    } else if arg == "--json-schema" {
      result.json_schema = true
    } else if arg == "--check-deps" {
      result.check_deps = true
    } else if not(arg.has_prefix("-")) {
      result.input_file = Some(arg)
    }
    i = i + 1
  }
  result
}

///|
fn load_config(path : String) -> Result[Config, String] {
  let content = @fs.read_file_to_string(path) catch {
    e => return Err("Failed to read config: " + e.to_string())
  }
  parse_config_json(content)
}

///|
fn parse_config_json(content : String) -> Result[Config, String] {
  let json : Json = @json.parse(content) catch {
    e => return Err("Invalid JSON: " + e.to_string())
  }
  let config = Config::default()
  match json {
    Object(obj) => {
      match obj.get("backend") {
        Some(String(s)) => config.backend = s
        _ => ()
      }
      match obj.get("validators") {
        Some(True) => config.validators = true
        Some(False) => config.validators = false
        _ => ()
      }
      match obj.get("json_schema") {
        Some(True) => config.json_schema = true
        Some(False) => config.json_schema = false
        _ => ()
      }
      match obj.get("overrides") {
        Some(Array(arr)) => {
          for item in arr {
            match parse_override(item) {
              Ok(o) => config.overrides.push(o)
              Err(_) => ()
            }
          }
        }
        _ => ()
      }
    }
    _ => return Err("Config must be a JSON object")
  }
  Ok(config)
}

///|
fn parse_override(json : Json) -> Result[Override, String] {
  match json {
    Object(obj) => {
      let column : String? = match obj.get("column") {
        Some(String(s)) => Some(s)
        _ => None
      }
      let db_type : String? = match obj.get("db_type") {
        Some(String(s)) => Some(s)
        _ => None
      }
      let moonbit_type = match obj.get("moonbit_type") {
        Some(String(s)) => s
        _ => return Err("moonbit_type is required")
      }
      let nullable : Bool? = match obj.get("nullable") {
        Some(True) => Some(true)
        Some(False) => Some(false)
        _ => None
      }
      Ok({ column, db_type, moonbit_type, nullable })
    }
    _ => Err("Override must be an object")
  }
}

///|
fn check_dependencies(backend : String) -> Unit {
  // Read moon.mod.json if it exists
  let content = @fs.read_file_to_string("moon.mod.json") catch {
    _ => {
      eprintln("Warning: moon.mod.json not found")
      print_required_deps(backend)
      return
    }
  }
  let json : Json = @json.parse(content) catch {
    _ => {
      eprintln("Warning: Failed to parse moon.mod.json")
      print_required_deps(backend)
      return
    }
  }
  let deps : Map[String, String] = {}
  match json {
    Object(obj) =>
      match obj.get("deps") {
        Some(Object(deps_obj)) => {
          for entry in deps_obj {
            match entry.1 {
              String(v) => deps[entry.0] = v
              _ => ()
            }
          }
        }
        _ => ()
      }
    _ => ()
  }
  let required = get_required_deps(backend)
  let missing : Array[String] = []
  for dep in required {
    if not(deps.contains(dep)) {
      missing.push(dep)
    }
  }
  if missing.length() > 0 {
    eprintln("Warning: Missing dependencies for backend '" + backend + "':")
    for dep in missing {
      eprintln("  - " + dep)
    }
    eprintln("")
    eprintln("Add them with:")
    for dep in missing {
      eprintln("  moon add " + dep)
    }
  } else {
    eprintln("All dependencies satisfied for backend '" + backend + "'")
  }
}

///|
fn get_required_deps(backend : String) -> Array[String] {
  match backend {
    "sqlite" => ["mizchi/sqlite", "moonbitlang/x"]
    "sqlite_js" => ["mizchi/sqlite", "moonbitlang/x"]
    "d1" => ["mizchi/cloudflare", "mizchi/js"]
    "postgres" => ["mattn/postgres"]
    "postgres_js" => ["mizchi/npm_typed", "mizchi/js"]
    "mysql_js" => ["mizchi/js"]
    _ => []
  }
}

///|
fn print_required_deps(backend : String) -> Unit {
  let deps = get_required_deps(backend)
  if deps.length() > 0 {
    eprintln("Required dependencies for backend '" + backend + "':")
    for dep in deps {
      eprintln("  - " + dep)
    }
  }
}

///|
fn eprintln(s : String) -> Unit {
  // Write to stderr - for now just use println with prefix
  println("[stderr] " + s)
}

///|
fn parse_queries(content : String) -> Array[QueryDef] {
  let queries : Array[QueryDef] = []
  let lines = split_lines(content)
  let mut i = 0
  while i < lines.length() {
    let line = lines[i].trim().to_string()
    if line.has_prefix("-- @query ") {
      let parts = split_whitespace(substr(line, 10))
      if parts.length() >= 2 {
        let name = parts[0]
        let cmd = parts[1]
        let params : Array[(String, String)] = []
        let returns : Array[(String, String)] = []
        let sql_lines : Array[String] = []
        i = i + 1
        while i < lines.length() {
          let next_line = lines[i].trim().to_string()
          if next_line.has_prefix("-- @param ") {
            let param_str = substr(next_line, 10)
            let param_parts = split_whitespace(param_str)
            if param_parts.length() >= 2 {
              params.push((param_parts[0], param_parts[1]))
            }
          } else if next_line.has_prefix("-- @returns ") {
            let ret_str = substr(next_line, 12)
            let cols = split_by_comma(ret_str)
            for col in cols {
              let col_parts = split_whitespace(col.trim().to_string())
              if col_parts.length() >= 2 {
                returns.push((col_parts[0], col_parts[1]))
              }
            }
          } else if next_line.has_prefix("-- @query ") {
            break
          } else if next_line.length() > 0 && not(next_line.has_prefix("--")) {
            sql_lines.push(next_line)
            if next_line.has_suffix(";") {
              i = i + 1
              break
            }
          }
          i = i + 1
        }
        let sql = join_lines(sql_lines, " ")
        queries.push({ name, cmd, params, returns, sql })
        continue
      }
    }
    i = i + 1
  }
  queries
}

///|
fn generate_code(queries : Array[QueryDef], config : Config) -> String {
  let buf = StringBuilder::new()
  buf.write_string("// Generated by sqlc-gen-moonbit (standalone)\n")
  buf.write_string("// Backend: ")
  buf.write_string(config.backend)
  buf.write_string("\n\n")

  // Generate backend-specific helpers and imports comment
  generate_helpers(buf, config.backend)

  let emitter = @emit.MoonBitEmitter::new()
  let validator_emitter = @emit.ValidatorEmitter::new()

  // Generate structs using codegen.mbt
  for query in queries {
    // Generate Row struct for :one and :many
    if (query.cmd == ":one" || query.cmd == ":many") && query.returns.length() > 0 {
      let row_struct = query_to_row_struct(query, config.overrides)
      buf.write_string(emitter.emit_struct(row_struct))
      buf.write_string("\n")
    }

    // Generate Params struct
    if query.params.length() > 0 {
      let params_struct = query_to_params_struct(query, config.overrides)
      buf.write_string(emitter.emit_struct(params_struct))
      buf.write_string("\n")
      // Constructor
      generate_constructor(buf, params_struct)
      // Validator
      if config.validators && has_validatable_fields(params_struct) {
        buf.write_string(validator_emitter.emit_struct_validator(params_struct))
        buf.write_string("\n")
      }
    }
  }

  // Generate SQL constants and functions
  for query in queries {
    generate_query_function(buf, query, config.backend)
  }
  buf.to_string()
}

///|
fn generate_helpers(buf : StringBuilder, backend : String) -> Unit {
  match backend {
    "sqlite" | "sqlite_js" => {
      buf.write_string("// Required: import { \"mizchi/sqlite\", \"moonbitlang/x/encoding\" }\n\n")
      buf.write_string("///|\nfn cstring(s : String) -> Bytes {\n")
      buf.write_string("  @encoding.encode(@encoding.UTF8, s)\n")
      buf.write_string("}\n\n")
      buf.write_string("///|\nfn bytes_to_string(b : Bytes) -> String {\n")
      buf.write_string("  let decoder = @encoding.decoder(@encoding.UTF8)\n")
      buf.write_string("  decoder.decode_lossy(b[0:b.length()])\n")
      buf.write_string("}\n\n")
    }
    "d1" =>
      buf.write_string(
        "// Required: import { \"mizchi/cloudflare\", \"mizchi/js/core\" }\n\n",
      )
    "postgres" =>
      buf.write_string("// Required: import { \"mattn/postgres\" }\n\n")
    "postgres_js" =>
      buf.write_string(
        "// Required: import { \"mizchi/npm_typed/pg\", \"mizchi/js/core\" }\n\n",
      )
    "mysql_js" => buf.write_string("// Required: import { \"mizchi/js/core\" }\n\n")
    _ => ()
  }
}

///|
fn query_to_row_struct(query : QueryDef, overrides : Array[Override]) -> @ir.Struct {
  let struct_name = query.name + "Row"
  let mut s = @ir.Struct::new(struct_name).with_derives(["Show", "Eq"])
  for ret in query.returns {
    let ty = sql_type_to_ir_with_override(ret.0, ret.1, overrides)
    let field = @ir.Field::new(to_snake_case(ret.0), ty)
    s = s.with_field(field)
  }
  s
}

///|
fn query_to_params_struct(
  query : QueryDef,
  overrides : Array[Override],
) -> @ir.Struct {
  let struct_name = query.name + "Params"
  let mut s = @ir.Struct::new(struct_name).with_derives(["Show", "Eq"])
  for param in query.params {
    let ty = sql_type_to_ir_with_override(param.0, param.1, overrides)
    let mut field = @ir.Field::new(to_snake_case(param.0), ty)
    // Add constraints for string types
    if is_string_sql_type(param.1) {
      field = field.as_required()
    }
    s = s.with_field(field)
  }
  s
}

///|
fn sql_type_to_ir_with_override(
  col_name : String,
  sql_type : String,
  overrides : Array[Override],
) -> @ir.Type {
  // Check for override
  for override_ in overrides {
    match override_.column {
      Some(c) if c == col_name => return @ir.Type::named(override_.moonbit_type)
      _ => ()
    }
    match override_.db_type {
      Some(dt) if dt.to_upper() == sql_type.to_upper() =>
        return @ir.Type::named(override_.moonbit_type)
      _ => ()
    }
  }
  sql_type_to_ir(sql_type)
}

///|
fn sql_type_to_ir(sql_type : String) -> @ir.Type {
  match sql_type.to_upper() {
    "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" | "SMALLINT" =>
      @ir.Type::int64()
    "REAL" | "FLOAT" | "DOUBLE" | "NUMERIC" | "DECIMAL" => @ir.Type::double()
    "TEXT" | "VARCHAR" | "CHAR" => @ir.Type::string()
    "BLOB" | "BYTEA" => @ir.Type::bytes()
    "BOOLEAN" | "BOOL" => @ir.Type::bool()
    _ => @ir.Type::string()
  }
}

///|
fn is_string_sql_type(sql_type : String) -> Bool {
  match sql_type.to_upper() {
    "TEXT" | "VARCHAR" | "CHAR" => true
    _ => false
  }
}

///|
fn has_validatable_fields(s : @ir.Struct) -> Bool {
  for field in s.fields {
    if not(field.constraints.is_empty()) {
      return true
    }
  }
  false
}

///|
fn generate_constructor(buf : StringBuilder, s : @ir.Struct) -> Unit {
  buf.write_string("///|\npub fn ")
  buf.write_string(s.name)
  buf.write_string("::new(")
  for i, field in s.fields {
    if i > 0 {
      buf.write_string(", ")
    }
    buf.write_string(field.name)
    buf.write_string(" : ")
    emit_ir_type(buf, field.ty)
  }
  buf.write_string(") -> ")
  buf.write_string(s.name)
  buf.write_string(" {\n  { ")
  for i, field in s.fields {
    if i > 0 {
      buf.write_string(", ")
    }
    buf.write_string(field.name)
  }
  buf.write_string(" }\n}\n\n")
}

///|
fn emit_ir_type(buf : StringBuilder, ty : @ir.Type) -> Unit {
  match ty {
    @ir.Type::Primitive(name) => buf.write_string(name)
    @ir.Type::Optional(inner) => {
      emit_ir_type(buf, inner)
      buf.write_string("?")
    }
    @ir.Type::Array(inner) => {
      buf.write_string("Array[")
      emit_ir_type(buf, inner)
      buf.write_string("]")
    }
    @ir.Type::Named(name, _) => buf.write_string(name)
    _ => buf.write_string("Unit")
  }
}

///|
fn generate_query_function(
  buf : StringBuilder,
  query : QueryDef,
  backend : String,
) -> Unit {
  match backend {
    "sqlite" | "sqlite_js" => generate_query_function_sqlite(buf, query)
    "d1" => generate_query_function_d1(buf, query)
    "postgres" => generate_query_function_postgres(buf, query)
    "postgres_js" => generate_query_function_postgres_js(buf, query)
    "mysql_js" => generate_query_function_mysql_js(buf, query)
    _ => generate_query_function_sqlite(buf, query)
  }
}

///|
fn generate_query_function_sqlite(buf : StringBuilder, query : QueryDef) -> Unit {
  let func_name = to_snake_case(query.name)
  let row_type = query.name + "Row"
  let params_type = query.name + "Params"
  let sql_const = func_name + "_sql"

  // SQL constant
  buf.write_string("///|\npub let ")
  buf.write_string(sql_const)
  buf.write_string(" : String = \"")
  buf.write_string(escape_string(query.sql))
  buf.write_string("\"\n\n")

  // Function
  let return_type = match query.cmd {
    ":one" => row_type + "?"
    ":many" => "Array[" + row_type + "]"
    _ => "Unit"
  }
  buf.write_string("///| ")
  buf.write_string(query.name)
  buf.write_string("\npub fn ")
  buf.write_string(func_name)
  buf.write_string("(db : @sqlite.Sqlite3")
  if query.params.length() > 0 {
    buf.write_string(", params : ")
    buf.write_string(params_type)
  }
  buf.write_string(") -> ")
  buf.write_string(return_type)
  buf.write_string(" {\n")

  // Prepare
  buf.write_string("  let stmt = @sqlite.sqlite_prepare(db, cstring(")
  buf.write_string(sql_const)
  buf.write_string("))\n")

  // Bind parameters
  for i, param in query.params {
    let idx = (i + 1).to_string()
    let param_name = "params." + to_snake_case(param.0)
    buf.write_string("  ")
    buf.write_string(param_to_bind_call_sqlite(param.1, idx, param_name))
    buf.write_string(" |> ignore\n")
  }

  // Execute based on cmd
  match query.cmd {
    ":one" => {
      buf.write_string("  if @sqlite.sqlite_step(stmt) == @sqlite.SQLITE_ROW {\n")
      buf.write_string("    let row : ")
      buf.write_string(row_type)
      buf.write_string(" = {\n")
      for i, ret in query.returns {
        buf.write_string("      ")
        buf.write_string(to_snake_case(ret.0))
        buf.write_string(": ")
        buf.write_string(column_to_moonbit_sqlite(ret.1, i))
        buf.write_string(",\n")
      }
      buf.write_string("    }\n")
      buf.write_string("    @sqlite.sqlite_finalize(stmt)\n")
      buf.write_string("    Some(row)\n")
      buf.write_string("  } else {\n")
      buf.write_string("    @sqlite.sqlite_finalize(stmt)\n")
      buf.write_string("    None\n")
      buf.write_string("  }\n")
    }
    ":many" => {
      buf.write_string("  let results : Array[")
      buf.write_string(row_type)
      buf.write_string("] = []\n")
      buf.write_string("  while @sqlite.sqlite_step(stmt) == @sqlite.SQLITE_ROW {\n")
      buf.write_string("    let row : ")
      buf.write_string(row_type)
      buf.write_string(" = {\n")
      for i, ret in query.returns {
        buf.write_string("      ")
        buf.write_string(to_snake_case(ret.0))
        buf.write_string(": ")
        buf.write_string(column_to_moonbit_sqlite(ret.1, i))
        buf.write_string(",\n")
      }
      buf.write_string("    }\n")
      buf.write_string("    results.push(row)\n")
      buf.write_string("  }\n")
      buf.write_string("  @sqlite.sqlite_finalize(stmt)\n")
      buf.write_string("  results\n")
    }
    _ => {
      buf.write_string("  @sqlite.sqlite_step(stmt) |> ignore\n")
      buf.write_string("  @sqlite.sqlite_finalize(stmt)\n")
    }
  }
  buf.write_string("}\n\n")
}

///|
fn generate_query_function_d1(buf : StringBuilder, query : QueryDef) -> Unit {
  let func_name = to_snake_case(query.name)
  let row_type = query.name + "Row"
  let params_type = query.name + "Params"
  let sql_const = func_name + "_sql"

  // SQL constant
  buf.write_string("///|\npub let ")
  buf.write_string(sql_const)
  buf.write_string(" : String = \"")
  buf.write_string(escape_string(query.sql))
  buf.write_string("\"\n\n")

  // Function (async)
  let return_type = match query.cmd {
    ":one" => row_type + "?"
    ":many" => "Array[" + row_type + "]"
    _ => "Unit"
  }
  buf.write_string("///| ")
  buf.write_string(query.name)
  buf.write_string("\npub async fn ")
  buf.write_string(func_name)
  buf.write_string("(db : @cloudflare.D1Database")
  if query.params.length() > 0 {
    buf.write_string(", params : ")
    buf.write_string(params_type)
  }
  buf.write_string(") -> ")
  buf.write_string(return_type)
  buf.write_string(" raise @cloudflare.D1Error {\n")
  buf.write_string("  let stmt = db.prepare(")
  buf.write_string(sql_const)
  buf.write_string(")")
  if query.params.length() > 0 {
    buf.write_string(".bind([\n")
    for i, param in query.params {
      if i > 0 {
        buf.write_string(",\n")
      }
      buf.write_string("    ")
      buf.write_string(param_to_js_value(param.1, "params." + to_snake_case(param.0)))
    }
    buf.write_string("\n  ])")
  }
  buf.write_string("\n")
  match query.cmd {
    ":one" => {
      buf.write_string("  let result = stmt.first()\n")
      buf.write_string("  match result {\n")
      buf.write_string("    Some(row) => Some({\n")
      for ret in query.returns {
        buf.write_string("      ")
        buf.write_string(to_snake_case(ret.0))
        buf.write_string(": ")
        buf.write_string(js_value_to_moonbit(ret.1, "row[\"" + ret.0 + "\"]"))
        buf.write_string(",\n")
      }
      buf.write_string("    })\n")
      buf.write_string("    None => None\n")
      buf.write_string("  }\n")
    }
    ":many" => {
      buf.write_string("  let result = stmt.all()\n")
      buf.write_string("  result.results.map(fn(row) {\n")
      buf.write_string("    {\n")
      for ret in query.returns {
        buf.write_string("      ")
        buf.write_string(to_snake_case(ret.0))
        buf.write_string(": ")
        buf.write_string(js_value_to_moonbit(ret.1, "row[\"" + ret.0 + "\"]"))
        buf.write_string(",\n")
      }
      buf.write_string("    }\n")
      buf.write_string("  })\n")
    }
    _ => buf.write_string("  stmt.run()\n")
  }
  buf.write_string("}\n\n")
}

///|
fn generate_query_function_postgres(buf : StringBuilder, query : QueryDef) -> Unit {
  let func_name = to_snake_case(query.name)
  let row_type = query.name + "Row"
  let params_type = query.name + "Params"
  let sql_const = func_name + "_sql"

  // SQL constant (convert ? to $1, $2, etc.)
  buf.write_string("///|\npub let ")
  buf.write_string(sql_const)
  buf.write_string(" : String = \"")
  buf.write_string(escape_string(convert_placeholders_to_dollar(query.sql)))
  buf.write_string("\"\n\n")

  // Function
  let return_type = match query.cmd {
    ":one" => row_type + "?"
    ":many" => "Array[" + row_type + "]"
    _ => "Unit"
  }
  buf.write_string("///| ")
  buf.write_string(query.name)
  buf.write_string("\npub fn ")
  buf.write_string(func_name)
  buf.write_string("(conn : @postgres.Connection")
  if query.params.length() > 0 {
    buf.write_string(", params : ")
    buf.write_string(params_type)
  }
  buf.write_string(") -> ")
  buf.write_string(return_type)
  buf.write_string(" raise @postgres.PgError {\n")
  buf.write_string("  let result = conn.exec(")
  buf.write_string(sql_const)
  if query.params.length() > 0 {
    buf.write_string(", params=[")
    for i, param in query.params {
      if i > 0 {
        buf.write_string(", ")
      }
      buf.write_string(param_to_pg_value(param.1, "params." + to_snake_case(param.0)))
    }
    buf.write_string("]")
  }
  buf.write_string(")\n")
  match query.cmd {
    ":one" => {
      buf.write_string("  if result.row_count() > 0 {\n")
      buf.write_string("    let row = result.row(0)\n")
      buf.write_string("    Some({\n")
      for i, ret in query.returns {
        buf.write_string("      ")
        buf.write_string(to_snake_case(ret.0))
        buf.write_string(": ")
        buf.write_string(pg_value_to_moonbit(ret.1, i.to_string()))
        buf.write_string(",\n")
      }
      buf.write_string("    })\n")
      buf.write_string("  } else {\n")
      buf.write_string("    None\n")
      buf.write_string("  }\n")
    }
    ":many" => {
      buf.write_string("  let results : Array[")
      buf.write_string(row_type)
      buf.write_string("] = []\n")
      buf.write_string("  for i = 0; i < result.row_count(); i = i + 1 {\n")
      buf.write_string("    let row = result.row(i)\n")
      buf.write_string("    results.push({\n")
      for i, ret in query.returns {
        buf.write_string("      ")
        buf.write_string(to_snake_case(ret.0))
        buf.write_string(": ")
        buf.write_string(pg_value_to_moonbit(ret.1, i.to_string()))
        buf.write_string(",\n")
      }
      buf.write_string("    })\n")
      buf.write_string("  }\n")
      buf.write_string("  results\n")
    }
    _ => ()
  }
  buf.write_string("}\n\n")
}

///|
fn generate_query_function_postgres_js(
  buf : StringBuilder,
  query : QueryDef,
) -> Unit {
  let func_name = to_snake_case(query.name)
  let row_type = query.name + "Row"
  let params_type = query.name + "Params"
  let sql_const = func_name + "_sql"

  // SQL constant
  buf.write_string("///|\npub let ")
  buf.write_string(sql_const)
  buf.write_string(" : String = \"")
  buf.write_string(escape_string(convert_placeholders_to_dollar(query.sql)))
  buf.write_string("\"\n\n")

  // Function (async)
  let return_type = match query.cmd {
    ":one" => row_type + "?"
    ":many" => "Array[" + row_type + "]"
    _ => "Unit"
  }
  buf.write_string("///| ")
  buf.write_string(query.name)
  buf.write_string("\npub async fn ")
  buf.write_string(func_name)
  buf.write_string("(pool : @pg.Pool")
  if query.params.length() > 0 {
    buf.write_string(", params : ")
    buf.write_string(params_type)
  }
  buf.write_string(") -> ")
  buf.write_string(return_type)
  buf.write_string(" {\n")
  buf.write_string("  let result = pool.query(")
  buf.write_string(sql_const)
  if query.params.length() > 0 {
    buf.write_string(", values=[\n")
    for i, param in query.params {
      if i > 0 {
        buf.write_string(",\n")
      }
      buf.write_string("    ")
      buf.write_string(param_to_js_value(param.1, "params." + to_snake_case(param.0)))
    }
    buf.write_string("\n  ]")
  }
  buf.write_string(")\n")
  match query.cmd {
    ":one" => {
      buf.write_string("  if result.rows.length() > 0 {\n")
      buf.write_string("    let row = result.rows[0]\n")
      buf.write_string("    Some({\n")
      for ret in query.returns {
        buf.write_string("      ")
        buf.write_string(to_snake_case(ret.0))
        buf.write_string(": ")
        buf.write_string(js_value_to_moonbit(ret.1, "row[\"" + ret.0 + "\"]"))
        buf.write_string(",\n")
      }
      buf.write_string("    })\n")
      buf.write_string("  } else {\n")
      buf.write_string("    None\n")
      buf.write_string("  }\n")
    }
    ":many" => {
      buf.write_string("  result.rows.map(fn(row) {\n")
      buf.write_string("    {\n")
      for ret in query.returns {
        buf.write_string("      ")
        buf.write_string(to_snake_case(ret.0))
        buf.write_string(": ")
        buf.write_string(js_value_to_moonbit(ret.1, "row[\"" + ret.0 + "\"]"))
        buf.write_string(",\n")
      }
      buf.write_string("    }\n")
      buf.write_string("  })\n")
    }
    _ => ()
  }
  buf.write_string("}\n\n")
}

///|
fn generate_query_function_mysql_js(buf : StringBuilder, query : QueryDef) -> Unit {
  let func_name = to_snake_case(query.name)
  let row_type = query.name + "Row"
  let params_type = query.name + "Params"
  let sql_const = func_name + "_sql"

  // SQL constant
  buf.write_string("///|\npub let ")
  buf.write_string(sql_const)
  buf.write_string(" : String = \"")
  buf.write_string(escape_string(query.sql))
  buf.write_string("\"\n\n")

  // Function (async)
  let return_type = match query.cmd {
    ":one" => row_type + "?"
    ":many" => "Array[" + row_type + "]"
    _ => "Unit"
  }
  buf.write_string("///| ")
  buf.write_string(query.name)
  buf.write_string("\npub async fn ")
  buf.write_string(func_name)
  buf.write_string("(conn : @core.JsValue")
  if query.params.length() > 0 {
    buf.write_string(", params : ")
    buf.write_string(params_type)
  }
  buf.write_string(") -> ")
  buf.write_string(return_type)
  buf.write_string(" {\n")
  buf.write_string("  // MySQL implementation placeholder\n")
  buf.write_string("  // Use mysql2 npm package\n")
  match query.cmd {
    ":one" => buf.write_string("  None\n")
    ":many" => buf.write_string("  []\n")
    _ => ()
  }
  buf.write_string("}\n\n")
}

// Helper functions for different backends

///|
fn param_to_bind_call_sqlite(
  sql_type : String,
  idx : String,
  expr : String,
) -> String {
  match sql_type.to_upper() {
    "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" =>
      "@sqlite.sqlite_bind_int64(stmt, " + idx + ", " + expr + ")"
    "SMALLINT" =>
      "@sqlite.sqlite_bind_int(stmt, " + idx + ", " + expr + ".to_int())"
    "REAL" | "FLOAT" | "DOUBLE" =>
      "@sqlite.sqlite_bind_double(stmt, " + idx + ", " + expr + ")"
    "TEXT" | "VARCHAR" | "CHAR" =>
      "@sqlite.sqlite_bind_text(stmt, " + idx + ", cstring(" + expr + "))"
    "BLOB" | "BYTEA" =>
      "@sqlite.sqlite_bind_blob(stmt, " + idx + ", " + expr + ")"
    "BOOLEAN" | "BOOL" =>
      "@sqlite.sqlite_bind_int(stmt, " +
      idx +
      ", if " +
      expr +
      " { 1 } else { 0 })"
    _ => "@sqlite.sqlite_bind_text(stmt, " + idx + ", cstring(" + expr + "))"
  }
}

///|
fn column_to_moonbit_sqlite(sql_type : String, idx : Int) -> String {
  let idx_str = idx.to_string()
  match sql_type.to_upper() {
    "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" | "SMALLINT" =>
      "@sqlite.sqlite_column_int64(stmt, " + idx_str + ")"
    "REAL" | "FLOAT" | "DOUBLE" =>
      "@sqlite.sqlite_column_double(stmt, " + idx_str + ")"
    "TEXT" | "VARCHAR" | "CHAR" =>
      "bytes_to_string(@sqlite.sqlite_column_text(stmt, " + idx_str + "))"
    "BLOB" | "BYTEA" => "@sqlite.sqlite_column_blob(stmt, " + idx_str + ")"
    "BOOLEAN" | "BOOL" =>
      "@sqlite.sqlite_column_int(stmt, " + idx_str + ") != 0"
    _ => "bytes_to_string(@sqlite.sqlite_column_text(stmt, " + idx_str + "))"
  }
}

///|
fn param_to_js_value(sql_type : String, expr : String) -> String {
  match sql_type.to_upper() {
    "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" | "SMALLINT" =>
      "@core.JsValue::from_int64(" + expr + ")"
    "REAL" | "FLOAT" | "DOUBLE" =>
      "@core.JsValue::from_double(" + expr + ")"
    "TEXT" | "VARCHAR" | "CHAR" => "@core.JsValue::from_string(" + expr + ")"
    "BOOLEAN" | "BOOL" => "@core.JsValue::from_bool(" + expr + ")"
    _ => "@core.JsValue::from_string(" + expr + ")"
  }
}

///|
fn js_value_to_moonbit(sql_type : String, expr : String) -> String {
  match sql_type.to_upper() {
    "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" | "SMALLINT" =>
      expr + ".as_int64()"
    "REAL" | "FLOAT" | "DOUBLE" => expr + ".as_double()"
    "TEXT" | "VARCHAR" | "CHAR" => expr + ".as_string()"
    "BOOLEAN" | "BOOL" => expr + ".as_bool()"
    _ => expr + ".as_string()"
  }
}

///|
fn param_to_pg_value(sql_type : String, expr : String) -> String {
  match sql_type.to_upper() {
    "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" | "SMALLINT" =>
      "@postgres.PgValue::from_int64(" + expr + ")"
    "REAL" | "FLOAT" | "DOUBLE" =>
      "@postgres.PgValue::from_double(" + expr + ")"
    "TEXT" | "VARCHAR" | "CHAR" =>
      "@postgres.PgValue::from_string(" + expr + ")"
    "BOOLEAN" | "BOOL" => "@postgres.PgValue::from_bool(" + expr + ")"
    _ => "@postgres.PgValue::from_string(" + expr + ")"
  }
}

///|
fn pg_value_to_moonbit(sql_type : String, idx : String) -> String {
  match sql_type.to_upper() {
    "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" | "SMALLINT" =>
      "row.get_int64(" + idx + ")"
    "REAL" | "FLOAT" | "DOUBLE" => "row.get_double(" + idx + ")"
    "TEXT" | "VARCHAR" | "CHAR" => "row.get_string(" + idx + ")"
    "BOOLEAN" | "BOOL" => "row.get_bool(" + idx + ")"
    _ => "row.get_string(" + idx + ")"
  }
}

///|
fn convert_placeholders_to_dollar(sql : String) -> String {
  let buf = StringBuilder::new()
  let mut idx = 1
  for c in sql {
    if c == '?' {
      buf.write_string("$")
      buf.write_string(idx.to_string())
      idx = idx + 1
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
    if c >= 'A' && c <= 'Z' {
      if prev_lower {
        buf.write_char('_')
      }
      buf.write_char((c.to_int() + 32).unsafe_to_char())
      prev_lower = false
    } else {
      buf.write_char(c)
      prev_lower = c >= 'a' && c <= 'z'
    }
  }
  buf.to_string()
}

///|
fn escape_string(s : String) -> String {
  let buf = StringBuilder::new()
  for c in s {
    match c {
      '"' => buf.write_string("\\\"")
      '\\' => buf.write_string("\\\\")
      '\n' => buf.write_string(" ")
      '\r' => ()
      '\t' => buf.write_string(" ")
      _ => buf.write_char(c)
    }
  }
  buf.to_string()
}

// String utility functions

///|
fn substr(s : String, start : Int) -> String {
  let buf = StringBuilder::new()
  let chars = s.to_array()
  for i = start; i < chars.length(); i = i + 1 {
    buf.write_char(chars[i])
  }
  buf.to_string()
}

///|
fn split_lines(s : String) -> Array[String] {
  let lines : Array[String] = []
  let buf = StringBuilder::new()
  for c in s {
    if c == '\n' {
      lines.push(buf.to_string())
      buf.reset()
    } else if c != '\r' {
      buf.write_char(c)
    }
  }
  if buf.to_string().length() > 0 {
    lines.push(buf.to_string())
  }
  lines
}

///|
fn split_whitespace(s : String) -> Array[String] {
  let parts : Array[String] = []
  let buf = StringBuilder::new()
  for c in s {
    if c == ' ' || c == '\t' {
      if buf.to_string().length() > 0 {
        parts.push(buf.to_string())
        buf.reset()
      }
    } else {
      buf.write_char(c)
    }
  }
  if buf.to_string().length() > 0 {
    parts.push(buf.to_string())
  }
  parts
}

///|
fn split_by_comma(s : String) -> Array[String] {
  let parts : Array[String] = []
  let buf = StringBuilder::new()
  for c in s {
    if c == ',' {
      parts.push(buf.to_string())
      buf.reset()
    } else {
      buf.write_char(c)
    }
  }
  if buf.to_string().length() > 0 {
    parts.push(buf.to_string())
  }
  parts
}

///|
fn join_lines(lines : Array[String], sep : String) -> String {
  let buf = StringBuilder::new()
  for i, line in lines {
    if i > 0 {
      buf.write_string(sep)
    }
    buf.write_string(line)
  }
  buf.to_string()
}
