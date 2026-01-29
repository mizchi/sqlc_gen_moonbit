// Backend types and plugin options

///|
pub enum Backend {
  Sqlite
  SqliteJs
  D1
  Postgres
  PostgresJs
}

///|
pub fn Backend::to_string(self : Backend) -> String {
  match self {
    Sqlite => "sqlite"
    SqliteJs => "sqlite_js"
    D1 => "d1"
    Postgres => "postgres"
    PostgresJs => "postgres_js"
  }
}

///|
pub struct TypeOverride {
  column : String? // "table.column" format
  db_type : String? // "INTEGER", "TEXT" etc
  moonbit_type : String // target type override
  nullable : Bool? // explicit nullable override
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
  let backend = if json_str.contains("\"postgres_js\"") {
    PostgresJs
  } else if json_str.contains("\"postgres\"") {
    Postgres
  } else if json_str.contains("\"d1\"") {
    D1
  } else if json_str.contains("\"sqlite_js\"") {
    SqliteJs
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
