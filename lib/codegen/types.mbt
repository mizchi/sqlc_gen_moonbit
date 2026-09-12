// Backend types and plugin options

///|
pub enum Backend {
  Sqlite
  SqliteJs
  D1
  Postgres
  PostgresJs
  MysqlJs
}

///|
pub fn Backend::to_string(self : Backend) -> String {
  match self {
    Sqlite => "sqlite"
    SqliteJs => "sqlite_js"
    D1 => "d1"
    Postgres => "postgres"
    PostgresJs => "postgres_js"
    MysqlJs => "mysql_js"
  }
}

///|
pub impl Debug for Backend with fn to_repr(value : Backend) -> Repr {
  Repr(value.to_string())
}

///|
pub impl @json.FromJson for Backend with fn from_json(
  json : Json,
  path : @json.JsonPath,
) -> Backend {
  let value : String = @json.from_json(json)
  match value {
    "sqlite" => Sqlite
    "sqlite_js" => SqliteJs
    "d1" => D1
    "postgres" => Postgres
    "postgres_js" => PostgresJs
    "mysql_js" => MysqlJs
    _ => raise @json.JsonDecodeError((path, "Invalid Backend"))
  }
}

///|
pub struct TypeOverride {
  column : String? // "table.column" format
  db_type : String? // "INTEGER", "TEXT" etc
  moonbit_type : String // target type override
  nullable : Bool? // explicit nullable override
} derive(Debug, @json.FromJson)

///|
pub struct Options {
  backend : Backend
  validators : Bool
  json_schema : Bool
  derives : Array[String]
  file_prefix : String
  overrides : Array[TypeOverride]
} derive(Debug)

///|
pub impl FromJson for Options with fn from_json(
  json : Json,
  path : @json.JsonPath,
) -> Options {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError((path, "Options: expected an object"))
  }

  let backend = match obj.get("backend") {
    Some(v) => Backend::from_json(v, path)
    _ => Sqlite
  }
  let validators = match obj.get("validators") {
    Some(True) => true
    _ => false
  }
  let json_schema = match obj.get("json_schema") {
    Some(True) => true
    _ => false
  }
  let derives = match obj.get("derives") {
    Some(Array(arr)) => {
      let result : Array[String] = []
      for item in arr {
        match item {
          String(s) => result.push(s)
          _ => ()
        }
      }
      result
    }
    _ => []
  }
  let file_prefix = match obj.get("file_prefix") {
    Some(Json::String(prefix)) if prefix != "" => prefix
    _ => "sqlc"
  }

  let overrides = match obj.get("overrides") {
    Some(Array(arr)) => {
      let result : Array[TypeOverride] = []
      for item in arr {
        result.push(TypeOverride::from_json(item, path))
      }
      result
    }
    _ => []
  }
  Options::{
    backend,
    validators,
    json_schema,
    file_prefix,
    derives,
    overrides,
  }
}

///|

///|
pub fn parse_plugin_options(data : Bytes) -> Options {
  // Parse JSON-like format: {"backend":"d1","validators":true,"json_schema":true,"derive":["Show", "Debug"],"overrides":[...]}
  let json_str = bytes_to_string_simple(data)
  let default_options = Options::{
    backend: Sqlite,
    validators: false,
    json_schema: false,
    file_prefix: "sqlc",
    derives: [],
    overrides: [],
  }
  let json : Json = @json.parse(json_str) catch { _ => return default_options }

  @json.from_json(json) catch {
    _ => default_options
  }
}
