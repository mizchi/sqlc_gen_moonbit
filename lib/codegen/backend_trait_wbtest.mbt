// Tests for Backend Trait implementation

///|
test "sqlite backend info" {
  let backend = SqliteBackend::new()
  let info = backend.info()
  assert_eq(info.name, "sqlite")
  assert_eq(info.db_type, "@sqlite.Sqlite3")
  assert_eq(info.error_type, Some("SqlError"))
  assert_eq(info.uses_result, false)
  assert_eq(info.is_async, false)
  assert_eq(info.target, "native")
}

///|
test "sqlite backend param bind expr - integer" {
  let backend = SqliteBackend::new()
  let col = @plugin.Column::{
    name: "id",
    not_null: true,
    is_array: false,
    comment: "",
    length: -1,
    is_named_param: false,
    is_func_call: false,
    scope: "",
    table: @plugin.Identifier::default(),
    table_alias: "",
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "INTEGER" },
    is_sqlc_slice: false,
    embed_table: @plugin.Identifier::default(),
    original_name: "",
    unsigned: false,
    array_dims: 0,
  }
  let expr = backend.param_bind_expr(col, 1, "params.id")
  assert_true(expr.contains("sqlite_bind_int64"))
  assert_true(expr.contains("params.id"))
}

///|
test "sqlite backend column read expr - text" {
  let backend = SqliteBackend::new()
  let col = @plugin.Column::{
    name: "name",
    not_null: true,
    is_array: false,
    comment: "",
    length: -1,
    is_named_param: false,
    is_func_call: false,
    scope: "",
    table: @plugin.Identifier::default(),
    table_alias: "",
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "TEXT" },
    is_sqlc_slice: false,
    embed_table: @plugin.Identifier::default(),
    original_name: "",
    unsigned: false,
    array_dims: 0,
  }
  let expr = backend.column_read_expr(col, 0, "stmt")
  assert_true(expr.contains("bytes_to_string"))
  assert_true(expr.contains("sqlite_column_text"))
}

///|
test "backend_get_func_name" {
  let query = @plugin.Query::{
    name: "GetUser",
    cmd: ":one",
    text: "SELECT * FROM users WHERE id = ?",
    columns: [],
    params: [],
    comments: [],
    filename: "",
    insert_into_table: @plugin.Identifier::default(),
  }
  assert_eq(backend_get_func_name(query), "get_user")
}

///|
test "backend_get_row_type" {
  let query = @plugin.Query::{
    name: "ListUsers",
    cmd: ":many",
    text: "SELECT * FROM users",
    columns: [],
    params: [],
    comments: [],
    filename: "",
    insert_into_table: @plugin.Identifier::default(),
  }
  assert_eq(backend_get_row_type(query), "ListUsersRow")
}

///|
test "backend_get_return_type one" {
  let info = BackendInfo::new("test", "TestDb", None, false, false, "native")
  let query = @plugin.Query::{
    name: "GetUser",
    cmd: ":one",
    text: "",
    columns: [],
    params: [],
    comments: [],
    filename: "",
    insert_into_table: @plugin.Identifier::default(),
  }
  assert_eq(backend_get_return_type(info, query), "GetUserRow?")
}

///|
test "backend_get_return_type many" {
  let info = BackendInfo::new("test", "TestDb", None, false, false, "native")
  let query = @plugin.Query::{
    name: "ListUsers",
    cmd: ":many",
    text: "",
    columns: [],
    params: [],
    comments: [],
    filename: "",
    insert_into_table: @plugin.Identifier::default(),
  }
  assert_eq(backend_get_return_type(info, query), "Array[ListUsersRow]")
}

///|
test "backend_get_return_type with result wrapper" {
  let info = BackendInfo::new(
    "postgres",
    "@postgres.Connection",
    Some("@postgres.PgError"),
    true,
    false,
    "native",
  )
  let query = @plugin.Query::{
    name: "GetUser",
    cmd: ":one",
    text: "",
    columns: [],
    params: [],
    comments: [],
    filename: "",
    insert_into_table: @plugin.Identifier::default(),
  }
  assert_eq(
    backend_get_return_type(info, query),
    "Result[GetUserRow?, @postgres.PgError]",
  )
}

///|
test "d1 backend info" {
  let backend = D1Backend::new()
  let info = backend.info()
  assert_eq(info.name, "d1")
  assert_eq(info.db_type, "@cloudflare.D1Database")
  assert_eq(info.error_type, Some("@cloudflare.D1Error"))
  assert_eq(info.uses_result, false)
  assert_eq(info.is_async, true)
  assert_eq(info.target, "js")
}

///|
test "d1 backend param bind expr - nullable" {
  let backend = D1Backend::new()
  let col = @plugin.Column::{
    name: "email",
    not_null: false,
    is_array: false,
    comment: "",
    length: -1,
    is_named_param: false,
    is_func_call: false,
    scope: "",
    table: @plugin.Identifier::default(),
    table_alias: "",
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "TEXT" },
    is_sqlc_slice: false,
    embed_table: @plugin.Identifier::default(),
    original_name: "",
    unsigned: false,
    array_dims: 0,
  }
  let expr = backend.param_bind_expr(col, 1, "params.email")
  assert_true(expr.contains("match"))
  assert_true(expr.contains("@core.null()"))
}

///|
test "sqlite_js backend info" {
  let backend = SqliteJsBackend::new()
  let info = backend.info()
  assert_eq(info.name, "sqlite_js")
  assert_eq(info.db_type, "@sqlite.Database")
  assert_eq(info.error_type, Some("SqlError"))
  assert_eq(info.is_async, false)
  assert_eq(info.target, "js")
}

///|
test "postgres backend info" {
  let backend = PostgresBackend::new()
  let info = backend.info()
  assert_eq(info.name, "postgres")
  assert_eq(info.db_type, "@postgres.Connection")
  assert_eq(info.error_type, Some("@postgres.PgError"))
  assert_eq(info.uses_result, true)
  assert_eq(info.is_async, false)
  assert_eq(info.target, "native")
}
