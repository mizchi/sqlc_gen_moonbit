///|
fn issue18_column(name : String, type_name : String) -> @plugin.Column {
  @plugin.Column::{
    name,
    not_null: true,
    is_array: false,
    comment: "",
    length: -1,
    is_named_param: false,
    is_func_call: false,
    scope: "",
    table: @plugin.Identifier::{ catalog: "", schema: "", name: "t", },
    table_alias: "",
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: type_name, },
    is_sqlc_slice: false,
    embed_table: @plugin.Identifier::default(),
    original_name: "",
    unsigned: false,
    array_dims: 0,
  }
}

///|
fn issue18_param(number : Int, col : @plugin.Column) -> @plugin.Parameter {
  @plugin.Parameter::{ number, column: col, }
}

///|
fn issue18_request() -> @plugin.GenerateRequest {
  let id_col = issue18_column("id", "INTEGER")
  let value_col = issue18_column("value", "TEXT")
  let slug_col = issue18_column("slug", "TEXT")
  let table = @plugin.Table::{
    rel: @plugin.Identifier::{ catalog: "", schema: "", name: "t", },
    columns: [id_col, value_col, slug_col],
    comment: "",
  }
  let schema = @plugin.Schema::{
    comment: "",
    name: "main",
    tables: [table],
    enums: [],
    composite_types: [],
  }
  let sql =
    #|INSERT INTO t (id, value, slug)
    #|VALUES (?, ?, ?)
    #|ON CONFLICT(id) DO UPDATE SET
    #|  value = excluded.value,
    #|  slug = CASE WHEN sqlc.arg('new_slug') <> '' THEN sqlc.arg('new_slug') ELSE slug END
  let query = @plugin.Query::{
    text: sql,
    name: "UpsertWithSqlcArg",
    cmd: ":exec",
    columns: [],
    params: [
      issue18_param(1, id_col),
      issue18_param(2, value_col),
      issue18_param(3, slug_col),
    ],
    comments: [],
    filename: "query.sql",
    insert_into_table: @plugin.Identifier::{
      catalog: "",
      schema: "",
      name: "t",
    },
  }
  let request = @plugin.GenerateRequest::default()
  request.catalog = @plugin.Catalog::{
    comment: "",
    default_schema: "main",
    name: "",
    schemas: [schema],
  }
  request.queries = [query]
  request.sqlc_version = "test"
  request
}

///|
fn issue19_request() -> @plugin.GenerateRequest {
  let object_sha_col = issue18_column("object_sha", "TEXT")
  let size_bytes_col = issue18_column("size_bytes", "INTEGER")
  let table = @plugin.Table::{
    rel: @plugin.Identifier::{ catalog: "", schema: "", name: "objects", },
    columns: [object_sha_col, size_bytes_col],
    comment: "",
  }
  let schema = @plugin.Schema::{
    comment: "",
    name: "main",
    tables: [table],
    enums: [],
    composite_types: [],
  }
  let sql =
    #|SELECT
    #|  COUNT(object_sha) AS pack_count,
    #|  COALESCE(SUM(size_bytes), 0) AS pack_bytes
    #|FROM objects
    #|GROUP BY object_sha
    #|HAVING COUNT(object_sha) >= sqlc.arg('min_packs')
    #|   OR COALESCE(SUM(size_bytes), 0) >= sqlc.arg('min_bytes')
  let query = @plugin.Query::{
    text: sql,
    name: "AggregateInference",
    cmd: ":many",
    columns: [
      issue18_column("pack_count", "integer"),
      {
        ..issue18_column("pack_bytes", "any"),
        not_null: false,
        is_func_call: false,
        table: @plugin.Identifier::default(),
      },
    ],
    params: [],
    comments: [],
    filename: "query.sql",
    insert_into_table: @plugin.Identifier::default(),
  }
  let request = @plugin.GenerateRequest::default()
  request.catalog = @plugin.Catalog::{
    comment: "",
    default_schema: "main",
    name: "",
    schemas: [schema],
  }
  request.queries = [query]
  request.sqlc_version = "test"
  request
}

///|
fn issue19_plain_sum_request() -> @plugin.GenerateRequest {
  let request = issue19_request()
  let query = request.queries[0]
  let sql =
    #|SELECT
    #|  COUNT(object_sha) AS pack_count,
    #|  SUM(size_bytes) AS pack_bytes
    #|FROM objects
    #|GROUP BY object_sha
  query.text = sql
  request
}

///|
test "normalize sqlc.arg left in ON CONFLICT DO UPDATE" {
  let request = issue18_request()
  normalize_sqlc_args_in_request(request, D1)
  let query = request.queries[0]
  assert_eq(query.params.length(), 4)
  assert_eq(query.params[3].number, 4)
  assert_eq(query.params[3].column.name, "new_slug")
  assert_eq(query.params[3].column.type_.name, "TEXT")
  assert_true(query.params[3].column.is_named_param)
  assert_true(query.text.contains("?4 <> '' THEN ?4"))
  assert_false(query.text.contains("sqlc.arg"))
}

///|
test "normalized sqlc.arg is emitted in types and D1 bind sequence" {
  let request = issue18_request()
  normalize_sqlc_args_in_request(request, D1)
  let options = Options::{
    backend: D1,
    validators: false,
    json_schema: false,
    file_prefix: "sql",
    derives: [],
    overrides: [],
  }
  let types = generate_types(request, options)
  assert_true(types.contains("new_slug : String"))
  assert_true(
    types.contains(
      "pub fn UpsertWithSqlcArgParams::new(id : Int64, value : String, slug : String, new_slug : String)",
    ),
  )
  let queries = generate_queries_d1(request, [])
  assert_true(
    queries.contains(
      ".bind([d1_bind_int64(params.id), @core.any(params.value), @core.any(params.slug), @core.any(params.new_slug)])",
    ),
  )
  assert_false(queries.contains("sqlc.arg"))
}

///|
test "normalize aggregate sqlc.arg params and COALESCE SUM row type" {
  let request = issue19_request()
  normalize_sqlc_args_in_request(request, D1)
  let query = request.queries[0]
  assert_eq(query.params.length(), 2)
  assert_eq(query.params[0].column.name, "min_packs")
  assert_eq(query.params[0].column.type_.name, "INTEGER")
  assert_eq(query.params[1].column.name, "min_bytes")
  assert_eq(query.params[1].column.type_.name, "INTEGER")
  assert_true(query.text.contains("COUNT(object_sha) >= ?1"))
  assert_true(query.text.contains("COALESCE(SUM(size_bytes), 0) >= ?2"))
  assert_false(query.text.contains("sqlc.arg"))
  assert_eq(query.columns[1].name, "pack_bytes")
  assert_eq(query.columns[1].type_.name, "INTEGER")
  assert_true(query.columns[1].not_null)
}

///|
test "plain SUM row type stays nullable when sqlc cannot infer it" {
  let request = issue19_plain_sum_request()
  normalize_sqlc_args_in_request(request, D1)
  let query = request.queries[0]
  assert_eq(query.columns[1].type_.name, "any")
  assert_false(query.columns[1].not_null)
}

///|
test "normalized aggregate inference is emitted in D1 types and binds" {
  let request = issue19_request()
  normalize_sqlc_args_in_request(request, D1)
  let options = Options::{
    backend: D1,
    validators: false,
    derives: [],
    file_prefix: "sql",
    json_schema: false,
    overrides: [],
  }
  let types = generate_types(request, options)
  assert_true(types.contains("pack_count : Int64"))
  assert_true(types.contains("pack_bytes : Int64"))
  assert_true(types.contains("min_packs : Int64"))
  assert_true(types.contains("min_bytes : Int64"))
  let queries = generate_queries_d1(request, [])
  assert_true(
    queries.contains(
      ".bind([d1_bind_int64(params.min_packs), d1_bind_int64(params.min_bytes)])",
    ),
  )
  assert_true(
    queries.contains(
      "pack_bytes: d1_read_int64(d1_require_field(row, \"pack_bytes\"))",
    ),
  )
  assert_false(queries.contains("sqlc.arg"))
}
