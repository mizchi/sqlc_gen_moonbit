// Tests for D1 backend codegen — particularly the dynamic-SQL path
// triggered by `sqlc.slice('name')` params.

///|
fn make_slice_query(
  name : String,
  cmd : String,
  text : String,
  params : Array[@plugin.Parameter],
  columns : Array[@plugin.Column],
) -> @plugin.Query {
  @plugin.Query::{
    name,
    cmd,
    text,
    columns,
    params,
    comments: [],
    filename: "",
    insert_into_table: @plugin.Identifier::default(),
  }
}

///|
fn make_simple_column(
  name : String,
  type_name : String,
  not_null : Bool,
  is_sqlc_slice : Bool,
) -> @plugin.Column {
  @plugin.Column::{
    name,
    not_null,
    is_array: false,
    comment: "",
    length: -1,
    is_named_param: false,
    is_func_call: false,
    scope: "",
    table: @plugin.Identifier::default(),
    table_alias: "",
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: type_name },
    is_sqlc_slice,
    embed_table: @plugin.Identifier::default(),
    original_name: "",
    unsigned: false,
    array_dims: 0,
  }
}

///|
test "generate_queries_d1 emits slice helper once" {
  let req = @plugin.GenerateRequest::{
    settings: @plugin.Settings::default(),
    catalog: @plugin.Catalog::default(),
    queries: [],
    sqlc_version: "",
    plugin_options: Bytes::default(),
    global_options: Bytes::default(),
  }
  let out = generate_queries_d1(req, [])
  // Helper is always emitted in the prelude (cheap, lets us avoid
  // threading "any slice present" knowledge across the catalog).
  assert_true(out.contains("fn sqlc_slice_placeholders(n : Int) -> String"))
  assert_true(out.contains("if n <= 0 {"))
  assert_true(out.contains("return \"NULL\""))
}

///|
test "generate_queries_d1 - static query path unchanged" {
  let query = make_slice_query(
    "GetUser",
    ":one",
    "SELECT id, name FROM users WHERE id = ?",
    [
      @plugin.Parameter::{
        number: 1,
        column: make_simple_column("id", "INTEGER", true, false),
      },
    ],
    [
      make_simple_column("id", "INTEGER", true, false),
      make_simple_column("name", "TEXT", true, false),
    ],
  )
  let req = @plugin.GenerateRequest::{
    settings: @plugin.Settings::default(),
    catalog: @plugin.Catalog::default(),
    queries: [query],
    sqlc_version: "",
    plugin_options: Bytes::default(),
    global_options: Bytes::default(),
  }
  let out = generate_queries_d1(req, [])
  // No slice => the existing static `.bind([...])` shape is preserved.
  assert_true(out.contains(".bind([@core.any(params.id)])"))
  assert_false(out.contains("let mut sql_text"))
  assert_false(out.contains("sqlc_slice_placeholders(params"))
}

///|
test "generate_queries_d1 - slice param triggers dynamic SQL + bind loop" {
  let query = make_slice_query(
    "ListByIds",
    ":many",
    "SELECT id, name FROM users WHERE id IN (/*SLICE:ids*/?)",
    [
      @plugin.Parameter::{
        number: 1,
        column: make_simple_column("ids", "INTEGER", true, true),
      },
    ],
    [
      make_simple_column("id", "INTEGER", true, false),
      make_simple_column("name", "TEXT", true, false),
    ],
  )
  let req = @plugin.GenerateRequest::{
    settings: @plugin.Settings::default(),
    catalog: @plugin.Catalog::default(),
    queries: [query],
    sqlc_version: "",
    plugin_options: Bytes::default(),
    global_options: Bytes::default(),
  }
  let out = generate_queries_d1(req, [])
  // SQL substitution happens at call time.
  assert_true(out.contains("let mut sql_text = list_by_ids_sql"))
  assert_true(
    out.contains(
      "sql_text = sql_text.replace(old=\"/*SLICE:ids*/?\", new=sqlc_slice_placeholders(params.ids.length()))",
    ),
  )
  // bind list built dynamically.
  assert_true(out.contains("let bind_args : Array[@core.Any] = []"))
  assert_true(out.contains("for slice_item in params.ids {"))
  assert_true(out.contains("bind_args.push(@core.any(slice_item))"))
  assert_true(out.contains("db.prepare(sql_text).bind(bind_args)"))
  // Static path must NOT also fire.
  assert_false(out.contains(".bind([@core.any(params.ids)])"))
}

///|
test "generate_queries_d1 - mixed scalar + slice params bind in order" {
  let query = make_slice_query(
    "ListOwnerByIds",
    ":many",
    "SELECT id FROM things WHERE owner_id = ? AND id IN (/*SLICE:ids*/?)",
    [
      @plugin.Parameter::{
        number: 1,
        column: make_simple_column("owner_id", "TEXT", true, false),
      },
      @plugin.Parameter::{
        number: 2,
        column: make_simple_column("ids", "INTEGER", true, true),
      },
    ],
    [make_simple_column("id", "INTEGER", true, false)],
  )
  let req = @plugin.GenerateRequest::{
    settings: @plugin.Settings::default(),
    catalog: @plugin.Catalog::default(),
    queries: [query],
    sqlc_version: "",
    plugin_options: Bytes::default(),
    global_options: Bytes::default(),
  }
  let out = generate_queries_d1(req, [])
  // Scalar pushed before slice, in declaration order.
  let scalar_push_idx = match find_substring(out, "bind_args.push(@core.any(params.owner_id))") {
    Some(idx) => idx
    None => -1
  }
  let slice_loop_idx = match find_substring(out, "for slice_item in params.ids") {
    Some(idx) => idx
    None => -1
  }
  assert_true(scalar_push_idx >= 0)
  assert_true(slice_loop_idx >= 0)
  assert_true(scalar_push_idx < slice_loop_idx)
}
