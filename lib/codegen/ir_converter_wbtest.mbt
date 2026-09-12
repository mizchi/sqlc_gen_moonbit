// Tests for IR Converter

///|
test "column_to_ir_type - integer not null" {
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
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "INTEGER", },
    is_sqlc_slice: false,
    embed_table: @plugin.Identifier::default(),
    original_name: "",
    unsigned: false,
    array_dims: 0,
  }
  let ty = column_to_ir_type(col)
  // Should be Int64 (not Optional)
  match ty {
    @ir.Type::Primitive(name) => assert_eq(name, "Int64")
    _ => fail("Expected Primitive type")
  }
}

///|
test "column_to_ir_type - text nullable" {
  let col = @plugin.Column::{
    name: "name",
    not_null: false,
    is_array: false,
    comment: "",
    length: -1,
    is_named_param: false,
    is_func_call: false,
    scope: "",
    table: @plugin.Identifier::default(),
    table_alias: "",
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "TEXT", },
    is_sqlc_slice: false,
    embed_table: @plugin.Identifier::default(),
    original_name: "",
    unsigned: false,
    array_dims: 0,
  }
  let ty = column_to_ir_type(col)
  // Should be Optional(String)
  match ty {
    @ir.Type::Optional(@ir.Type::Primitive(name)) => assert_eq(name, "String")
    _ => fail("Expected Optional(Primitive) type")
  }
}

///|
test "column_to_ir_type - sqlc.slice text param wraps in Array" {
  let col = @plugin.Column::{
    name: "ids",
    not_null: true,
    is_array: false,
    comment: "",
    length: -1,
    is_named_param: false,
    is_func_call: false,
    scope: "",
    table: @plugin.Identifier::default(),
    table_alias: "",
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "TEXT", },
    is_sqlc_slice: true,
    embed_table: @plugin.Identifier::default(),
    original_name: "",
    unsigned: false,
    array_dims: 0,
  }
  let ty = column_to_ir_type(col)
  // sqlc.slice always becomes Array[T] regardless of the column's
  // NOT NULL flag — the slice itself is what gets bound, not a
  // single nullable scalar.
  match ty {
    @ir.Type::Array(@ir.Type::Primitive(name)) => assert_eq(name, "String")
    _ => fail("Expected Array(Primitive) type for sqlc.slice param")
  }
}

///|
test "column_to_ir_type - sqlc.slice integer param wraps in Array" {
  let col = @plugin.Column::{
    name: "user_ids",
    not_null: true,
    is_array: false,
    comment: "",
    length: -1,
    is_named_param: false,
    is_func_call: false,
    scope: "",
    table: @plugin.Identifier::default(),
    table_alias: "",
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "INTEGER", },
    is_sqlc_slice: true,
    embed_table: @plugin.Identifier::default(),
    original_name: "",
    unsigned: false,
    array_dims: 0,
  }
  let ty = column_to_ir_type(col)
  match ty {
    @ir.Type::Array(@ir.Type::Primitive(name)) => assert_eq(name, "Int64")
    _ => fail("Expected Array(Primitive) type for sqlc.slice param")
  }
}

///|
test "column_to_constraints - required string" {
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
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "TEXT", },
    is_sqlc_slice: false,
    embed_table: @plugin.Identifier::default(),
    original_name: "",
    unsigned: false,
    array_dims: 0,
  }
  let c = column_to_constraints(col)
  assert_true(c.required)
  assert_true(c.max_length is None)
}

///|
test "column_to_constraints - varchar with length" {
  let col = @plugin.Column::{
    name: "email",
    not_null: true,
    is_array: false,
    comment: "",
    length: 255,
    is_named_param: false,
    is_func_call: false,
    scope: "",
    table: @plugin.Identifier::default(),
    table_alias: "",
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "VARCHAR", },
    is_sqlc_slice: false,
    embed_table: @plugin.Identifier::default(),
    original_name: "",
    unsigned: false,
    array_dims: 0,
  }
  let c = column_to_constraints(col)
  assert_true(c.required)
  assert_eq(c.max_length, Some(255))
}

///|
test "generated structs use current derive syntax" {
  let query = @plugin.Query::{
    name: "GetUser",
    cmd: ":one",
    text: "SELECT id FROM users WHERE id = ?",
    columns: [
      @plugin.Column::{
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
        type_: @plugin.Identifier::{ catalog: "", schema: "", name: "INTEGER", },
        is_sqlc_slice: false,
        embed_table: @plugin.Identifier::default(),
        original_name: "",
        unsigned: false,
        array_dims: 0,
      },
    ],
    params: [],
    comments: [],
    filename: "",
    insert_into_table: @plugin.Identifier::default(),
  }
  let buf = StringBuilder()
  generate_result_struct(buf, "GetUserRow", query.columns, [], query)
  let code = buf.to_string()
  assert_true(code.contains("} derive(Debug, Eq)"))
  assert_false(code.contains("#derive("))
}

///|
test "single-field params constructor is unambiguous" {
  let query = @plugin.Query::{
    name: "GetUserById",
    cmd: ":one",
    text: "SELECT id FROM users WHERE id = ?",
    columns: [],
    params: [
      @plugin.Parameter::{
        number: 1,
        column: @plugin.Column::{
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
          type_: @plugin.Identifier::{
            catalog: "",
            schema: "",
            name: "INTEGER",
          },
          is_sqlc_slice: false,
          embed_table: @plugin.Identifier::default(),
          original_name: "",
          unsigned: false,
          array_dims: 0,
        },
      },
    ],
    comments: [],
    filename: "",
    insert_into_table: @plugin.Identifier::default(),
  }
  let buf = StringBuilder()
  generate_params_struct(buf, "GetUserByIdParams", query.params, [], query)
  let code = buf.to_string()
  assert_true(code.contains("{ id, }"))
  assert_false(code.contains("{ id }"))
}

///|
test "sqlite moon package declares native target" {
  let pkg = generate_moon_pkg({
    backend: Sqlite,
    validators: false,
    json_schema: false,
    file_prefix: "sql",
    derives: [],
    overrides: [],
  })
  assert_true(pkg.contains("supported_targets = \"native\""))
  assert_false(pkg.contains("options("))
}

///|
test "json schema required fields match properties" {
  let request = @plugin.GenerateRequest::{
    settings: @plugin.Settings::default(),
    catalog: @plugin.Catalog::default(),
    queries: [
      @plugin.Query::{
        name: "CreateSkillPrice",
        cmd: ":exec",
        text: "INSERT INTO skill_prices VALUES (?, ?)",
        columns: [],
        params: [
          @plugin.Parameter::{
            number: 1,
            column: @plugin.Column::{
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
              type_: @plugin.Identifier::{
                catalog: "",
                schema: "",
                name: "TEXT",
              },
              is_sqlc_slice: false,
              embed_table: @plugin.Identifier::default(),
              original_name: "",
              unsigned: false,
              array_dims: 0,
            },
          },
          @plugin.Parameter::{
            number: 2,
            column: @plugin.Column::{
              name: "skill_id",
              not_null: true,
              is_array: false,
              comment: "",
              length: -1,
              is_named_param: false,
              is_func_call: false,
              scope: "",
              table: @plugin.Identifier::default(),
              table_alias: "",
              type_: @plugin.Identifier::{
                catalog: "",
                schema: "",
                name: "TEXT",
              },
              is_sqlc_slice: false,
              embed_table: @plugin.Identifier::default(),
              original_name: "",
              unsigned: false,
              array_dims: 0,
            },
          },
        ],
        comments: [],
        filename: "",
        insert_into_table: @plugin.Identifier::default(),
      },
    ],
    sqlc_version: "test",
    plugin_options: Bytes::new(0),
    global_options: Bytes::new(0),
  }
  let schema = generate_json_schema(request)
  assert_true(schema.contains("\"required\": [\"id\", \"skill_id\"]"))
  assert_false(schema.contains("skill_ic"))
}

///|
test "serialize_response preserves large file contents" {
  let content = "x".repeat(4096) + "\"required\": [\"id\", \"skill_id\"]"
  let response = @plugin.GenerateResponse::new([
    @plugin.File::new("a.mbt", string_to_utf8("first")),
    @plugin.File::new("sqlc_schema.json", string_to_utf8(content)),
    @plugin.File::new("z.mbt", string_to_utf8("last")),
  ])
  let bytes = serialize_response(response)
  let reader = @protobuf.BytesReader::from_bytes(bytes)
  let decoded : @plugin.GenerateResponse = @protobuf.Read::read(reader) catch {
    _ => fail("failed to decode response")
  }
  assert_eq(decoded.files.length(), 3)
  let schema = bytes_to_string_simple(decoded.files[1].contents)
  assert_true(schema.contains("\"required\": [\"id\", \"skill_id\"]"))
  assert_false(schema.contains("skill_ic"))
}

///|
test "column_to_constraints - unsigned integer" {
  let col = @plugin.Column::{
    name: "count",
    not_null: true,
    is_array: false,
    comment: "",
    length: -1,
    is_named_param: false,
    is_func_call: false,
    scope: "",
    table: @plugin.Identifier::default(),
    table_alias: "",
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "INTEGER", },
    is_sqlc_slice: false,
    embed_table: @plugin.Identifier::default(),
    original_name: "",
    unsigned: true,
    array_dims: 0,
  }
  let c = column_to_constraints(col)
  assert_true(c.unsigned)
}

///|
test "column_to_ir_field" {
  let col = @plugin.Column::{
    name: "user_name",
    not_null: true,
    is_array: false,
    comment: "",
    length: 100,
    is_named_param: false,
    is_func_call: false,
    scope: "",
    table: @plugin.Identifier::default(),
    table_alias: "",
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "VARCHAR", },
    is_sqlc_slice: false,
    embed_table: @plugin.Identifier::default(),
    original_name: "",
    unsigned: false,
    array_dims: 0,
  }
  let field = column_to_ir_field(col, 1)
  assert_eq(field.name, "user_name")
  // Should be String (not Optional since not_null=true)
  match field.ty {
    @ir.Type::Primitive(name) => assert_eq(name, "String")
    _ => fail("Expected Primitive type")
  }
  assert_true(field.constraints.required)
  assert_eq(field.constraints.max_length, Some(100))
}

///|
test "query_params_to_ir_struct" {
  let query = @plugin.Query::{
    name: "CreateUser",
    cmd: ":exec",
    text: "INSERT INTO users (name, email) VALUES (?, ?)",
    columns: [],
    params: [
      @plugin.Parameter::{
        number: 1,
        column: @plugin.Column::{
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
          type_: @plugin.Identifier::{ catalog: "", schema: "", name: "TEXT", },
          is_sqlc_slice: false,
          embed_table: @plugin.Identifier::default(),
          original_name: "",
          unsigned: false,
          array_dims: 0,
        },
      },
      @plugin.Parameter::{
        number: 2,
        column: @plugin.Column::{
          name: "email",
          not_null: true,
          is_array: false,
          comment: "",
          length: 255,
          is_named_param: false,
          is_func_call: false,
          scope: "",
          table: @plugin.Identifier::default(),
          table_alias: "",
          type_: @plugin.Identifier::{
            catalog: "",
            schema: "",
            name: "VARCHAR",
          },
          is_sqlc_slice: false,
          embed_table: @plugin.Identifier::default(),
          original_name: "",
          unsigned: false,
          array_dims: 0,
        },
      },
    ],
    comments: [],
    filename: "",
    insert_into_table: @plugin.Identifier::default(),
  }
  let s = query_params_to_ir_struct(query)
  assert_eq(s.name, "CreateUserParams")
  assert_eq(s.fields.length(), 2)
  assert_eq(s.fields[0].name, "name")
  assert_eq(s.fields[1].name, "email")
}
