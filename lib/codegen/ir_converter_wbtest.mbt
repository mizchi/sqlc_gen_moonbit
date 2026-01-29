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
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "INTEGER" },
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
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "TEXT" },
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
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "TEXT" },
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
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "VARCHAR" },
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
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "INTEGER" },
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
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: "VARCHAR" },
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
          type_: @plugin.Identifier::{ catalog: "", schema: "", name: "TEXT" },
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
