// IR Converter - Convert @plugin.Column to @ir.Field for codegen.mbt integration

///|
/// Convert SQL type name to IR Type
pub fn column_to_ir_type(col : @plugin.Column) -> @ir.Type {
  let type_name = col.type_.name.to_upper()
  let base_type : @ir.Type = match type_name {
    "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" | "SERIAL" | "BIGSERIAL" =>
      @ir.Type::int64()
    "INT2" | "SMALLINT" => @ir.Type::int()
    "REAL"
    | "FLOAT4"
    | "FLOAT8"
    | "FLOAT"
    | "DOUBLE"
    | "DOUBLE PRECISION"
    | "NUMERIC"
    | "DECIMAL" => @ir.Type::double()
    "TEXT"
    | "VARCHAR"
    | "CHAR"
    | "CHARACTER"
    | "CHARACTER VARYING"
    | "BPCHAR"
    | "CLOB" => @ir.Type::string()
    "BOOLEAN" | "BOOL" => @ir.Type::bool()
    "BYTEA" | "BLOB" => @ir.Type::bytes()
    _ =>
      // Default to String for unknown types
      @ir.Type::string()
  }
  // Wrap in Optional if nullable
  if col.not_null {
    base_type
  } else {
    @ir.Type::optional(base_type)
  }
}

///|
/// Convert @plugin.Column to @ir.Constraints
pub fn column_to_constraints(col : @plugin.Column) -> @ir.Constraints {
  let mut c = @ir.Constraints::new()
  let type_name = col.type_.name.to_upper()
  // NOT NULL for string types means required (non-empty)
  let is_string_type = match type_name {
    "TEXT" | "VARCHAR" | "CHAR" | "CHARACTER" | "CHARACTER VARYING" | "CLOB" =>
      true
    _ => false
  }
  if col.not_null && is_string_type {
    c = c.as_required()
  }
  // VARCHAR(n) length constraint
  if col.length > 0 && is_string_type {
    c = c.with_max_length(col.length)
  }
  // Unsigned constraint for numeric types
  if col.unsigned {
    c = c.as_unsigned()
  }
  c
}

///|
/// Convert @plugin.Column to @ir.Field
pub fn column_to_ir_field(
  col : @plugin.Column,
  param_number : Int,
) -> @ir.Field {
  let field_name = if col.name.length() > 0 {
    to_snake_case(col.name)
  } else {
    "p" + param_number.to_string()
  }
  let ty = column_to_ir_type(col)
  let constraints = column_to_constraints(col)
  @ir.Field::new(field_name, ty).with_constraints(constraints)
}

///|
/// Convert query params to IR Struct
pub fn query_params_to_ir_struct(query : @plugin.Query) -> @ir.Struct {
  let struct_name = to_pascal_case(query.name) + "Params"
  let mut s = @ir.Struct::new(struct_name)
  for param in query.params {
    let field = column_to_ir_field(param.column, param.number)
    s = s.with_field(field)
  }
  s
}

///|
/// Convert query columns to IR Struct (for Row types)
pub fn query_columns_to_ir_struct(query : @plugin.Query) -> @ir.Struct {
  let struct_name = to_pascal_case(query.name) + "Row"
  let mut s = @ir.Struct::new(struct_name)
  for i, col in query.columns {
    let field = column_to_ir_field(col, i + 1)
    s = s.with_field(field)
  }
  s
}

///|
/// Convert SQL type to IR Type with TypeOverride support
pub fn column_to_ir_type_with_override(
  col : @plugin.Column,
  overrides : Array[TypeOverride],
) -> @ir.Type {
  match find_column_override(col, overrides) {
    Some(override_) => {
      let base_type = @ir.Type::named(override_.moonbit_type)
      let is_nullable = match override_.nullable {
        Some(n) => n
        None => not(col.not_null)
      }
      if is_nullable {
        @ir.Type::optional(base_type)
      } else {
        base_type
      }
    }
    None => column_to_ir_type(col)
  }
}

///|
/// Convert @plugin.Column to @ir.Field with TypeOverride support
pub fn column_to_ir_field_with_override(
  col : @plugin.Column,
  param_number : Int,
  overrides : Array[TypeOverride],
) -> @ir.Field {
  let field_name = if col.name.length() > 0 {
    to_snake_case(col.name)
  } else {
    "p" + param_number.to_string()
  }
  let ty = column_to_ir_type_with_override(col, overrides)
  let constraints = column_to_constraints(col)
  @ir.Field::new(field_name, ty).with_constraints(constraints)
}

///|
/// Convert query params to IR Struct with TypeOverride and derives
pub fn query_params_to_ir_struct_full(
  query : @plugin.Query,
  overrides : Array[TypeOverride],
) -> @ir.Struct {
  let struct_name = to_pascal_case(query.name) + "Params"
  let mut s = @ir.Struct::new(struct_name).with_derives(["Show", "Eq"])
  for param in query.params {
    let field = column_to_ir_field_with_override(
      param.column,
      param.number,
      overrides,
    )
    s = s.with_field(field)
  }
  s
}

///|
/// Convert query columns to IR Struct with TypeOverride and derives
pub fn query_columns_to_ir_struct_full(
  query : @plugin.Query,
  overrides : Array[TypeOverride],
) -> @ir.Struct {
  let struct_name = to_pascal_case(query.name) + "Row"
  let mut s = @ir.Struct::new(struct_name).with_derives(["Show", "Eq"])
  for i, col in query.columns {
    let field = column_to_ir_field_with_override(col, i + 1, overrides)
    s = s.with_field(field)
  }
  s
}
