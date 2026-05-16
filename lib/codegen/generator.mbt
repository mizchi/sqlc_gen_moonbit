// Common code generation utilities using codegen.mbt

///|
pub fn generate_result_struct(
  buf : StringBuilder,
  _name : String,
  _columns : Array[@plugin.Column],
  overrides : Array[TypeOverride],
  query : @plugin.Query,
) -> Unit {
  let ir_struct = query_columns_to_ir_struct_full(query, overrides)
  emit_moonbit_struct(buf, ir_struct)
  buf.write_string("\n")
}

///|
pub fn generate_params_struct(
  buf : StringBuilder,
  _name : String,
  _params : Array[@plugin.Parameter],
  overrides : Array[TypeOverride],
  query : @plugin.Query,
) -> Unit {
  let ir_struct = query_params_to_ir_struct_full(query, overrides)
  emit_moonbit_struct(buf, ir_struct)
  buf.write_string("\n")
  // Generate constructor function
  generate_params_constructor(buf, ir_struct)
}

///|
fn emit_moonbit_struct(buf : StringBuilder, s : @ir.Struct) -> Unit {
  buf.write_string("///|\n")
  match s.doc {
    Some(doc) => {
      buf.write_string("/// ")
      buf.write_string(doc)
      buf.write_string("\n")
    }
    None => ()
  }
  emit_visibility(buf, s.visibility)
  buf.write_string("struct ")
  buf.write_string(s.name)
  if s.type_params.length() > 0 {
    buf.write_string("[")
    for i, param in s.type_params {
      if i > 0 {
        buf.write_string(", ")
      }
      buf.write_string(param)
    }
    buf.write_string("]")
  }
  buf.write_string(" {\n")
  for field in s.fields {
    match field.doc {
      Some(doc) => {
        buf.write_string("  /// ")
        buf.write_string(doc)
        buf.write_string("\n")
      }
      None => ()
    }
    buf.write_string("  ")
    if field.is_mutable {
      buf.write_string("mut ")
    }
    buf.write_string(field.name)
    buf.write_string(" : ")
    emit_ir_type(buf, field.ty)
    buf.write_string("\n")
  }
  buf.write_string("}")
  if s.derives.length() > 0 {
    buf.write_string(" derive(")
    for i, derive_name in s.derives {
      if i > 0 {
        buf.write_string(", ")
      }
      buf.write_string(derive_name)
    }
    buf.write_string(")")
  }
  buf.write_string("\n")
}

///|
fn emit_visibility(buf : StringBuilder, vis : @ir.Visibility) -> Unit {
  match vis {
    @ir.Visibility::Public => buf.write_string("pub ")
    @ir.Visibility::Private => buf.write_string("priv ")
    @ir.Visibility::ReadOnly => buf.write_string("pub(readonly) ")
    @ir.Visibility::Package(pkg) => {
      buf.write_string("pub(")
      buf.write_string(pkg)
      buf.write_string(") ")
    }
  }
}

///|
fn generate_params_constructor(buf : StringBuilder, s : @ir.Struct) -> Unit {
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
  buf.write_string(" {\n")
  buf.write_string("  { ")
  for i, field in s.fields {
    if i > 0 {
      buf.write_string(", ")
    }
    buf.write_string(field.name)
  }
  if s.fields.length() == 1 {
    buf.write_string(",")
  }
  buf.write_string(" }\n")
  buf.write_string("}\n\n")
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
    @ir.Type::Named(name, args) => {
      buf.write_string(name)
      if args.length() > 0 {
        buf.write_string("[")
        for i, arg in args {
          if i > 0 {
            buf.write_string(", ")
          }
          emit_ir_type(buf, arg)
        }
        buf.write_string("]")
      }
    }
    _ => buf.write_string("Unit")
  }
}

///|
pub fn find_column_override(
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
pub fn sql_type_to_moonbit(
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
        None => !col.not_null
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
        // Integer types - SQLite INTEGER is 64bit, so use Int64 for consistency
        "INTEGER"
        | "INT"
        | "INT4"
        | "INT8"
        | "BIGINT"
        | "SERIAL"
        | "BIGSERIAL" => "Int64"
        "INT2" | "SMALLINT" => "Int"
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
