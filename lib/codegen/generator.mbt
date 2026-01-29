// Common code generation utilities

///|
pub fn generate_result_struct(
  buf : StringBuilder,
  name : String,
  columns : Array[@plugin.Column],
  overrides : Array[TypeOverride],
) -> Unit {
  buf.write_string("///|\npub struct ")
  buf.write_string(name)
  buf.write_string(" {\n")
  for col in columns {
    buf.write_string("  ")
    buf.write_string(to_snake_case(col.name))
    buf.write_string(" : ")
    buf.write_string(sql_type_to_moonbit(col, overrides))
    buf.write_string("\n")
  }
  buf.write_string("} derive(Show, Eq)\n\n")
}

///|
pub fn generate_params_struct(
  buf : StringBuilder,
  name : String,
  params : Array[@plugin.Parameter],
  overrides : Array[TypeOverride],
) -> Unit {
  buf.write_string("///|\npub struct ")
  buf.write_string(name)
  buf.write_string(" {\n")
  for param in params {
    let col = param.column
    let param_name = if col.name.length() > 0 {
      to_snake_case(col.name)
    } else {
      "p" + param.number.to_string()
    }
    buf.write_string("  ")
    buf.write_string(param_name)
    buf.write_string(" : ")
    buf.write_string(sql_type_to_moonbit(col, overrides))
    buf.write_string("\n")
  }
  buf.write_string("} derive(Show, Eq)\n\n")

  // Generate constructor function
  buf.write_string("///|\npub fn ")
  buf.write_string(name)
  buf.write_string("::new(")
  for i, param in params {
    let col = param.column
    let param_name = if col.name.length() > 0 {
      to_snake_case(col.name)
    } else {
      "p" + param.number.to_string()
    }
    if i > 0 {
      buf.write_string(", ")
    }
    buf.write_string(param_name)
    buf.write_string(" : ")
    buf.write_string(sql_type_to_moonbit(col, overrides))
  }
  buf.write_string(") -> ")
  buf.write_string(name)
  buf.write_string(" {\n")
  buf.write_string("  { ")
  for i, param in params {
    let col = param.column
    let param_name = if col.name.length() > 0 {
      to_snake_case(col.name)
    } else {
      "p" + param.number.to_string()
    }
    if i > 0 {
      buf.write_string(", ")
    }
    buf.write_string(param_name)
  }
  buf.write_string(", }\n")
  buf.write_string("}\n\n")
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
        None => not(col.not_null)
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
