// JSON Schema generation

///|
pub fn generate_json_schema(request : @plugin.GenerateRequest) -> String {
  let buf = StringBuilder::new()
  buf.write_string("{\n")
  buf.write_string(
    "  \"$schema\": \"http://json-schema.org/draft-07/schema#\",\n",
  )
  buf.write_string("  \"definitions\": {\n")
  let mut first_def = true

  // Generate schema for each Params struct
  for query in request.queries {
    if query.params.length() > 0 {
      if not(first_def) {
        buf.write_string(",\n")
      }
      first_def = false
      let struct_name = to_pascal_case(query.name) + "Params"
      generate_json_schema_object(buf, struct_name, query.params, "    ")
    }
  }

  // Generate schema for each Row struct
  for query in request.queries {
    if query.cmd == ":one" || query.cmd == ":many" {
      if query.columns.length() > 0 {
        if not(first_def) {
          buf.write_string(",\n")
        }
        first_def = false
        let struct_name = to_pascal_case(query.name) + "Row"
        generate_json_schema_row(buf, struct_name, query.columns, "    ")
      }
    }
  }
  buf.write_string("\n  }\n")
  buf.write_string("}\n")
  buf.to_string()
}

///|
fn generate_json_schema_object(
  buf : StringBuilder,
  name : String,
  params : Array[@plugin.Parameter],
  indent : String,
) -> Unit {
  buf.write_string(indent)
  buf.write_string("\"")
  buf.write_string(name)
  buf.write_string("\": {\n")
  buf.write_string(indent)
  buf.write_string("  \"type\": \"object\",\n")

  // Required fields
  let required : Array[String] = []
  for param in params {
    if param.column.not_null {
      let param_name = if param.column.name.length() > 0 {
        to_snake_case(param.column.name)
      } else {
        "p" + param.number.to_string()
      }
      required.push(param_name)
    }
  }
  if required.length() > 0 {
    buf.write_string(indent)
    buf.write_string("  \"required\": [")
    for i, r in required {
      if i > 0 {
        buf.write_string(", ")
      }
      buf.write_string("\"")
      buf.write_string(r)
      buf.write_string("\"")
    }
    buf.write_string("],\n")
  }

  // Properties
  buf.write_string(indent)
  buf.write_string("  \"properties\": {\n")
  for i, param in params {
    if i > 0 {
      buf.write_string(",\n")
    }
    let col = param.column
    let param_name = if col.name.length() > 0 {
      to_snake_case(col.name)
    } else {
      "p" + param.number.to_string()
    }
    generate_json_schema_property(buf, param_name, col, indent + "    ")
  }
  buf.write_string("\n")
  buf.write_string(indent)
  buf.write_string("  }\n")
  buf.write_string(indent)
  buf.write_string("}")
}

///|
fn generate_json_schema_row(
  buf : StringBuilder,
  name : String,
  columns : Array[@plugin.Column],
  indent : String,
) -> Unit {
  buf.write_string(indent)
  buf.write_string("\"")
  buf.write_string(name)
  buf.write_string("\": {\n")
  buf.write_string(indent)
  buf.write_string("  \"type\": \"object\",\n")

  // Required fields
  let required : Array[String] = []
  for col in columns {
    if col.not_null {
      required.push(to_snake_case(col.name))
    }
  }
  if required.length() > 0 {
    buf.write_string(indent)
    buf.write_string("  \"required\": [")
    for i, r in required {
      if i > 0 {
        buf.write_string(", ")
      }
      buf.write_string("\"")
      buf.write_string(r)
      buf.write_string("\"")
    }
    buf.write_string("],\n")
  }

  // Properties
  buf.write_string(indent)
  buf.write_string("  \"properties\": {\n")
  for i, col in columns {
    if i > 0 {
      buf.write_string(",\n")
    }
    generate_json_schema_property(
      buf,
      to_snake_case(col.name),
      col,
      indent + "    ",
    )
  }
  buf.write_string("\n")
  buf.write_string(indent)
  buf.write_string("  }\n")
  buf.write_string(indent)
  buf.write_string("}")
}

///|
fn generate_json_schema_property(
  buf : StringBuilder,
  name : String,
  col : @plugin.Column,
  indent : String,
) -> Unit {
  buf.write_string(indent)
  buf.write_string("\"")
  buf.write_string(name)
  buf.write_string("\": {")
  let type_name = col.type_.name.to_upper()
  let json_type = match type_name {
    "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" | "SMALLINT" => "integer"
    "REAL" | "FLOAT" | "DOUBLE" | "NUMERIC" | "DECIMAL" => "number"
    "BOOLEAN" | "BOOL" => "boolean"
    _ => "string"
  }

  // Handle nullable types
  if col.not_null {
    buf.write_string(" \"type\": \"")
    buf.write_string(json_type)
    buf.write_string("\"")
  } else {
    buf.write_string(" \"type\": [\"")
    buf.write_string(json_type)
    buf.write_string("\", \"null\"]")
  }

  // Add constraints
  if col.length > 0 && json_type == "string" {
    buf.write_string(", \"maxLength\": ")
    buf.write_string(col.length.to_string())
  }
  if col.not_null && json_type == "string" {
    buf.write_string(", \"minLength\": 1")
  }
  if col.unsigned && (json_type == "integer" || json_type == "number") {
    buf.write_string(", \"minimum\": 0")
  }
  buf.write_string(" }")
}
