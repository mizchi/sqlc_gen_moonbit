// JSON Schema generation using codegen.mbt

///|
pub fn generate_json_schema(request : @plugin.GenerateRequest) -> String {
  let buf = StringBuilder::new()
  buf.write_string("{\n")
  buf.write_string(
    "  \"$schema\": \"http://json-schema.org/draft-07/schema#\",\n",
  )
  buf.write_string("  \"definitions\": {\n")
  let emitter = @emit.JsonSchemaEmitter::new()
  let mut first_def = true

  // Generate schema for each Params struct
  for query in request.queries {
    if query.params.length() > 0 {
      if not(first_def) {
        buf.write_string(",\n")
      }
      first_def = false
      let ir_struct = query_params_to_ir_struct(query)
      write_definition(buf, emitter, ir_struct)
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
        let ir_struct = query_columns_to_ir_struct(query)
        write_definition(buf, emitter, ir_struct)
      }
    }
  }
  buf.write_string("\n  }\n")
  buf.write_string("}\n")
  buf.to_string()
}

///|
fn write_definition(
  buf : StringBuilder,
  emitter : @emit.JsonSchemaEmitter,
  s : @ir.Struct,
) -> Unit {
  buf.write_string("    \"")
  buf.write_string(s.name)
  buf.write_string("\": ")
  // Emit struct schema without $schema (it's a definition)
  emit_struct_definition(buf, emitter, s)
}

///|
fn emit_struct_definition(
  buf : StringBuilder,
  _emitter : @emit.JsonSchemaEmitter,
  s : @ir.Struct,
) -> Unit {
  buf.write_string("{\n")
  buf.write_string("      \"type\": \"object\"")

  // Required fields
  let required : Array[String] = []
  for field in s.fields {
    match field.ty {
      @ir.Type::Optional(_) => ()
      _ => required.push(field.name)
    }
  }
  if required.length() > 0 {
    buf.write_string(",\n      \"required\": [")
    for i, name in required {
      if i > 0 {
        buf.write_string(", ")
      }
      buf.write_string("\"")
      buf.write_string(name)
      buf.write_string("\"")
    }
    buf.write_string("]")
  }

  // Properties
  if s.fields.length() > 0 {
    buf.write_string(",\n      \"properties\": {\n")
    for i, field in s.fields {
      if i > 0 {
        buf.write_string(",\n")
      }
      emit_field_property(buf, field)
    }
    buf.write_string("\n      }")
  }
  buf.write_string("\n    }")
}

///|
fn emit_field_property(buf : StringBuilder, field : @ir.Field) -> Unit {
  buf.write_string("        \"")
  buf.write_string(field.name)
  buf.write_string("\": {")
  let json_type = ir_type_to_json_type(field.ty)
  buf.write_string(" \"type\": ")
  buf.write_string(json_type)

  // Constraints
  let c = field.constraints
  match c.max_length {
    Some(len) => {
      buf.write_string(", \"maxLength\": ")
      buf.write_string(len.to_string())
    }
    None => ()
  }
  match c.min_length {
    Some(len) => {
      buf.write_string(", \"minLength\": ")
      buf.write_string(len.to_string())
    }
    None => ()
  }
  if c.unsigned {
    buf.write_string(", \"minimum\": 0")
  }
  match c.min_value {
    Some(val) =>
      if not(c.unsigned) {
        buf.write_string(", \"minimum\": ")
        buf.write_string(val.to_string())
      }
    None => ()
  }
  match c.max_value {
    Some(val) => {
      buf.write_string(", \"maximum\": ")
      buf.write_string(val.to_string())
    }
    None => ()
  }
  buf.write_string(" }")
}

///|
fn ir_type_to_json_type(ty : @ir.Type) -> String {
  match ty {
    @ir.Type::Primitive(name) =>
      match name {
        "Int" | "Int64" | "UInt" | "UInt64" => "\"integer\""
        "Double" | "Float" => "\"number\""
        "Bool" => "\"boolean\""
        "String" => "\"string\""
        "Bytes" => "\"string\""
        _ => "\"string\""
      }
    @ir.Type::Optional(inner) => {
      let inner_type = ir_type_to_json_type(inner)
      "[" + inner_type + ", \"null\"]"
    }
    @ir.Type::Array(_) => "\"array\""
    _ => "\"object\""
  }
}
