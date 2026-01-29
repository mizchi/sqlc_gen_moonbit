// D1 backend (Cloudflare Workers, js target) - Trait implementation

///|
pub struct D1Backend {
  _placeholder : Unit
}

///|
pub fn D1Backend::new() -> D1Backend {
  { _placeholder: () }
}

///|
impl BackendCodeGen for D1Backend with info(_self) {
  BackendInfo::new(
    "d1",
    "@cloudflare.D1Database",
    Some("@cloudflare.D1Error"),
    false,
    true, // async
    "js",
  )
}

///|
impl BackendCodeGen for D1Backend with generate_helpers(_self, buf) {
  // D1 doesn't need helper functions
  buf.write_string("// Requires: mizchi/cloudflare\n\n")
}

///|
impl BackendCodeGen for D1Backend with param_bind_expr(_self, col, _idx, expr) {
  let type_name = col.type_.name.to_upper()
  let is_nullable = not(col.not_null)
  if is_nullable {
    // For nullable params, use match to handle None - @core.null() for null value
    "(match " + expr + " { Some(v) => @core.any(v); None => @core.null() })"
  } else {
    match type_name {
      "INTEGER"
      | "INT"
      | "INT4"
      | "SMALLINT"
      | "INT8"
      | "BIGINT"
      | "REAL"
      | "FLOAT"
      | "DOUBLE"
      | "NUMERIC"
      | "DECIMAL"
      | "TEXT"
      | "VARCHAR"
      | "CHAR"
      | "CHARACTER"
      | "CLOB"
      | "BOOLEAN"
      | "BOOL" => "@core.any(" + expr + ")"
      _ => "@core.any(" + expr + ".to_string())"
    }
  }
}

///|
impl BackendCodeGen for D1Backend with column_read_expr(
  _self,
  col,
  _idx,
  obj_expr,
) {
  let type_name = col.type_.name.to_upper()
  let field_name = "\"" + col.name + "\""
  let field_access = obj_expr + "[" + field_name + "]"

  // Handle nullable columns
  if not(col.not_null) {
    // For nullable columns, check for null/undefined first
    match type_name {
      "INTEGER" | "INT" | "INT4" =>
        "(if @core.is_null(" +
        field_access +
        ") || @core.typeof_(" +
        field_access +
        ") == \"undefined\" { None } else { Some(@cloudflare.js_number_to_int64(" +
        field_access +
        ")) })"
      "SMALLINT" =>
        "(if @core.is_null(" +
        field_access +
        ") || @core.typeof_(" +
        field_access +
        ") == \"undefined\" { None } else { Some(@cloudflare.js_number_to_int64(" +
        field_access +
        ").to_int()) })"
      "INT8" | "BIGINT" =>
        "(if @core.is_null(" +
        field_access +
        ") || @core.typeof_(" +
        field_access +
        ") == \"undefined\" { None } else { Some(@cloudflare.js_number_to_int64(" +
        field_access +
        ")) })"
      _ =>
        "(if @core.is_null(" +
        field_access +
        ") || @core.typeof_(" +
        field_access +
        ") == \"undefined\" { None } else { Some(" +
        field_access +
        ".cast()) })"
    }
  } else {
    // Non-nullable columns
    match type_name {
      "INTEGER" | "INT" | "INT4" =>
        "@cloudflare.js_number_to_int64(" + field_access + ")"
      "SMALLINT" =>
        "@cloudflare.js_number_to_int64(" + field_access + ").to_int()"
      "INT8" | "BIGINT" =>
        "@cloudflare.js_number_to_int64(" + field_access + ")"
      "REAL" | "FLOAT" | "DOUBLE" | "NUMERIC" | "DECIMAL" =>
        field_access + ".cast()"
      "TEXT" | "VARCHAR" | "CHAR" | "CHARACTER" | "CLOB" =>
        field_access + ".cast()"
      "BOOLEAN" | "BOOL" => field_access + ".cast()"
      _ => field_access + ".cast()"
    }
  }
}

///|
impl BackendCodeGen for D1Backend with generate_one(
  self,
  buf,
  query,
  _overrides,
) {
  let info = self.info()
  let sql_const = backend_get_sql_const(query)
  let return_type = backend_get_return_type(info, query)

  // Doc comment and signature
  backend_write_doc_comment(buf, query)
  backend_write_fn_signature_start(buf, info, query, return_type)

  // Prepare and bind statement
  buf.write_string("  let stmt = db.prepare(")
  buf.write_string(sql_const)
  buf.write_string(")")
  d1_write_bindings(self, buf, query)
  buf.write_string("\n")

  // Execute and read result
  buf.write_string("  match stmt.first() {\n")
  buf.write_string("    None => None\n")
  buf.write_string("    Some(result) => Some({\n")
  for col in query.columns {
    buf.write_string("      ")
    buf.write_string(to_snake_case(col.name))
    buf.write_string(": ")
    buf.write_string(self.column_read_expr(col, 0, "result"))
    buf.write_string(",\n")
  }
  buf.write_string("    })\n")
  buf.write_string("  }\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for D1Backend with generate_many(
  self,
  buf,
  query,
  _overrides,
) {
  let info = self.info()
  let row_type = backend_get_row_type(query)
  let sql_const = backend_get_sql_const(query)
  let return_type = backend_get_return_type(info, query)

  // Doc comment and signature
  backend_write_doc_comment(buf, query)
  backend_write_fn_signature_start(buf, info, query, return_type)

  // Prepare and bind statement
  buf.write_string("  let stmt = db.prepare(")
  buf.write_string(sql_const)
  buf.write_string(")")
  d1_write_bindings(self, buf, query)
  buf.write_string("\n")

  // Execute and collect results
  buf.write_string("  let d1_result = stmt.all()\n")
  buf.write_string("  let results_arr = d1_result.get_results()\n")
  buf.write_string("  let results : Array[")
  buf.write_string(row_type)
  buf.write_string("] = []\n")
  buf.write_string("  for row in results_arr {\n")
  buf.write_string("    results.push({\n")
  for col in query.columns {
    buf.write_string("      ")
    buf.write_string(to_snake_case(col.name))
    buf.write_string(": ")
    buf.write_string(self.column_read_expr(col, 0, "row"))
    buf.write_string(",\n")
  }
  buf.write_string("    })\n")
  buf.write_string("  }\n")
  buf.write_string("  results\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for D1Backend with generate_exec(
  self,
  buf,
  query,
  _overrides,
) {
  let info = self.info()
  let sql_const = backend_get_sql_const(query)
  let return_type = backend_get_return_type(info, query)

  // Doc comment and signature
  backend_write_doc_comment(buf, query)
  backend_write_fn_signature_start(buf, info, query, return_type)

  // Prepare and bind statement
  buf.write_string("  let stmt = db.prepare(")
  buf.write_string(sql_const)
  buf.write_string(")")
  d1_write_bindings(self, buf, query)
  buf.write_string("\n")

  // Execute
  buf.write_string("  stmt.run() |> ignore\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for D1Backend with generate_execrows(
  self,
  buf,
  query,
  _overrides,
) {
  let info = self.info()
  let sql_const = backend_get_sql_const(query)
  let return_type = backend_get_return_type(info, query)

  // Doc comment and signature
  backend_write_doc_comment(buf, query)
  backend_write_fn_signature_start(buf, info, query, return_type)

  // Prepare and bind statement
  buf.write_string("  let stmt = db.prepare(")
  buf.write_string(sql_const)
  buf.write_string(")")
  d1_write_bindings(self, buf, query)
  buf.write_string("\n")

  // Execute and return affected rows
  buf.write_string("  let result = stmt.run()\n")
  buf.write_string("  match result.meta() {\n")
  buf.write_string("    Some(m) => m.changes().unwrap_or(0)\n")
  buf.write_string("    None => 0\n")
  buf.write_string("  }\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for D1Backend with generate_execlastid(
  self,
  buf,
  query,
  _overrides,
) {
  let info = self.info()
  let sql_const = backend_get_sql_const(query)
  let return_type = backend_get_return_type(info, query)

  // Doc comment and signature
  backend_write_doc_comment(buf, query)
  backend_write_fn_signature_start(buf, info, query, return_type)

  // Prepare and bind statement
  buf.write_string("  let stmt = db.prepare(")
  buf.write_string(sql_const)
  buf.write_string(")")
  d1_write_bindings(self, buf, query)
  buf.write_string("\n")

  // Execute and return last insert id
  buf.write_string("  let result = stmt.run()\n")
  buf.write_string("  match result.meta() {\n")
  buf.write_string("    Some(m) => m.last_row_id().unwrap_or(0).to_int64()\n")
  buf.write_string("    None => 0L\n")
  buf.write_string("  }\n")
  buf.write_string("}\n\n")
}

// Helper function to write D1 bindings

///|
fn d1_write_bindings(
  backend : D1Backend,
  buf : StringBuilder,
  query : @plugin.Query,
) -> Unit {
  if query.params.length() > 0 {
    buf.write_string(".bind([")
    for i, param in query.params {
      let col = param.column
      let param_name = backend_get_param_name(col, param.number)
      if i > 0 {
        buf.write_string(", ")
      }
      buf.write_string(
        backend.param_bind_expr(col, i + 1, "params." + param_name),
      )
    }
    buf.write_string("])")
  }
}
