// SQLite JS backend (js target, high-level API) - Trait implementation

///|
pub struct SqliteJsBackend {
  _placeholder : Unit
}

///|
pub fn SqliteJsBackend::new() -> SqliteJsBackend {
  { _placeholder: () }
}

///|
impl BackendCodeGen for SqliteJsBackend with info(_self) {
  BackendInfo::new(
    "sqlite_js",
    "@sqlite.Database",
    Some("SqlError"),
    false,
    false,
    "js",
  )
}

///|
impl BackendCodeGen for SqliteJsBackend with generate_helpers(_self, buf) {
  buf.write_string("// Requires: mizchi/sqlite (JS target)\n\n")
  buf.write_string("///|\nfn bytes_to_string(b : Bytes) -> String {\n")
  buf.write_string("  let decoder = @encoding.decoder(@encoding.UTF8)\n")
  buf.write_string("  decoder.decode_lossy(b[0:b.length()])\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for SqliteJsBackend with param_bind_expr(
  _self,
  col,
  _idx,
  expr,
) {
  let type_name = col.type_.name.to_upper()
  let is_nullable = not(col.not_null)
  if is_nullable {
    // For nullable types, handle Option
    match type_name {
      "INTEGER" | "INT" | "INT4" =>
        "(match " +
        expr +
        " { Some(v) => @sqlite.Int64(v); None => @sqlite.Null })"
      "INT2" | "SMALLINT" =>
        "(match " +
        expr +
        " { Some(v) => @sqlite.Int(v); None => @sqlite.Null })"
      "INT8" | "BIGINT" =>
        "(match " +
        expr +
        " { Some(v) => @sqlite.Int64(v); None => @sqlite.Null })"
      "REAL" | "FLOAT" | "DOUBLE" | "NUMERIC" | "DECIMAL" =>
        "(match " +
        expr +
        " { Some(v) => @sqlite.Double(v); None => @sqlite.Null })"
      "TEXT" | "VARCHAR" | "CHAR" | "CHARACTER" | "CLOB" =>
        "(match " +
        expr +
        " { Some(v) => @sqlite.Text(@encoding.encode(@encoding.UTF8, v)); None => @sqlite.Null })"
      "BLOB" | "BYTEA" =>
        "(match " +
        expr +
        " { Some(v) => @sqlite.Blob(v); None => @sqlite.Null })"
      "BOOLEAN" | "BOOL" =>
        "(match " +
        expr +
        " { Some(v) => @sqlite.Int(if v { 1 } else { 0 }); None => @sqlite.Null })"
      _ =>
        "(match " +
        expr +
        " { Some(v) => @sqlite.Text(@encoding.encode(@encoding.UTF8, v.to_string())); None => @sqlite.Null })"
    }
  } else {
    match type_name {
      "INTEGER" | "INT" | "INT4" => "@sqlite.Int64(" + expr + ")"
      "INT2" | "SMALLINT" => "@sqlite.Int(" + expr + ")"
      "INT8" | "BIGINT" => "@sqlite.Int64(" + expr + ")"
      "REAL" | "FLOAT" | "DOUBLE" | "NUMERIC" | "DECIMAL" =>
        "@sqlite.Double(" + expr + ")"
      "TEXT" | "VARCHAR" | "CHAR" | "CHARACTER" | "CLOB" =>
        "@sqlite.Text(@encoding.encode(@encoding.UTF8, " + expr + "))"
      "BLOB" | "BYTEA" => "@sqlite.Blob(" + expr + ")"
      "BOOLEAN" | "BOOL" => "@sqlite.Int(if " + expr + " { 1 } else { 0 })"
      _ =>
        "@sqlite.Text(@encoding.encode(@encoding.UTF8, " +
        expr +
        ".to_string()))"
    }
  }
}

///|
impl BackendCodeGen for SqliteJsBackend with column_read_expr(
  _self,
  col,
  idx,
  _stmt_var,
) {
  let type_name = col.type_.name.to_upper()
  let idx_str = idx.to_string()
  let is_nullable = not(col.not_null)

  // Use high-level Statement::column which returns SqlValue
  let base_expr = match type_name {
    "INTEGER" | "INT" | "INT4" => "stmt.column_int(" + idx_str + ").to_int64()"
    "INT2" | "SMALLINT" => "stmt.column_int(" + idx_str + ")"
    "INT8" | "BIGINT" => "stmt.column_int(" + idx_str + ").to_int64()"
    "REAL" | "FLOAT" | "DOUBLE" | "NUMERIC" | "DECIMAL" =>
      "(match stmt.column(" +
      idx_str +
      ") { @sqlite.Double(v) => v; @sqlite.Int(v) => v.to_double(); _ => 0.0 })"
    "TEXT" | "VARCHAR" | "CHAR" | "CHARACTER" | "CLOB" =>
      "bytes_to_string(stmt.column_text(" + idx_str + "))"
    "BLOB" | "BYTEA" => "stmt.column_text(" + idx_str + ")"
    "BOOLEAN" | "BOOL" => "stmt.column_int(" + idx_str + ") != 0"
    _ => "bytes_to_string(stmt.column_text(" + idx_str + "))"
  }

  // Wrap in Option if nullable
  if is_nullable {
    "(match stmt.column(" +
    idx_str +
    ") { @sqlite.Null => None; _ => Some(" +
    base_expr +
    ") })"
  } else {
    base_expr
  }
}

///|
impl BackendCodeGen for SqliteJsBackend with generate_one(
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

  // Prepare statement
  buf.write_string("  let stmt = match db.prepare(")
  buf.write_string(sql_const)
  buf.write_string(") {\n")
  buf.write_string("    Some(s) => s\n")
  buf.write_string("    None => raise SqlError(bytes_to_string(db.errmsg()))\n")
  buf.write_string("  }\n")

  // Bind parameters
  sqlite_js_write_param_bindings(self, buf, query)

  // Execute and read result
  buf.write_string("  if stmt.step() {\n")
  buf.write_string("    let row : ")
  buf.write_string(row_type)
  buf.write_string(" = {\n")
  for i, col in query.columns {
    buf.write_string("      ")
    buf.write_string(to_snake_case(col.name))
    buf.write_string(": ")
    buf.write_string(self.column_read_expr(col, i, "stmt"))
    buf.write_string(",\n")
  }
  buf.write_string("    }\n")
  buf.write_string("    stmt.finalize()\n")
  buf.write_string("    Some(row)\n")
  buf.write_string("  } else {\n")
  buf.write_string("    stmt.finalize()\n")
  buf.write_string("    None\n")
  buf.write_string("  }\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for SqliteJsBackend with generate_many(
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

  // Prepare statement
  buf.write_string("  let stmt = match db.prepare(")
  buf.write_string(sql_const)
  buf.write_string(") {\n")
  buf.write_string("    Some(s) => s\n")
  buf.write_string("    None => raise SqlError(bytes_to_string(db.errmsg()))\n")
  buf.write_string("  }\n")

  // Bind parameters
  sqlite_js_write_param_bindings(self, buf, query)

  // Execute and collect results
  buf.write_string("  let results : Array[")
  buf.write_string(row_type)
  buf.write_string("] = []\n")
  buf.write_string("  while stmt.step() {\n")
  buf.write_string("    let row : ")
  buf.write_string(row_type)
  buf.write_string(" = {\n")
  for i, col in query.columns {
    buf.write_string("      ")
    buf.write_string(to_snake_case(col.name))
    buf.write_string(": ")
    buf.write_string(self.column_read_expr(col, i, "stmt"))
    buf.write_string(",\n")
  }
  buf.write_string("    }\n")
  buf.write_string("    results.push(row)\n")
  buf.write_string("  }\n")
  buf.write_string("  stmt.finalize()\n")
  buf.write_string("  results\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for SqliteJsBackend with generate_exec(
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

  // Prepare statement
  buf.write_string("  let stmt = match db.prepare(")
  buf.write_string(sql_const)
  buf.write_string(") {\n")
  buf.write_string("    Some(s) => s\n")
  buf.write_string("    None => raise SqlError(bytes_to_string(db.errmsg()))\n")
  buf.write_string("  }\n")

  // Bind parameters
  sqlite_js_write_param_bindings(self, buf, query)

  // Execute
  buf.write_string("  stmt.execute() |> ignore\n")
  buf.write_string("  stmt.finalize()\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for SqliteJsBackend with generate_execrows(
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

  // Prepare statement
  buf.write_string("  let stmt = match db.prepare(")
  buf.write_string(sql_const)
  buf.write_string(") {\n")
  buf.write_string("    Some(s) => s\n")
  buf.write_string("    None => raise SqlError(bytes_to_string(db.errmsg()))\n")
  buf.write_string("  }\n")

  // Bind parameters
  sqlite_js_write_param_bindings(self, buf, query)

  // Execute and return changes
  buf.write_string("  stmt.execute() |> ignore\n")
  buf.write_string("  stmt.finalize()\n")
  buf.write_string("  db.changes()\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for SqliteJsBackend with generate_execlastid(
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

  // Prepare statement
  buf.write_string("  let stmt = match db.prepare(")
  buf.write_string(sql_const)
  buf.write_string(") {\n")
  buf.write_string("    Some(s) => s\n")
  buf.write_string("    None => raise SqlError(bytes_to_string(db.errmsg()))\n")
  buf.write_string("  }\n")

  // Bind parameters
  sqlite_js_write_param_bindings(self, buf, query)

  // Execute and return last insert rowid
  buf.write_string("  stmt.execute() |> ignore\n")
  buf.write_string("  stmt.finalize()\n")
  buf.write_string("  db.last_insert_rowid()\n")
  buf.write_string("}\n\n")
}

// Helper function to write parameter bindings for SQLite JS

///|
fn sqlite_js_write_param_bindings(
  backend : SqliteJsBackend,
  buf : StringBuilder,
  query : @plugin.Query,
) -> Unit {
  if query.params.length() > 0 {
    for i, param in query.params {
      let col = param.column
      let param_name = backend_get_param_name(col, param.number)
      buf.write_string("  stmt.bind(")
      buf.write_string((i + 1).to_string())
      buf.write_string(", ")
      buf.write_string(
        backend.param_bind_expr(col, i + 1, "params." + param_name),
      )
      buf.write_string(") |> ignore\n")
    }
  }
}
