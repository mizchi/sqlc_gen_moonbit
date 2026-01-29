// SQLite backend (native target, low-level API) - Trait implementation

///|
pub struct SqliteBackend {
  _placeholder : Unit
}

///|
pub fn SqliteBackend::new() -> SqliteBackend {
  { _placeholder: () }
}

///|
impl BackendCodeGen for SqliteBackend with info(_self) {
  BackendInfo::new(
    "sqlite",
    "@sqlite.Sqlite3",
    Some("SqlError"),
    false,
    false,
    "native",
  )
}

///|
impl BackendCodeGen for SqliteBackend with generate_helpers(_self, buf) {
  // cstring helper
  buf.write_string("///|\nfn cstring(s : String) -> Bytes {\n")
  buf.write_string("  @encoding.encode(@encoding.UTF8, s)\n")
  buf.write_string("}\n\n")

  // bytes_to_string helper
  buf.write_string("///|\nfn bytes_to_string(b : Bytes) -> String {\n")
  buf.write_string("  let decoder = @encoding.decoder(@encoding.UTF8)\n")
  buf.write_string("  decoder.decode_lossy(b[0:b.length()])\n")
  buf.write_string("}\n\n")

  // check_sqlite_error helper
  buf.write_string(
    "///|\nfn check_sqlite_error(db : @sqlite.Sqlite3, rc : Int) -> Unit raise SqlError {\n",
  )
  buf.write_string(
    "  if rc != @sqlite.SQLITE_ROW && rc != @sqlite.SQLITE_DONE {\n",
  )
  buf.write_string("    let msg = bytes_to_string(@sqlite.sqlite_errmsg(db))\n")
  buf.write_string("    raise SqlError(msg)\n")
  buf.write_string("  }\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for SqliteBackend with param_bind_expr(
  _self,
  col,
  idx,
  expr,
) {
  let type_name = col.type_.name.to_upper()
  let idx_str = idx.to_string()
  let is_nullable = not(col.not_null)

  // Get the base bind call expression
  let base_bind = fn(val_expr : String) -> String {
    match type_name {
      "INTEGER" | "INT" | "INT4" | "SMALLINT" =>
        "@sqlite.sqlite_bind_int64(stmt, " + idx_str + ", " + val_expr + ")"
      "INT8" | "BIGINT" =>
        "@sqlite.sqlite_bind_int64(stmt, " + idx_str + ", " + val_expr + ")"
      "REAL" | "FLOAT" | "DOUBLE" | "NUMERIC" | "DECIMAL" =>
        "@sqlite.sqlite_bind_double(stmt, " + idx_str + ", " + val_expr + ")"
      "TEXT" | "VARCHAR" | "CHAR" | "CHARACTER" | "CLOB" =>
        "@sqlite.sqlite_bind_text(stmt, " +
        idx_str +
        ", cstring(" +
        val_expr +
        "))"
      "BLOB" | "BYTEA" =>
        "@sqlite.sqlite_bind_blob(stmt, " + idx_str + ", " + val_expr + ")"
      "BOOLEAN" | "BOOL" =>
        "@sqlite.sqlite_bind_int(stmt, " +
        idx_str +
        ", if " +
        val_expr +
        " { 1 } else { 0 })"
      _ =>
        "@sqlite.sqlite_bind_text(stmt, " +
        idx_str +
        ", cstring(" +
        val_expr +
        "))"
    }
  }

  // Handle nullable parameters with match
  if is_nullable {
    "(match " +
    expr +
    " { None => @sqlite.sqlite_bind_null(stmt, " +
    idx_str +
    "); Some(v) => " +
    base_bind("v") +
    " })"
  } else {
    base_bind(expr)
  }
}

///|
impl BackendCodeGen for SqliteBackend with column_read_expr(
  _self,
  col,
  idx,
  _stmt_var,
) {
  let type_name = col.type_.name.to_upper()
  let idx_str = idx.to_string()
  let is_nullable = not(col.not_null)

  // Get the base value expression
  let base_expr = match type_name {
    "INTEGER" | "INT" | "INT4" | "INT8" | "BIGINT" | "SMALLINT" =>
      "@sqlite.sqlite_column_int64(stmt, " + idx_str + ")"
    "REAL" | "FLOAT" | "DOUBLE" | "NUMERIC" | "DECIMAL" =>
      "@sqlite.sqlite_column_double(stmt, " + idx_str + ")"
    "TEXT" | "VARCHAR" | "CHAR" | "CHARACTER" | "CLOB" =>
      "bytes_to_string(@sqlite.sqlite_column_text(stmt, " + idx_str + "))"
    "BLOB" | "BYTEA" => "@sqlite.sqlite_column_blob(stmt, " + idx_str + ")"
    "BOOLEAN" | "BOOL" =>
      "@sqlite.sqlite_column_int(stmt, " + idx_str + ") != 0"
    _ => "bytes_to_string(@sqlite.sqlite_column_text(stmt, " + idx_str + "))"
  }

  // Wrap in Option if nullable
  if is_nullable {
    "(if @sqlite.sqlite_column_type(stmt, " +
    idx_str +
    ") == @sqlite.SQLITE_NULL { None } else { Some(" +
    base_expr +
    ") })"
  } else {
    base_expr
  }
}

///|
impl BackendCodeGen for SqliteBackend with generate_one(
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
  buf.write_string("  let stmt = @sqlite.sqlite_prepare(db, cstring(")
  buf.write_string(sql_const)
  buf.write_string("))\n")

  // Bind parameters
  sqlite_write_param_bindings(self, buf, query)

  // Execute and read result
  buf.write_string("  let rc = @sqlite.sqlite_step(stmt)\n")
  buf.write_string("  if rc == @sqlite.SQLITE_ROW {\n")
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
  buf.write_string("    @sqlite.sqlite_finalize(stmt)\n")
  buf.write_string("    Some(row)\n")
  buf.write_string("  } else if rc == @sqlite.SQLITE_DONE {\n")
  buf.write_string("    @sqlite.sqlite_finalize(stmt)\n")
  buf.write_string("    None\n")
  buf.write_string("  } else {\n")
  buf.write_string("    @sqlite.sqlite_finalize(stmt)\n")
  buf.write_string("    check_sqlite_error(db, rc)\n")
  buf.write_string("    None\n")
  buf.write_string("  }\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for SqliteBackend with generate_many(
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
  buf.write_string("  let stmt = @sqlite.sqlite_prepare(db, cstring(")
  buf.write_string(sql_const)
  buf.write_string("))\n")

  // Bind parameters
  sqlite_write_param_bindings(self, buf, query)

  // Execute and collect results
  buf.write_string("  let results : Array[")
  buf.write_string(row_type)
  buf.write_string("] = []\n")
  buf.write_string("  while true {\n")
  buf.write_string("    let rc = @sqlite.sqlite_step(stmt)\n")
  buf.write_string("    if rc == @sqlite.SQLITE_ROW {\n")
  buf.write_string("      let row : ")
  buf.write_string(row_type)
  buf.write_string(" = {\n")
  for i, col in query.columns {
    buf.write_string("        ")
    buf.write_string(to_snake_case(col.name))
    buf.write_string(": ")
    buf.write_string(self.column_read_expr(col, i, "stmt"))
    buf.write_string(",\n")
  }
  buf.write_string("      }\n")
  buf.write_string("      results.push(row)\n")
  buf.write_string("    } else if rc == @sqlite.SQLITE_DONE {\n")
  buf.write_string("      break\n")
  buf.write_string("    } else {\n")
  buf.write_string("      @sqlite.sqlite_finalize(stmt)\n")
  buf.write_string("      check_sqlite_error(db, rc)\n")
  buf.write_string("      break\n")
  buf.write_string("    }\n")
  buf.write_string("  }\n")
  buf.write_string("  @sqlite.sqlite_finalize(stmt)\n")
  buf.write_string("  results\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for SqliteBackend with generate_exec(
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
  buf.write_string("  let stmt = @sqlite.sqlite_prepare(db, cstring(")
  buf.write_string(sql_const)
  buf.write_string("))\n")

  // Bind parameters
  sqlite_write_param_bindings(self, buf, query)

  // Execute
  buf.write_string("  let rc = @sqlite.sqlite_step(stmt)\n")
  buf.write_string("  @sqlite.sqlite_finalize(stmt)\n")
  buf.write_string("  check_sqlite_error(db, rc)\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for SqliteBackend with generate_execrows(
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
  buf.write_string("  let stmt = @sqlite.sqlite_prepare(db, cstring(")
  buf.write_string(sql_const)
  buf.write_string("))\n")

  // Bind parameters
  sqlite_write_param_bindings(self, buf, query)

  // Execute and return changes
  buf.write_string("  let rc = @sqlite.sqlite_step(stmt)\n")
  buf.write_string("  @sqlite.sqlite_finalize(stmt)\n")
  buf.write_string("  check_sqlite_error(db, rc)\n")
  buf.write_string("  @sqlite.sqlite_changes(db)\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for SqliteBackend with generate_execlastid(
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
  buf.write_string("  let stmt = @sqlite.sqlite_prepare(db, cstring(")
  buf.write_string(sql_const)
  buf.write_string("))\n")

  // Bind parameters
  sqlite_write_param_bindings(self, buf, query)

  // Execute and return last insert rowid
  buf.write_string("  let rc = @sqlite.sqlite_step(stmt)\n")
  buf.write_string("  @sqlite.sqlite_finalize(stmt)\n")
  buf.write_string("  check_sqlite_error(db, rc)\n")
  buf.write_string("  @sqlite.sqlite_last_insert_rowid(db)\n")
  buf.write_string("}\n\n")
}

// Helper function to write parameter bindings for SQLite

///|
fn sqlite_write_param_bindings(
  backend : SqliteBackend,
  buf : StringBuilder,
  query : @plugin.Query,
) -> Unit {
  if query.params.length() > 0 {
    for i, param in query.params {
      let col = param.column
      let param_name = backend_get_param_name(col, param.number)
      buf.write_string("  ")
      buf.write_string(
        backend.param_bind_expr(col, i + 1, "params." + param_name),
      )
      buf.write_string(" |> ignore\n")
    }
  }
}
