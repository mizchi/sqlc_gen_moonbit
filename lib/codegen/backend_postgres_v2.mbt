// PostgreSQL backend (native target, Result-based error handling) - Trait implementation

///|
pub struct PostgresBackend {
  _placeholder : Unit
}

///|
pub fn PostgresBackend::new() -> PostgresBackend {
  { _placeholder: () }
}

///|
impl BackendCodeGen for PostgresBackend with info(_self) {
  BackendInfo::new(
    "postgres",
    "@postgres.Connection",
    Some("@postgres.PgError"),
    true, // uses Result
    false,
    "native",
  )
}

///|
impl BackendCodeGen for PostgresBackend with generate_helpers(_self, buf) {
  buf.write_string("// Requires: mattn/postgres\n\n")
}

///|
impl BackendCodeGen for PostgresBackend with param_bind_expr(
  _self,
  col,
  _idx,
  expr,
) {
  let type_name = col.type_.name.to_upper()
  let is_nullable = not(col.not_null)

  // Check type category using contains matching
  let is_string_type = type_name.contains("TEXT") ||
    type_name.contains("VARCHAR") ||
    type_name.contains("CHAR") ||
    type_name.contains("CHARACTER") ||
    type_name.contains("BPCHAR") ||
    type_name.contains("CLOB")
  let is_int_type = type_name == "INTEGER" ||
    type_name == "INT" ||
    type_name == "INT4" ||
    type_name == "INT2" ||
    type_name == "SMALLINT"
  let is_bigint_type = type_name == "INT8" ||
    type_name == "BIGINT" ||
    type_name.contains("SERIAL")
  let is_float_type = type_name.contains("REAL") ||
    type_name.contains("FLOAT") ||
    type_name.contains("DOUBLE") ||
    type_name.contains("NUMERIC") ||
    type_name.contains("DECIMAL")
  let is_bool_type = type_name.contains("BOOL")
  if is_nullable {
    // For nullable types, handle Option -> convert to Value or Null
    if is_string_type ||
      is_int_type ||
      is_bigint_type ||
      is_float_type ||
      is_bool_type {
      "(match " +
      expr +
      " { Some(v) => @postgres.ToValue::to_value(v); None => @postgres.Value::Null })"
    } else {
      "(match " +
      expr +
      " { Some(v) => @postgres.ToValue::to_value(v.to_string()); None => @postgres.Value::Null })"
    }
  } else if is_string_type ||
    is_int_type ||
    is_bigint_type ||
    is_float_type ||
    is_bool_type {
    "@postgres.ToValue::to_value(" + expr + ")"
  } else {
    "@postgres.ToValue::to_value(" + expr + ".to_string())"
  }
}

///|
impl BackendCodeGen for PostgresBackend with column_read_expr(
  _self,
  col,
  idx,
  _row_var,
) {
  let type_name = col.type_.name.to_upper()
  let idx_str = idx.to_string()
  let is_nullable = not(col.not_null)

  // mattn/postgres returns Array[Array[String]], so we access row[idx]
  let conversion = match type_name {
    "INTEGER" | "INT" | "INT4" =>
      "(try { @strconv.parse_int64(row[" + idx_str + "]) } catch { _ => 0L })"
    "INT2" | "SMALLINT" =>
      "(try { @strconv.parse_int(row[" + idx_str + "]) } catch { _ => 0 })"
    "INT8" | "BIGINT" | "SERIAL" | "BIGSERIAL" =>
      "(try { @strconv.parse_int64(row[" + idx_str + "]) } catch { _ => 0L })"
    "REAL" | "FLOAT4" =>
      "(try { @strconv.parse_double(row[" + idx_str + "]) } catch { _ => 0.0 })"
    "DOUBLE PRECISION" | "FLOAT8" | "FLOAT" | "NUMERIC" | "DECIMAL" =>
      "(try { @strconv.parse_double(row[" + idx_str + "]) } catch { _ => 0.0 })"
    "TEXT"
    | "VARCHAR"
    | "CHAR"
    | "CHARACTER"
    | "CHARACTER VARYING"
    | "BPCHAR" => "row[" + idx_str + "]"
    "BOOLEAN" | "BOOL" =>
      "(row[" + idx_str + "] == \"t\" || row[" + idx_str + "] == \"true\")"
    "TIMESTAMP"
    | "TIMESTAMPTZ"
    | "TIMESTAMP WITHOUT TIME ZONE"
    | "TIMESTAMP WITH TIME ZONE" => "row[" + idx_str + "]"
    "DATE" => "row[" + idx_str + "]"
    "TIME" | "TIMETZ" | "TIME WITHOUT TIME ZONE" | "TIME WITH TIME ZONE" =>
      "row[" + idx_str + "]"
    "UUID" => "row[" + idx_str + "]"
    "JSONB" | "JSON" => "row[" + idx_str + "]"
    "BYTEA" => "row[" + idx_str + "]"
    "INET" | "CIDR" | "MACADDR" | "MACADDR8" => "row[" + idx_str + "]"
    _ => "row[" + idx_str + "]"
  }

  // Wrap in Option if nullable
  if is_nullable {
    "(if row[" +
    idx_str +
    "].length() == 0 { None } else { Some(" +
    conversion +
    ") })"
  } else {
    conversion
  }
}

///|
impl BackendCodeGen for PostgresBackend with generate_one(
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

  // Execute query with parameters
  postgres_write_query_execution(buf, query, sql_const)
  buf.write_string("  match result {\n")
  buf.write_string("    Err(e) => Err(e)\n")
  buf.write_string("    Ok(qr) => {\n")
  buf.write_string("      let rows = qr.rows()\n")
  buf.write_string("      qr.free()\n")
  buf.write_string("      if rows.length() == 0 {\n")
  buf.write_string("        Ok(None)\n")
  buf.write_string("      } else {\n")
  buf.write_string("        let row = rows[0]\n")
  buf.write_string("        Ok(Some({\n")
  for i, col in query.columns {
    buf.write_string("          ")
    buf.write_string(to_snake_case(col.name))
    buf.write_string(": ")
    buf.write_string(self.column_read_expr(col, i, "row"))
    buf.write_string(",\n")
  }
  buf.write_string("        }))\n")
  buf.write_string("      }\n")
  buf.write_string("    }\n")
  buf.write_string("  }\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for PostgresBackend with generate_many(
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

  // Execute query with parameters
  postgres_write_query_execution(buf, query, sql_const)
  buf.write_string("  match result {\n")
  buf.write_string("    Err(e) => Err(e)\n")
  buf.write_string("    Ok(qr) => {\n")
  buf.write_string("      let rows = qr.rows()\n")
  buf.write_string("      qr.free()\n")
  buf.write_string("      let results : Array[")
  buf.write_string(row_type)
  buf.write_string("] = []\n")
  buf.write_string("      for row in rows {\n")
  buf.write_string("        results.push({\n")
  for i, col in query.columns {
    buf.write_string("          ")
    buf.write_string(to_snake_case(col.name))
    buf.write_string(": ")
    buf.write_string(self.column_read_expr(col, i, "row"))
    buf.write_string(",\n")
  }
  buf.write_string("        })\n")
  buf.write_string("      }\n")
  buf.write_string("      Ok(results)\n")
  buf.write_string("    }\n")
  buf.write_string("  }\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for PostgresBackend with generate_exec(
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

  // Execute query with parameters
  postgres_write_query_execution(buf, query, sql_const)
  buf.write_string("  match result {\n")
  buf.write_string("    Err(e) => Err(e)\n")
  buf.write_string("    Ok(qr) => {\n")
  buf.write_string("      qr.free()\n")
  buf.write_string("      Ok(())\n")
  buf.write_string("    }\n")
  buf.write_string("  }\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for PostgresBackend with generate_execrows(
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

  // Execute query with parameters
  postgres_write_query_execution(buf, query, sql_const)
  buf.write_string("  match result {\n")
  buf.write_string("    Err(e) => Err(e)\n")
  buf.write_string("    Ok(qr) => {\n")
  buf.write_string("      let count = qr.affected_rows()\n")
  buf.write_string("      qr.free()\n")
  buf.write_string("      Ok(count)\n")
  buf.write_string("    }\n")
  buf.write_string("  }\n")
  buf.write_string("}\n\n")
}

///|
impl BackendCodeGen for PostgresBackend with generate_execlastid(
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

  // Execute query with parameters
  postgres_write_query_execution(buf, query, sql_const)
  buf.write_string("  match result {\n")
  buf.write_string("    Err(e) => Err(e)\n")
  buf.write_string("    Ok(qr) => {\n")
  buf.write_string(
    "      // For INSERT RETURNING, get the returned id from first row\n",
  )
  buf.write_string("      let rows = qr.rows()\n")
  buf.write_string("      qr.free()\n")
  buf.write_string("      if rows.length() > 0 && rows[0].length() > 0 {\n")
  buf.write_string(
    "        Ok(try { @strconv.parse_int64(rows[0][0]) } catch { _ => 0L })\n",
  )
  buf.write_string("      } else {\n")
  buf.write_string("        Ok(0L)\n")
  buf.write_string("      }\n")
  buf.write_string("    }\n")
  buf.write_string("  }\n")
  buf.write_string("}\n\n")
}

// Helper function to write query execution for PostgreSQL

///|
fn postgres_write_query_execution(
  buf : StringBuilder,
  query : @plugin.Query,
  sql_const : String,
) -> Unit {
  if query.params.length() > 0 {
    buf.write_string("  let params : Array[@postgres.Value] = [\n")
    for i, param in query.params {
      let col = param.column
      let param_name = backend_get_param_name(col, param.number)
      buf.write_string("    ")
      buf.write_string(
        PostgresBackend::new().param_bind_expr(
          col,
          i + 1,
          "params." + param_name,
        ),
      )
      if i < query.params.length() - 1 {
        buf.write_string(",")
      }
      buf.write_string("\n")
    }
    buf.write_string("  ]\n")
    buf.write_string("  let result = conn.execute(")
    buf.write_string(sql_const)
    buf.write_string(", params)\n")
  } else {
    buf.write_string("  let result = conn.query(")
    buf.write_string(sql_const)
    buf.write_string(")\n")
  }
}
