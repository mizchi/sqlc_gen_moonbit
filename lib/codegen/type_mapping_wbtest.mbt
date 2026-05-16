///|
fn make_column(type_name : String, not_null : Bool) -> @plugin.Column {
  let col = @plugin.Column::default()
  col.name = "id"
  col.not_null = not_null
  col.type_ = @plugin.Identifier::new("", "", type_name)
  col
}

///|
test "INTEGER mapping uses Int64 conversions" {
  let col = make_column("INTEGER", true)
  assert_eq(sql_type_to_moonbit(col, []), "Int64")
  let pg_expr = postgres_column_from_row(col, 0)
  assert_true(pg_expr.contains("parse_int64"))
  let pg_js_expr = postgres_js_column_from_any(col, "row")
  assert_true(pg_js_expr.contains("js_number_to_int64"))
  let mysql_js_expr = mysql_js_column_from_any(col, "row")
  assert_true(mysql_js_expr.contains("js_number_to_int64"))
  let sqlite_js_expr = sqlite_js_column_from_sql_value(col, 0)
  assert_true(sqlite_js_expr.contains("to_int64"))
  let d1_expr = d1_column_from_core_any(col, "row")
  assert_true(d1_expr.contains("js_number_to_int64"))
  assert_true(!d1_expr.contains("to_int("))
}

///|
test "emit_sql_doc_lines preserves newlines from query.sql" {
  let buf = StringBuilder::new()
  let sql =
    #|SELECT id, name
    #|FROM users
    #|WHERE id = ?
  emit_sql_doc_lines(buf, sql)
  let out = buf.to_string()
  let expected =
    #|/// SELECT id, name
    #|/// FROM users
    #|/// WHERE id = ?
    #|
  assert_eq(out, expected)
}

///|
test "emit_sql_doc_lines normalises CRLF and drops trailing blank lines" {
  let buf = StringBuilder::new()
  emit_sql_doc_lines(buf, "SELECT 1\r\nFROM t\r\nWHERE x = ?\r\n\r\n")
  let out = buf.to_string()
  let expected =
    #|/// SELECT 1
    #|/// FROM t
    #|/// WHERE x = ?
    #|
  assert_eq(out, expected)
}

///|
test "emit_sql_doc_lines trims trailing whitespace per line" {
  let buf = StringBuilder::new()
  emit_sql_doc_lines(buf, "SELECT id   \nFROM users\t \nWHERE id = ?")
  let out = buf.to_string()
  let expected =
    #|/// SELECT id
    #|/// FROM users
    #|/// WHERE id = ?
    #|
  assert_eq(out, expected)
}
