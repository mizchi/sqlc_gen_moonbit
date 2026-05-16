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
test "d1 non-nullable column reads route through d1_require_field" {
  let text_col = make_column("TEXT", true)
  text_col.name = "slug"
  let text_expr = d1_column_from_core_any(text_col, "row")
  assert_true(text_expr.contains("d1_require_field(row, \"slug\")"))
  assert_true(text_expr.contains(".cast()"))
  let int_col = make_column("INTEGER", true)
  int_col.name = "id"
  let int_expr = d1_column_from_core_any(int_col, "row")
  assert_true(int_expr.contains("d1_require_field(row, \"id\")"))
  assert_true(int_expr.contains("js_number_to_int64"))
}

///|
test "d1 nullable column reads keep the explicit null/undefined guard" {
  let col = make_column("TEXT", false)
  col.name = "summary"
  let expr = d1_column_from_core_any(col, "row")
  // Nullable path keeps its own inline guard and returns Option, so
  // it must not call d1_require_field (which would raise instead of
  // returning None for NULL cells).
  assert_true(!expr.contains("d1_require_field"))
  assert_true(expr.contains("@core.is_null"))
  assert_true(expr.contains("None"))
}

///|
test "d1 generated module emits d1_require_field helper exactly once" {
  let request = @plugin.GenerateRequest::default()
  let col = make_column("TEXT", true)
  col.name = "slug"
  let query = @plugin.Query::default()
  query.name = "GetSlug"
  query.cmd = ":one"
  query.text = "SELECT slug FROM pages WHERE id = ?"
  query.columns = [col]
  request.queries = [query]
  let out = generate_queries_d1(request, [])
  let mut count = 0
  let needle = "fn d1_require_field("
  let mut i = 0
  while i + needle.length() <= out.length() {
    if out.view(start_offset=i, end_offset=i + needle.length()) == needle.view() {
      count = count + 1
      i = i + needle.length()
    } else {
      i = i + 1
    }
  }
  assert_eq(count, 1)
  // And the generated query function actually calls the helper.
  assert_true(out.contains("d1_require_field(result, \"slug\")"))
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
