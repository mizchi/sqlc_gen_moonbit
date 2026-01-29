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
  assert_true(not(d1_expr.contains("to_int(")))
}
