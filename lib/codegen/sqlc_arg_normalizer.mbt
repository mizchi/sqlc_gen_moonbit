///|
/// Plugin-side fixup for sqlc.arg() / aggregate inference gaps that sqlc can
/// leave in SQLite/D1 requests. sqlc usually rewrites named args to numbered
/// placeholders and includes them in Query.params, but some contexts arrive
/// with raw sqlc.arg('name') text and no matching param.
priv struct SqlcArgOccurrence {
  name : String
  start : Int
  end : Int
}

///|
fn normalize_sqlc_args_in_request(
  request : @plugin.GenerateRequest,
  backend : Backend,
) -> Unit {
  for i = 0; i < request.queries.length(); i = i + 1 {
    let query = request.queries[i]
    normalize_sqlc_args_in_query(request.catalog, query, backend)
    request.queries[i] = query
  }
}

///|
fn normalize_sqlc_args_in_query(
  catalog : @plugin.Catalog,
  query : @plugin.Query,
  backend : Backend,
) -> Unit {
  normalize_aggregate_result_columns(catalog, query)
  let occurrences = find_sqlc_arg_occurrences(query.text)
  if occurrences.length() == 0 {
    return
  }
  if backend is MysqlJs {
    abort(
      "unsupported: sqlc.arg() was left in query " +
      query.name +
      " for mysql_js. Use positional ? parameters or split the query.",
    )
  }

  let mut max_number = 0
  for param in query.params {
    if param.number > max_number {
      max_number = param.number
    }
  }

  for occurrence in occurrences {
    match find_param_number_by_name(query, occurrence.name) {
      Some(_) => ()
      None => {
        let source_col = match
          infer_sqlc_arg_source_column(catalog, query, occurrence) {
          Some(col) => col
          None =>
            abort(
              "unsupported: could not infer type for sqlc.arg('" +
              occurrence.name +
              "') in query " +
              query.name +
              ". Use a positional ? parameter or add an explicit CAST.",
            )
        }
        max_number = max_number + 1
        query.params.push(@plugin.Parameter::{
          number: max_number,
          column: synthetic_named_param_column(occurrence.name, source_col),
        })
      }
    }
  }
  query.text = rewrite_sqlc_arg_occurrences(
    query.text,
    occurrences,
    query,
    backend,
  )
}

///|
fn infer_sqlc_arg_source_column(
  catalog : @plugin.Catalog,
  query : @plugin.Query,
  occurrence : SqlcArgOccurrence,
) -> @plugin.Column? {
  match infer_assignment_column_name(query.text, occurrence.start) {
    Some(target_col_name) =>
      match
        find_catalog_column(
          catalog,
          query.insert_into_table.name,
          target_col_name,
        ) {
        Some(col) => return Some(col)
        None => ()
      }
    None => ()
  }
  infer_aggregate_arg_column(catalog, query.text, occurrence.start)
}

///|
fn normalize_aggregate_result_columns(
  catalog : @plugin.Catalog,
  query : @plugin.Query,
) -> Unit {
  for i = 0; i < query.columns.length(); i = i + 1 {
    let col = query.columns[i]
    if col.type_.name.to_upper() == "ANY" {
      match infer_coalesce_sum_column(catalog, query.text, col.name) {
        Some(source) => {
          col.type_ = source.type_
          // COALESCE(SUM(...), 0) is non-null even when SUM() alone would
          // be nullable for an empty group.
          col.not_null = true
          query.columns[i] = col
        }
        None => ()
      }
    }
  }
}

///|
fn find_sqlc_arg_occurrences(text : String) -> Array[SqlcArgOccurrence] {
  let occurrences : Array[SqlcArgOccurrence] = []
  let needle = "sqlc.arg("
  let mut i = 0
  while i + needle.length() <= text.length() {
    if string_matches_at(text, i, needle) {
      match parse_sqlc_arg_at(text, i) {
        Some(occurrence) => {
          occurrences.push(occurrence)
          i = occurrence.end
        }
        None => i = i + 1
      }
    } else {
      i = i + 1
    }
  }
  occurrences
}

///|
fn parse_sqlc_arg_at(text : String, start : Int) -> SqlcArgOccurrence? {
  let mut i = start + "sqlc.arg(".length()
  while i < text.length() && is_ascii_space(text[i].to_int().unsafe_to_char()) {
    i = i + 1
  }
  if i >= text.length() {
    return None
  }
  let quote = text[i]
  if quote != '\'' && quote != '"' {
    return None
  }
  i = i + 1
  let name_start = i
  while i < text.length() && text[i] != quote {
    i = i + 1
  }
  if i >= text.length() {
    return None
  }
  let name = substring(text, name_start, i)
  i = i + 1
  while i < text.length() && is_ascii_space(text[i].to_int().unsafe_to_char()) {
    i = i + 1
  }
  if i >= text.length() || text[i] != ')' {
    return None
  }
  Some(SqlcArgOccurrence::{ name, start, end: i + 1, })
}

///|
fn rewrite_sqlc_arg_occurrences(
  text : String,
  occurrences : Array[SqlcArgOccurrence],
  query : @plugin.Query,
  backend : Backend,
) -> String {
  let buf = StringBuilder()
  let mut last = 0
  for occurrence in occurrences {
    for i = last; i < occurrence.start; i = i + 1 {
      buf.write_char(text[i].to_int().unsafe_to_char())
    }
    let number = match find_param_number_by_name(query, occurrence.name) {
      Some(n) => n
      None =>
        abort(
          "internal error: missing synthetic sqlc.arg param '" +
          occurrence.name +
          "' in query " +
          query.name,
        )
    }
    buf.write_string(sqlc_arg_placeholder(backend, number))
    last = occurrence.end
  }
  for i = last; i < text.length(); i = i + 1 {
    buf.write_char(text[i].to_int().unsafe_to_char())
  }
  buf.to_string()
}

///|
fn sqlc_arg_placeholder(backend : Backend, number : Int) -> String {
  match backend {
    Sqlite | SqliteJs | D1 => "?" + number.to_string()
    Postgres | PostgresJs => "$" + number.to_string()
    MysqlJs =>
      abort("internal error: mysql_js sqlc.arg placeholder is unsupported")
  }
}

///|
fn find_param_number_by_name(query : @plugin.Query, name : String) -> Int? {
  for param in query.params {
    if param.column.name == name {
      return Some(param.number)
    }
  }
  None
}

///|
fn infer_assignment_column_name(text : String, arg_start : Int) -> String? {
  let mut eq = arg_start - 1
  while eq >= 0 && text[eq] != '=' {
    eq = eq - 1
  }
  if eq < 0 {
    return None
  }
  let mut end = eq - 1
  while end >= 0 && is_ascii_space(text[end].to_int().unsafe_to_char()) {
    end = end - 1
  }
  if end < 0 {
    return None
  }
  let mut start = end
  while start >= 0 &&
        is_sql_identifier_char(text[start].to_int().unsafe_to_char()) {
    start = start - 1
  }
  let raw = substring(text, start + 1, end + 1)
  let name = unqualify_identifier(raw)
  if name.length() == 0 {
    None
  } else {
    Some(name)
  }
}

///|
fn unqualify_identifier(raw : String) -> String {
  let mut dot = -1
  for i = 0; i < raw.length(); i = i + 1 {
    if raw[i] == '.' {
      dot = i
    }
  }
  let base = substring(raw, dot + 1, raw.length())
  if base.length() >= 2 && base[0] == '"' && base[base.length() - 1] == '"' {
    substring(base, 1, base.length() - 1)
  } else {
    base
  }
}

///|
fn find_catalog_column(
  catalog : @plugin.Catalog,
  table_name : String,
  column_name : String,
) -> @plugin.Column? {
  for schema in catalog.schemas {
    for table in schema.tables {
      if table_name.length() == 0 || table.rel.name == table_name {
        for col in table.columns {
          if col.name == column_name || col.original_name == column_name {
            return Some(col)
          }
        }
      }
    }
  }
  None
}

///|
fn infer_aggregate_arg_column(
  catalog : @plugin.Catalog,
  text : String,
  arg_start : Int,
) -> @plugin.Column? {
  match comparison_lhs_before_arg(text, arg_start) {
    Some(lhs) => {
      let upper = lhs.to_upper()
      if upper.contains("COUNT(") {
        return Some(synthetic_type_source_column("INTEGER"))
      }
      if upper.contains("SUM(") {
        match infer_sum_source_column(catalog, text, arg_start) {
          Some(col) => return Some(col)
          None => ()
        }
      }
      None
    }
    None => None
  }
}

///|
fn comparison_lhs_before_arg(text : String, arg_start : Int) -> String? {
  let mut i = arg_start - 1
  while i >= 0 && is_ascii_space(text[i].to_int().unsafe_to_char()) {
    i = i - 1
  }
  if i < 0 {
    return None
  }
  let op_start = if text[i] == '=' {
    if i > 0 && (text[i - 1] == '>' || text[i - 1] == '<' || text[i - 1] == '!') {
      i - 1
    } else {
      i
    }
  } else if text[i] == '>' || text[i] == '<' {
    i
  } else {
    return None
  }
  let mut start = op_start - 1
  while start >= 0 && text[start] != '\n' {
    start = start - 1
  }
  Some(substring(text, start + 1, op_start))
}

///|
fn infer_coalesce_sum_column(
  catalog : @plugin.Catalog,
  text : String,
  alias_name : String,
) -> @plugin.Column? {
  let upper = text.to_upper()
  let alias_marker = " AS " + alias_name.to_upper()
  match find_substring(upper, alias_marker) {
    Some(alias_idx) =>
      match find_last_substring_before(upper, "SUM(", alias_idx) {
        Some(sum_idx) =>
          match find_last_substring_before(upper, "COALESCE(", sum_idx) {
            Some(coalesce_idx) =>
              if is_only_ascii_space_between(
                  text,
                  coalesce_idx + "COALESCE(".length(),
                  sum_idx,
                ) {
                match
                  parse_function_identifier_arg(text, sum_idx + "SUM(".length()) {
                  Some(col_name) =>
                    match find_catalog_column(catalog, "", col_name) {
                      Some(col) => Some(col)
                      None => None
                    }
                  None => None
                }
              } else {
                None
              }
            None => None
          }
        None => None
      }
    None => None
  }
}

///|
fn infer_sum_source_column(
  catalog : @plugin.Catalog,
  text : String,
  before : Int,
) -> @plugin.Column? {
  let upper = text.to_upper()
  match find_last_substring_before(upper, "SUM(", before) {
    Some(sum_idx) =>
      match parse_function_identifier_arg(text, sum_idx + "SUM(".length()) {
        Some(col_name) =>
          match find_catalog_column(catalog, "", col_name) {
            Some(col) => Some(col)
            None => None
          }
        None => None
      }
    None => None
  }
}

///|
fn parse_function_identifier_arg(text : String, start : Int) -> String? {
  let mut i = start
  while i < text.length() && is_ascii_space(text[i].to_int().unsafe_to_char()) {
    i = i + 1
  }
  let arg_start = i
  while i < text.length() &&
        is_sql_identifier_char(text[i].to_int().unsafe_to_char()) {
    i = i + 1
  }
  if i == arg_start {
    None
  } else {
    Some(unqualify_identifier(substring(text, arg_start, i)))
  }
}

///|
fn find_last_substring_before(
  haystack : String,
  needle : String,
  before : Int,
) -> Int? {
  if needle.length() == 0 || before <= 0 {
    return None
  }
  let mut last = -1
  let limit = if before > haystack.length() {
    haystack.length()
  } else {
    before
  }
  let mut i = 0
  while i + needle.length() <= limit {
    if string_matches_at(haystack, i, needle) {
      last = i
      i = i + needle.length()
    } else {
      i = i + 1
    }
  }
  if last >= 0 {
    Some(last)
  } else {
    None
  }
}

///|
fn is_only_ascii_space_between(text : String, start : Int, end : Int) -> Bool {
  if start > end {
    return false
  }
  let mut i = start
  while i < end {
    if !is_ascii_space(text[i].to_int().unsafe_to_char()) {
      return false
    }
    i = i + 1
  }
  true
}

///|
fn synthetic_named_param_column(
  name : String,
  source : @plugin.Column,
) -> @plugin.Column {
  @plugin.Column::{
    name,
    not_null: source.not_null,
    is_array: false,
    comment: "",
    length: source.length,
    is_named_param: true,
    is_func_call: false,
    scope: "",
    table: source.table,
    table_alias: "",
    type_: source.type_,
    is_sqlc_slice: false,
    embed_table: @plugin.Identifier::default(),
    original_name: source.name,
    unsigned: source.unsigned,
    array_dims: 0,
  }
}

///|
fn synthetic_type_source_column(type_name : String) -> @plugin.Column {
  @plugin.Column::{
    name: "",
    not_null: true,
    is_array: false,
    comment: "",
    length: -1,
    is_named_param: false,
    is_func_call: true,
    scope: "",
    table: @plugin.Identifier::default(),
    table_alias: "",
    type_: @plugin.Identifier::{ catalog: "", schema: "", name: type_name, },
    is_sqlc_slice: false,
    embed_table: @plugin.Identifier::default(),
    original_name: "",
    unsigned: false,
    array_dims: 0,
  }
}

///|
fn string_matches_at(text : String, start : Int, needle : String) -> Bool {
  if start < 0 || start + needle.length() > text.length() {
    return false
  }
  for i = 0; i < needle.length(); i = i + 1 {
    if text[start + i] != needle[i] {
      return false
    }
  }
  true
}

///|
fn is_ascii_space(c : Char) -> Bool {
  c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

///|
fn is_ascii_alpha(c : Char) -> Bool {
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

///|
fn is_ascii_digit(c : Char) -> Bool {
  c >= '0' && c <= '9'
}

///|
fn is_sql_identifier_char(c : Char) -> Bool {
  is_ascii_alpha(c) || is_ascii_digit(c) || c == '_' || c == '.' || c == '"'
}
