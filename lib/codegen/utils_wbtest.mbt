// Tests for count_substring + validate_no_control_chars helpers added in
// the same change as the slice-double-use + bit-flip self-validation
// guards (issues #17 and #20). abort() paths are not exercised here —
// they would terminate the test runner; they are covered by manual
// reproduction documented in the issues.

///|
test "count_substring counts non-overlapping occurrences" {
  assert_eq(count_substring("aaa", "a"), 3)
  assert_eq(count_substring("abcabcabc", "abc"), 3)
  assert_eq(count_substring("xxxx", "xx"), 2)
  // Non-overlapping: matches advance by needle length.
  assert_eq(count_substring("aaaa", "aa"), 2)
}

///|
test "count_substring handles edge cases" {
  assert_eq(count_substring("", "a"), 0)
  assert_eq(count_substring("a", ""), 0)
  assert_eq(count_substring("abc", "abcd"), 0)
  assert_eq(count_substring("abc", "xyz"), 0)
}

///|
test "count_substring detects slice-marker double-use" {
  // The marker shape that backend_d1.mbt looks for when guarding
  // against the slice double-use issue (#20).
  let text = "SELECT * FROM t WHERE x IN (/*SLICE:ids*/?) OR y IN (/*SLICE:ids*/?)"
  assert_eq(count_substring(text, "/*SLICE:ids*/?"), 2)
}

///|
test "validate_no_control_chars accepts plain ASCII" {
  let clean = "pub fn foo() -> Int {\n  42\n}\n"
  match validate_no_control_chars("test.mbt", clean) {
    None => ()
    Some(msg) => fail("expected None, got Some(\{msg})")
  }
}

///|
test "validate_no_control_chars allows tab and CR" {
  let with_tab = "let a = 1\t// trailing tab\n"
  match validate_no_control_chars("test.mbt", with_tab) {
    None => ()
    Some(msg) => fail("expected None, got Some(\{msg})")
  }
}

///|
test "validate_no_control_chars rejects U+001F" {
  // The exact bit-flip shape mnemo observed: a stray \x1f inserted
  // between `id` and `: String` in a struct field declaration.
  let corrupted = "pub struct Foo {\n  id\u{001f}: String\n}\n"
  match validate_no_control_chars("sqlc_types.mbt", corrupted) {
    Some(msg) =>
      // Message names the file and roughly locates the issue.
      assert_true(msg.contains("sqlc_types.mbt"))
    None => fail("expected validator to flag U+001F")
  }
}

///|
test "validate_no_control_chars rejects U+0001 (any C0 control)" {
  let corrupted = "let \u{0001}x = 1\n"
  match validate_no_control_chars("sqlc_queries.mbt", corrupted) {
    Some(_) => ()
    None => fail("expected validator to flag U+0001")
  }
}
