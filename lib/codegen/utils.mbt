// Utility functions for code generation

///|
pub fn to_pascal_case(s : String) -> String {
  let buf = StringBuilder::new()
  let mut capitalize_next = true
  for c in s {
    if c == '_' || c == '-' {
      capitalize_next = true
    } else if capitalize_next {
      buf.write_char(to_upper_char(c))
      capitalize_next = false
    } else {
      buf.write_char(c)
    }
  }
  buf.to_string()
}

///|
pub fn to_snake_case(s : String) -> String {
  let buf = StringBuilder::new()
  let mut prev_lower = false
  for c in s {
    if is_upper(c) {
      if prev_lower {
        buf.write_char('_')
      }
      buf.write_char(to_lower_char(c))
      prev_lower = false
    } else {
      buf.write_char(c)
      prev_lower = is_lower(c)
    }
  }
  buf.to_string()
}

///|
fn to_upper_char(c : Char) -> Char {
  if c >= 'a' && c <= 'z' {
    (c.to_int() - 32).unsafe_to_char()
  } else {
    c
  }
}

///|
fn to_lower_char(c : Char) -> Char {
  if c >= 'A' && c <= 'Z' {
    (c.to_int() + 32).unsafe_to_char()
  } else {
    c
  }
}

///|
fn is_upper(c : Char) -> Bool {
  c >= 'A' && c <= 'Z'
}

///|
fn is_lower(c : Char) -> Bool {
  c >= 'a' && c <= 'z'
}

///|
pub fn sanitize_newlines(s : String) -> String {
  let buf = StringBuilder::new()
  for c in s {
    match c {
      '\n' | '\r' => buf.write_char(' ')
      _ => buf.write_char(c)
    }
  }
  buf.to_string()
}

///|
/// Emit a multi-line SQL string as a sequence of `/// ` doc-comment
/// lines, preserving newlines from `query.sql`. Each line has its
/// trailing whitespace trimmed so the generator output stays
/// formatter-stable regardless of how the source SQL was indented.
///
/// `\r\n` and bare `\r` are normalised to `\n`. Empty trailing lines
/// (caused by a final newline in `query.sql`) are dropped so the
/// emitted block doesn't end with a dangling `/// `.
pub fn emit_sql_doc_lines(buf : StringBuilder, sql : String) -> Unit {
  // Normalise CR/CRLF → LF first so the split below sees one shape.
  let normalised = StringBuilder::new()
  let mut prev_cr = false
  for c in sql {
    match c {
      '\r' => {
        normalised.write_char('\n')
        prev_cr = true
      }
      '\n' => {
        if !prev_cr {
          normalised.write_char('\n')
        }
        prev_cr = false
      }
      _ => {
        normalised.write_char(c)
        prev_cr = false
      }
    }
  }
  let text = normalised.to_string()
  // Collect into an array so we can drop trailing-empty entries.
  let lines : Array[String] = []
  for view in text.split("\n") {
    lines.push(view.to_owned())
  }
  while lines.length() > 0 &&
        lines[lines.length() - 1].view().trim(chars=" \t").length() == 0 {
    let _ = lines.pop()
  }
  for line in lines {
    let trimmed = line.view().trim_end(chars=" \t")
    buf.write_string("/// ")
    buf.write_string(trimmed.to_owned())
    buf.write_string("\n")
  }
}

///|
pub fn escape_string(s : String) -> String {
  let buf = StringBuilder::new()
  buf.write_char('"')
  for c in s {
    match c {
      '"' => buf.write_string("\\\"")
      '\\' => buf.write_string("\\\\")
      '\n' => buf.write_string("\\n")
      '\r' => buf.write_string("\\r")
      '\t' => buf.write_string("\\t")
      _ => buf.write_char(c)
    }
  }
  buf.write_char('"')
  buf.to_string()
}

///|
pub fn string_to_utf8(s : String) -> Bytes {
  let arr : Array[Byte] = []
  for c in s {
    let code = c.to_uint()
    if code < 0x80U {
      arr.push(code.to_byte())
    } else if code < 0x800U {
      arr.push((0xC0U | (code >> 6)).to_byte())
      arr.push((0x80U | (code & 0x3FU)).to_byte())
    } else if code < 0x10000U {
      arr.push((0xE0U | (code >> 12)).to_byte())
      arr.push((0x80U | ((code >> 6) & 0x3FU)).to_byte())
      arr.push((0x80U | (code & 0x3FU)).to_byte())
    } else {
      arr.push((0xF0U | (code >> 18)).to_byte())
      arr.push((0x80U | ((code >> 12) & 0x3FU)).to_byte())
      arr.push((0x80U | ((code >> 6) & 0x3FU)).to_byte())
      arr.push((0x80U | (code & 0x3FU)).to_byte())
    }
  }
  Bytes::from_iter(arr.iter())
}

///|
pub fn bytes_to_string_simple(b : Bytes) -> String {
  let buf = StringBuilder::new()
  for i = 0; i < b.length(); i = i + 1 {
    let byte = b[i]
    if byte >= 32 && byte < 127 {
      buf.write_char(byte.to_int().unsafe_to_char())
    }
  }
  buf.to_string()
}

///|
pub fn find_substring(haystack : String, needle : String) -> Int? {
  let h_len = haystack.length()
  let n_len = needle.length()
  if n_len > h_len {
    return None
  }
  for i = 0; i <= h_len - n_len; i = i + 1 {
    let mut found = true
    for j = 0; j < n_len; j = j + 1 {
      if haystack[i + j] != needle[j] {
        found = false
        break
      }
    }
    if found {
      return Some(i)
    }
  }
  None
}

///|
/// Validate that a generated MoonBit source string contains no C0 control
/// characters other than `\t` (U+0009) and `\n` (U+000A). The wasm-host
/// boundary in sqlc has been observed to insert stray `\x1f` (unit
/// separator) bytes into the output, producing source like `id\x1f: String`
/// or `pub\x1ffn` that won't compile (issue #17). This pass catches that
/// class of corruption before the bytes leave the plugin, so the user sees
/// a clear "regen produced corrupted output, please rerun" message instead
/// of an inscrutable lex error in the generated file.
///
/// Returns `Some(reason)` on the first violation found, `None` if clean.
/// Single-character substitution corruption (e.g. `String` -> `Strhng`)
/// is not detected here — those produce valid ASCII and need a full
/// MoonBit lexer to catch.
pub fn validate_no_control_chars(
  filename : String,
  content : String,
) -> String? {
  for i = 0; i < content.length(); i = i + 1 {
    let c = content[i].to_int()
    // Allow \t (9), \n (10), \r (13); reject other C0 controls (0-31).
    if c < 32 && c != 9 && c != 10 && c != 13 {
      // Find the start of the line containing the offending char.
      let mut line = 1
      let mut last_nl = -1
      for j = 0; j < i; j = j + 1 {
        if content[j].to_int() == 10 {
          line = line + 1
          last_nl = j
        }
      }
      let col = i - last_nl
      return Some(
        filename +
        ":" +
        line.to_string() +
        ":" +
        col.to_string() +
        ": control character U+" +
        c.to_string() +
        " in generated output. This is a known sporadic codegen corruption " +
        "(issue #17). Rerun `sqlc generate`; if the corruption reproduces, " +
        "report it with the surrounding bytes.",
      )
    }
  }
  None
}

///|
/// Count non-overlapping occurrences of `needle` in `haystack`. Used by the
/// D1 backend to detect when the same `sqlc.slice('x')` appears in a query
/// at more than one position — a configuration the current bind-loop does
/// not support (see issue #20).
pub fn count_substring(haystack : String, needle : String) -> Int {
  let h_len = haystack.length()
  let n_len = needle.length()
  if n_len == 0 || n_len > h_len {
    return 0
  }
  let mut count = 0
  let mut i = 0
  while i <= h_len - n_len {
    let mut found = true
    for j = 0; j < n_len; j = j + 1 {
      if haystack[i + j] != needle[j] {
        found = false
        break
      }
    }
    if found {
      count = count + 1
      i = i + n_len
    } else {
      i = i + 1
    }
  }
  count
}

///|
pub fn substring(s : String, start : Int, end : Int) -> String {
  let buf = StringBuilder::new()
  for i = start; i < end && i < s.length(); i = i + 1 {
    buf.write_char(s[i].to_int().unsafe_to_char())
  }
  buf.to_string()
}
