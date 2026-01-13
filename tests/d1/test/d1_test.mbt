// D1 Integration Tests (MoonBit)
// Run with: moon test --target js
// These tests use @cloudflare.Miniflare directly

///|
fn get_schema_sql() -> String {
  "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT NOT NULL UNIQUE, email TEXT NOT NULL UNIQUE, display_name TEXT NOT NULL, bio TEXT, avatar_url TEXT, created_at TEXT NOT NULL DEFAULT (datetime('now')), updated_at TEXT NOT NULL DEFAULT (datetime('now')));"
}

///|
async test "D1: create and get user" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")
  let _ = db.exec(get_schema_sql())
  @gen.create_user(
    db,
    @gen.CreateUserParams::new(
      "alice",
      "alice@example.com",
      "Alice Developer",
      Some("MoonBit enthusiast"),
      None,
    ),
  )
  let user = @gen.get_user_by_username(
    db,
    @gen.GetUserByUsernameParams::new("alice"),
  )
  assert_true(user is Some(_))
  let u = user.unwrap()
  assert_eq(u.username, "alice")
  assert_eq(u.email, "alice@example.com")
  assert_eq(u.display_name, "Alice Developer")
  assert_eq(u.bio, Some("MoonBit enthusiast"))
  mf.dispose()
}
