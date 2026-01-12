// SQLite Native Integration Tests

///|
fn cstring(s : String) -> Bytes {
  @encoding.encode(@encoding.UTF8, s)
}

///|
fn open_memory_db() -> @sqlite.Sqlite3 {
  @sqlite.sqlite_open_v2(
    cstring(":memory:"),
    @sqlite.SQLITE_OPEN_READWRITE | @sqlite.SQLITE_OPEN_CREATE | @sqlite.SQLITE_OPEN_MEMORY,
    Bytes::new(0),
  )
}

///|
fn create_schema(db : @sqlite.Sqlite3) -> Unit {
  let sql = "CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, email TEXT NOT NULL UNIQUE)"
  let stmt = @sqlite.sqlite_prepare(db, cstring(sql))
  @sqlite.sqlite_step(stmt) |> ignore
  @sqlite.sqlite_finalize(stmt)
}

///|
test "create and get user" {
  let db = open_memory_db()
  create_schema(db)

  // Create user
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com"))

  // Get user by ID
  let user = @gen.get_user(db, @gen.GetUserParams::new(1L))
  assert_true(user is Some(_))
  let u = user.unwrap()
  assert_eq(u.id, 1L)
  assert_eq(u.name, "Alice")
  assert_eq(u.email, "alice@example.com")
}

///|
test "list users" {
  let db = open_memory_db()
  create_schema(db)

  // Create multiple users
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com"))
  @gen.create_user(db, @gen.CreateUserParams::new("Bob", "bob@example.com"))
  @gen.create_user(db, @gen.CreateUserParams::new("Charlie", "charlie@example.com"))

  // List all users (ordered by name)
  let users = @gen.list_users(db)
  assert_eq(users.length(), 3)
  assert_eq(users[0].name, "Alice")
  assert_eq(users[1].name, "Bob")
  assert_eq(users[2].name, "Charlie")
}

///|
test "get non-existent user returns None" {
  let db = open_memory_db()
  create_schema(db)

  let user = @gen.get_user(db, @gen.GetUserParams::new(999L))
  assert_true(user is None)
}

///|
test "list users empty table" {
  let db = open_memory_db()
  create_schema(db)

  let users = @gen.list_users(db)
  assert_eq(users.length(), 0)
}
