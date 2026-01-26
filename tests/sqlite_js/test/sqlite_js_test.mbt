// SQLite JS Integration Tests

///|
fn bytes_to_string(b : Bytes) -> String {
  let decoder = @encoding.decoder(@encoding.UTF8)
  decoder.decode_lossy(b[0:b.length()])
}

///|
fn open_memory_db() -> @sqlite.Database {
  @sqlite.Database::open(":memory:").unwrap()
}

///|
fn exec_sql(db : @sqlite.Database, sql : String) -> Unit {
  db.exec(sql) |> ignore
}

///|
fn create_schema(db : @sqlite.Database) -> Unit {
  exec_sql(db, "CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, email TEXT NOT NULL UNIQUE, age INTEGER, balance REAL DEFAULT 0.0, is_active INTEGER NOT NULL DEFAULT 1, bio TEXT, created_at TEXT NOT NULL DEFAULT (datetime('now')))")
  exec_sql(db, "CREATE TABLE posts (id INTEGER PRIMARY KEY AUTOINCREMENT, author_id INTEGER NOT NULL REFERENCES users(id), title TEXT NOT NULL, content TEXT NOT NULL, view_count INTEGER NOT NULL DEFAULT 0, is_published INTEGER NOT NULL DEFAULT 0, published_at TEXT, created_at TEXT NOT NULL DEFAULT (datetime('now')))")
  exec_sql(db, "CREATE TABLE tags (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE)")
  exec_sql(db, "CREATE TABLE post_tags (post_id INTEGER NOT NULL, tag_id INTEGER NOT NULL, PRIMARY KEY (post_id, tag_id))")
}

// ============================================
// Basic User CRUD Tests
// ============================================

///|
test "create and get user by id" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(
    db,
    @gen.CreateUserParams::new("Alice", "alice@example.com", Some(25), Some(100.5), 1, Some("Developer")),
  )
  let user = @gen.get_user_by_id(db, @gen.GetUserByIdParams::new(1))
  assert_true(user is Some(_))
  let u = user.unwrap()
  assert_eq(u.name, "Alice")
  assert_eq(u.email, "alice@example.com")
  assert_eq(u.age, Some(25))
  assert_eq(u.bio, Some("Developer"))
  db.close()
}

///|
test "create and get user by email" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(
    db,
    @gen.CreateUserParams::new("Bob", "bob@example.com", None, None, 1, None),
  )
  let user = @gen.get_user_by_email(db, @gen.GetUserByEmailParams::new("bob@example.com"))
  assert_true(user is Some(_))
  assert_eq(user.unwrap().name, "Bob")
  db.close()
}

///|
test "list users ordered by name" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Charlie", "charlie@example.com", None, None, 1, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Bob", "bob@example.com", None, None, 1, None))
  let users = @gen.list_users(db)
  assert_eq(users.length(), 3)
  assert_eq(users[0].name, "Alice")
  assert_eq(users[1].name, "Bob")
  assert_eq(users[2].name, "Charlie")
  db.close()
}

///|
test "get non-existent user returns None" {
  let db = open_memory_db()
  create_schema(db)
  let user = @gen.get_user_by_id(db, @gen.GetUserByIdParams::new(999))
  assert_true(user is None)
  db.close()
}

///|
test "create user returning id (:execlastid)" {
  let db = open_memory_db()
  create_schema(db)
  let id1 = @gen.create_user_returning_id(
    db,
    @gen.CreateUserReturningIdParams::new("Alice", "alice@example.com", None, None, 1, None),
  )
  assert_eq(id1, 1L)
  let id2 = @gen.create_user_returning_id(
    db,
    @gen.CreateUserReturningIdParams::new("Bob", "bob@example.com", None, None, 1, None),
  )
  assert_eq(id2, 2L)
  db.close()
}

///|
test "delete user by id (:execrows)" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1, None))
  let deleted = @gen.delete_user_by_id(db, @gen.DeleteUserByIdParams::new(1))
  assert_eq(deleted, 1)
  let user = @gen.get_user_by_id(db, @gen.GetUserByIdParams::new(1))
  assert_true(user is None)
  // Delete non-existent
  let deleted2 = @gen.delete_user_by_id(db, @gen.DeleteUserByIdParams::new(999))
  assert_eq(deleted2, 0)
  db.close()
}
