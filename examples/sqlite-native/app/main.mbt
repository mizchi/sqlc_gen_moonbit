///|
fn cstring(s : String) -> Bytes {
  @encoding.encode(@encoding.UTF8, s)
}

///|
fn exec_sql(db : @sqlite.Sqlite3, sql : String) -> Unit {
  let stmt = @sqlite.sqlite_prepare(db, cstring(sql))
  @sqlite.sqlite_step(stmt) |> ignore
  @sqlite.sqlite_finalize(stmt)
}

///|
fn main {
  // Open database using low-level API
  let db = @sqlite.sqlite_open_v2(
    cstring(":memory:"),
    @sqlite.SQLITE_OPEN_READWRITE | @sqlite.SQLITE_OPEN_CREATE | @sqlite.SQLITE_OPEN_MEMORY,
    Bytes::new(0)
  )

  // Create table with full schema
  exec_sql(db, "CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, email TEXT NOT NULL UNIQUE, age INTEGER, balance REAL DEFAULT 0.0, is_active INTEGER NOT NULL DEFAULT 1, bio TEXT, created_at TEXT NOT NULL DEFAULT (datetime('now')))")
  println("Created users table")

  // Insert users using generated function
  let params1 = @gen.CreateUserParams::new("Alice", "alice@example.com", Some(25L), Some(100.5), 1L, Some("Developer"))
  @gen.create_user(db, params1)
  println("Created user: Alice")

  let params2 = @gen.CreateUserParams::new("Bob", "bob@example.com", None, None, 1L, None)
  @gen.create_user(db, params2)
  println("Created user: Bob")

  // List all users
  let users = @gen.list_users(db)
  println("Users count: " + users.length().to_string())
  for user in users {
    println("  id=" + user.id.to_string() + " name=" + user.name + " email=" + user.email)
  }

  // Get user by ID
  let get_params = @gen.GetUserByIdParams::new(1L)
  match @gen.get_user_by_id(db, get_params) {
    Some(user) => {
      println("Found user: " + user.name)
      println("  age: " + user.age.to_string())
      println("  balance: " + user.balance.to_string())
      println("  bio: " + user.bio.to_string())
    }
    None => println("User not found")
  }

  println("Done!")
}
