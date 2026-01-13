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
  // Open in-memory database
  let db = @sqlite.sqlite_open_v2(
    cstring(":memory:"),
    @sqlite.SQLITE_OPEN_READWRITE | @sqlite.SQLITE_OPEN_CREATE | @sqlite.SQLITE_OPEN_MEMORY,
    Bytes::new(0),
  )

  // Create schema
  exec_sql(db, "CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, email TEXT NOT NULL UNIQUE, created_at TEXT NOT NULL DEFAULT (datetime('now')))")
  println("Created users table")

  // Create users
  let id1 = @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com"))
  let id2 = @gen.create_user(db, @gen.CreateUserParams::new("Bob", "bob@example.com"))
  println("Created users: id=" + id1.to_string() + ", " + id2.to_string())

  // List all users
  let users = @gen.list_users(db)
  println("Users (" + users.length().to_string() + "):")
  for user in users {
    println("  " + user.id.to_string() + ": " + user.name + " <" + user.email + ">")
  }

  // Get single user
  match @gen.get_user(db, @gen.GetUserParams::new(1L)) {
    Some(user) => println("Found: " + user.name)
    None => println("Not found")
  }

  // Update user
  let updated = @gen.update_user(db, @gen.UpdateUserParams::new("Alice Smith", "alice.smith@example.com", 1L))
  println("Updated " + updated.to_string() + " row(s)")

  // Delete user
  let deleted = @gen.delete_user(db, @gen.DeleteUserParams::new(2L))
  println("Deleted " + deleted.to_string() + " row(s)")

  // Final count
  let final_users = @gen.list_users(db)
  println("Final user count: " + final_users.length().to_string())
}
