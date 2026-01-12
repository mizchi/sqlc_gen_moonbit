///|
fn cstring(s : String) -> Bytes {
  @encoding.encode(@encoding.UTF8, s)
}

///|
fn main {
  // Open database using low-level API
  let db = @sqlite.sqlite_open_v2(
    cstring(":memory:"),
    @sqlite.SQLITE_OPEN_READWRITE | @sqlite.SQLITE_OPEN_CREATE | @sqlite.SQLITE_OPEN_MEMORY,
    Bytes::new(0)
  )

  // Create table
  let create_table_sql = "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT NOT NULL, email TEXT NOT NULL)"
  let stmt = @sqlite.sqlite_prepare(db, cstring(create_table_sql))
  @sqlite.sqlite_step(stmt) |> ignore
  @sqlite.sqlite_finalize(stmt)
  println("Created users table")

  // Insert a user
  let params1 = @gen.CreateUserParams::new("Alice", "alice@example.com")
  @gen.create_user(db, params1)
  println("Created user: Alice")

  let params2 = @gen.CreateUserParams::new("Bob", "bob@example.com")
  @gen.create_user(db, params2)
  println("Created user: Bob")

  // List all users
  let users = @gen.list_users(db)
  println("Users count: " + users.length().to_string())
  for user in users {
    println("  id=" + user.id.to_string() + " name=" + user.name + " email=" + user.email)
  }

  // Get user by ID
  let get_params = @gen.GetUserParams::new(1L)
  match @gen.get_user(db, get_params) {
    Some(user) => println("Found user: " + user.name)
    None => println("User not found")
  }

  println("Done!")
}
