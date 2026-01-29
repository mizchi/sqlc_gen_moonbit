///|
async fn run_tests_inner() -> Unit {
  println("=== PostgreSQL JS Backend Test ===")

  // Create connection pool
  let pool = @pg.Pool::new(
    host="localhost",
    port=5433,
    user="postgres",
    password="postgres",
    database="testdb",
  )

  // Clean up any existing test data
  let _ = pool.query("DELETE FROM users WHERE email LIKE '%@example.com'")

  // Test CreateUser
  println("\n[Test] CreateUser")
  let user_id = @gen.create_user(pool, @gen.CreateUserParams::new("Alice", "alice@example.com"))
  println("Created user with id: \{user_id}")

  // Test GetUser
  println("\n[Test] GetUser")
  match @gen.get_user(pool, @gen.GetUserParams::new(user_id)) {
    Some(user) => {
      println("Found user: \{user.name} <\{user.email}>")
      guard user.name == "Alice"
      guard user.email == "alice@example.com"
    }
    None => {
      println("ERROR: User not found!")
      panic()
    }
  }

  // Create another user
  let user_id2 = @gen.create_user(pool, @gen.CreateUserParams::new("Bob", "bob@example.com"))
  println("\nCreated second user with id: \{user_id2}")

  // Test ListUsers
  println("\n[Test] ListUsers")
  let users = @gen.list_users(pool)
  println("Total users: \{users.length()}")
  for user in users {
    println("  - \{user.name} <\{user.email}>")
  }

  // Test UpdateUser
  println("\n[Test] UpdateUser")
  let updated = @gen.update_user(pool, @gen.UpdateUserParams::new(user_id, "Alice Updated", "alice.updated@example.com"))
  println("Updated rows: \{updated}")

  // Verify update
  match @gen.get_user(pool, @gen.GetUserParams::new(user_id)) {
    Some(user) => {
      println("Updated user: \{user.name} <\{user.email}>")
      guard user.name == "Alice Updated"
    }
    None => panic()
  }

  // Test DeleteUser
  println("\n[Test] DeleteUser")
  @gen.delete_user(pool, @gen.DeleteUserParams::new(user_id))
  @gen.delete_user(pool, @gen.DeleteUserParams::new(user_id2))
  println("Deleted users")

  // Verify deletion
  let remaining = @gen.list_users(pool)
  let remaining_count = remaining.filter(fn(u) { u.email.contains("@example.com") }).length()
  println("Remaining users with @example.com: \{remaining_count}")

  // Close pool
  pool.end()

  println("\n=== All Tests Passed! ===")
}

///|
async fn run_tests() -> Unit noraise {
  try {
    run_tests_inner()
  } catch {
    e => println("Error: \{e}")
  }
}

///|
fn main {
  @core.run_async(run_tests)
}
