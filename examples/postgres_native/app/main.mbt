// Example: PostgreSQL basic usage with sqlc-gen-moonbit
// Note: This is a placeholder. Actual usage requires a PostgreSQL connection.

///|
fn main {
  println("PostgreSQL example with sqlc-gen-moonbit")
  println("Generated types and queries are in db/gen/")

  // Example usage (requires PostgreSQL connection):
  // let config = @postgres.ConnectionConfig::default()
  // config.host = "localhost"
  // config.database = "mydb"
  // config.user = "myuser"
  // config.password = Some("mypassword")
  //
  // let conn = @postgres.Connection::new(config)
  // conn.connect()
  //
  // let users = @gen.list_users(conn)
  // for user in users {
  //   println("User: " + user.name)
  // }
}
