// Postgres Integration Tests (MoonBit)

///|
fn get_postgres_url() -> String {
  let url = @postgres.pg_get_env("POSTGRES_TEST_URL")
  if url.length() == 0 {
    @postgres.pg_get_env("DATABASE_URL")
  } else {
    url
  }
}

///|
fn connect_postgres() -> Result[@postgres.Connection, String] {
  let url = get_postgres_url()
  if url.length() == 0 {
    return Err(
      "POSTGRES_TEST_URL or DATABASE_URL is required for postgres tests.",
    )
  }
  match @postgres.connect(url) {
    Ok(conn) => Ok(conn)
    Err(e) => Err("Postgres connect failed: " + e.to_string())
  }
}

///|
fn exec_sql(
  conn : @postgres.Connection,
  sql : String,
) -> Result[Unit, @postgres.PgError] {
  match conn.query(sql) {
    Ok(result) => {
      result.free()
      Ok(())
    }
    Err(e) => Err(e)
  }
}

///|
fn setup_schema(conn : @postgres.Connection) -> Result[Unit, @postgres.PgError] {
  match exec_sql(conn, "DROP TABLE IF EXISTS post_tags;") {
    Err(e) => return Err(e)
    Ok(_) => ()
  }
  match exec_sql(conn, "DROP TABLE IF EXISTS tags;") {
    Err(e) => return Err(e)
    Ok(_) => ()
  }
  match exec_sql(conn, "DROP TABLE IF EXISTS posts;") {
    Err(e) => return Err(e)
    Ok(_) => ()
  }
  match exec_sql(conn, "DROP TABLE IF EXISTS users;") {
    Err(e) => return Err(e)
    Ok(_) => ()
  }
  match
    exec_sql(
      conn,
      "CREATE TABLE users (" +
      "id SERIAL PRIMARY KEY," +
      "name VARCHAR(255) NOT NULL," +
      "email VARCHAR(255) NOT NULL UNIQUE," +
      "age INTEGER," +
      "balance NUMERIC(10,2) DEFAULT 0.0," +
      "is_active BOOLEAN NOT NULL DEFAULT TRUE," +
      "bio TEXT," +
      "created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP" +
      ")",
    ) {
    Err(e) => return Err(e)
    Ok(_) => ()
  }
  match
    exec_sql(
      conn,
      "CREATE TABLE posts (" +
      "id SERIAL PRIMARY KEY," +
      "author_id INTEGER NOT NULL REFERENCES users(id)," +
      "title VARCHAR(255) NOT NULL," +
      "content TEXT NOT NULL," +
      "view_count INTEGER NOT NULL DEFAULT 0," +
      "is_published BOOLEAN NOT NULL DEFAULT FALSE," +
      "published_at TIMESTAMP," +
      "created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP" +
      ")",
    ) {
    Err(e) => return Err(e)
    Ok(_) => ()
  }
  match
    exec_sql(
      conn,
      "CREATE TABLE tags (" +
      "id SERIAL PRIMARY KEY," +
      "name VARCHAR(100) NOT NULL UNIQUE" +
      ")",
    ) {
    Err(e) => return Err(e)
    Ok(_) => ()
  }
  match
    exec_sql(
      conn,
      "CREATE TABLE post_tags (" +
      "post_id INTEGER NOT NULL REFERENCES posts(id) ON DELETE CASCADE," +
      "tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE," +
      "PRIMARY KEY (post_id, tag_id)" +
      ")",
    ) {
    Err(e) => return Err(e)
    Ok(_) => ()
  }
  Ok(())
}

///|
test "postgres: create and get user" {
  match connect_postgres() {
    Err(msg) => {
      println(msg)
      assert_true(false)
    }
    Ok(conn) => {
      match setup_schema(conn) {
        Ok(_) => ()
        Err(e) => {
          println("Postgres schema setup failed: " + e.to_string())
          assert_true(false)
        }
      }
      match
        @gen.create_user_returning_id(
          conn,
          @gen.CreateUserReturningIdParams::new(
            "Alice",
            "alice@example.com",
            Some("25"),
            Some("100.5"),
            "true",
            Some("MoonBit developer"),
          ),
        ) {
        Ok(id) => assert_eq(id, 1L)
        Err(e) => {
          println("Postgres insert failed: " + e.to_string())
          assert_true(false)
        }
      }
      match
        @gen.get_user_by_email(
          conn,
          @gen.GetUserByEmailParams::new("alice@example.com"),
        ) {
        Ok(Some(user)) => {
          assert_eq(user.name, "Alice")
          assert_eq(user.email, "alice@example.com")
          assert_eq(user.bio, Some("MoonBit developer"))
        }
        Ok(None) => assert_true(false)
        Err(e) => {
          println("Postgres query failed: " + e.to_string())
          assert_true(false)
        }
      }
      conn.close()
    }
  }
}
