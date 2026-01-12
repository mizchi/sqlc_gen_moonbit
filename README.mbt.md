# sqlc-gen-moonbit

> **⚠️ Experimental**: This project is experimental and under active development. APIs may change without notice.

[sqlc](https://sqlc.dev/) plugin for generating type-safe MoonBit code from SQL.

## Features

- Generates type-safe MoonBit structs from SQL schemas
- Generates query functions with proper parameter binding
- Supports SQLite with `mizchi/sqlite` binding
- Supports `:one`, `:many`, `:exec` query types

## Installation

Build the plugin from source:

```bash
moon build --target native --release
```

The binary will be at `target/native/release/build/cmd/main/main.exe`.

## Usage

### 1. Create `sqlc.yaml`

```yaml
version: "2"
plugins:
  - name: moonbit
    process:
      cmd: "path/to/main.exe"
sql:
  - engine: sqlite
    schema: "sqlite/schema.sql"
    queries: "sqlite/query.sql"
    codegen:
      - plugin: moonbit
        out: "gen"
```

### 2. Define your schema

`sqlite/schema.sql`:

```sql
CREATE TABLE users (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE
);
```

### 3. Write queries with sqlc annotations

`sqlite/query.sql`:

```sql
-- name: GetUser :one
SELECT * FROM users WHERE id = ?;

-- name: ListUsers :many
SELECT * FROM users ORDER BY name;

-- name: CreateUser :exec
INSERT INTO users (name, email) VALUES (?, ?);
```

### 4. Generate code

```bash
sqlc generate
```

### 5. Use generated code

Add dependencies to `moon.mod.json`:

```json
{
  "deps": {
    "mizchi/sqlite": "0.1.3",
    "moonbitlang/x": "0.4.38"
  }
}
```

Import and use in your MoonBit code:

```moonbit
fn main {
  let db = @sqlite.sqlite_open_v2(
    cstring(":memory:"),
    @sqlite.SQLITE_OPEN_READWRITE | @sqlite.SQLITE_OPEN_CREATE | @sqlite.SQLITE_OPEN_MEMORY,
    Bytes::new(0)
  )

  // Create user
  let params = @gen.CreateUserParams::new("Alice", "alice@example.com")
  @gen.create_user(db, params)

  // List users
  let users = @gen.list_users(db)
  for user in users {
    println("name=" + user.name + " email=" + user.email)
  }

  // Get user by ID
  match @gen.get_user(db, @gen.GetUserParams::new(1L)) {
    Some(user) => println("Found: " + user.name)
    None => println("Not found")
  }
}
```

## Generated Code

For each query, sqlc-gen-moonbit generates:

- **Param structs**: `GetUserParams`, `CreateUserParams` with `::new()` constructor
- **Row structs**: `GetUserRow`, `ListUsersRow` with fields matching SELECT columns
- **Query functions**: `get_user()`, `list_users()`, `create_user()` with type-safe parameters

## Query Types

| Annotation | Return Type | Description |
|------------|-------------|-------------|
| `:one`     | `T?`        | Returns single row or None |
| `:many`    | `Array[T]`  | Returns all matching rows |
| `:exec`    | `Unit`      | Executes without returning data |

## License

Apache-2.0
