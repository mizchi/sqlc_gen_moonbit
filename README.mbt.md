# sqlc-gen-moonbit

> **⚠️ Experimental**: This project is experimental and under active development. APIs may change without notice.

[sqlc](https://sqlc.dev/) plugin for generating type-safe MoonBit code from SQL.

## Features

- Generates type-safe MoonBit structs from SQL schemas
- Generates query functions with proper parameter binding
- Supports SQLite (native) with `mizchi/sqlite` binding
- Supports Cloudflare D1 with `mizchi/cloudflare` binding
- Supports `:one`, `:many`, `:exec` query types
- Optional validators and JSON Schema generation

## Installation

### WASM Plugin (Recommended)

Add to your `sqlc.yaml`:

```yaml
version: "2"
plugins:
  - name: moonbit
    wasm:
      url: "https://raw.githubusercontent.com/mizchi/sqlc_gen_moonbit/main/bin/sqlc-gen-moonbit.wasm"
      sha256: "c97e4c6198c13f3244456cf481f77143c3a9b5a86c382019ff378b2729c76615"
sql:
  - engine: sqlite
    schema: "schema.sql"
    queries: "query.sql"
    codegen:
      - plugin: moonbit
        out: "gen"
        options:
          backend: "sqlite"  # or "d1" for Cloudflare D1
```

### Local WASM File

Download `bin/sqlc-gen-moonbit.wasm` and use local path:

```yaml
plugins:
  - name: moonbit
    wasm:
      url: "file://./sqlc-gen-moonbit.wasm"
      sha256: ""  # Optional for local files
```

### Build from Source

```bash
# Native plugin
just build-plugin

# WASM plugin
just build-plugin-wasm
```

## Usage

### 1. Define your schema

`db/schema.sql`:

```sql
CREATE TABLE users (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE
);
```

### 2. Write queries with sqlc annotations

`db/query.sql`:

```sql
-- name: GetUser :one
SELECT * FROM users WHERE id = ?;

-- name: ListUsers :many
SELECT * FROM users ORDER BY name;

-- name: CreateUser :exec
INSERT INTO users (name, email) VALUES (?, ?);
```

### 3. Generate code

```bash
sqlc generate
```

### 4. Use generated code

**For SQLite (native):**

Add dependencies to `moon.mod.json`:

```json
{
  "deps": {
    "mizchi/sqlite": "0.1.3",
    "moonbitlang/x": "0.4.38"
  }
}
```

```moonbit
fn main {
  let db = @sqlite.sqlite_open_v2(...)

  // Create user
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com"))

  // List users
  let users = @gen.list_users(db)

  // Get user by ID
  match @gen.get_user(db, @gen.GetUserParams::new(1L)) {
    Some(user) => println("Found: " + user.name)
    None => println("Not found")
  }
}
```

**For Cloudflare D1:**

Add dependencies to `moon.mod.json`:

```json
{
  "deps": {
    "mizchi/cloudflare": "0.0.13",
    "mizchi/js": "0.6.4"
  }
}
```

```moonbit
pub async fn handler(db : @cloudflare.D1Database) -> Unit {
  // Create user
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com"))

  // List users (async)
  let users = @gen.list_users(db)

  // Get user by ID (async)
  match @gen.get_user(db, @gen.GetUserParams::new(1L)) {
    Some(user) => println("Found: " + user.name)
    None => println("Not found")
  }
}
```

## Plugin Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `backend` | `"sqlite"` \| `"d1"` | `"sqlite"` | Target backend |
| `validators` | `bool` | `false` | Generate validation functions |
| `json_schema` | `bool` | `false` | Generate JSON Schema |

Example with all options:

```yaml
codegen:
  - plugin: moonbit
    out: "db/gen"
    options:
      backend: "d1"
      validators: true
      json_schema: true
```

## Generated Code

For each query, sqlc-gen-moonbit generates:

- **Param structs**: `GetUserParams`, `CreateUserParams` with `::new()` constructor
- **Row structs**: `GetUserRow`, `ListUsersRow` with fields matching SELECT columns
- **Query functions**: `get_user()`, `list_users()`, `create_user()` with type-safe parameters
- **Validators** (optional): `GetUserParams::validate()` returning `Result[Unit, String]`
- **JSON Schema** (optional): `sqlc_schema.json` with type definitions

## Query Types

| Annotation | Return Type | Description |
|------------|-------------|-------------|
| `:one`     | `T?`        | Returns single row or None |
| `:many`    | `Array[T]`  | Returns all matching rows |
| `:exec`    | `Unit`      | Executes without returning data |

## Examples

- [`examples/sqlite-native`](./examples/sqlite-native) - SQLite with native binding
- [`examples/d1`](./examples/d1) - Cloudflare D1 with Atlas migrations

## Development

```bash
just --list              # Show available commands
just build-plugin        # Build native plugin
just build-plugin-wasm   # Build WASM plugin
just generate            # Generate code for examples
just test                # Run all tests
```

## License

Apache-2.0
