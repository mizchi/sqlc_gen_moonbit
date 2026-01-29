# sqlc-gen-moonbit

[![CI](https://github.com/mizchi/sqlc_gen_moonbit/actions/workflows/ci.yml/badge.svg)](https://github.com/mizchi/sqlc_gen_moonbit/actions/workflows/ci.yml)

> **⚠️ Experimental**: This project is experimental and under active development. APIs may change without notice.

[sqlc](https://sqlc.dev/) plugin for generating type-safe MoonBit code from SQL.

## Supported Backends

| Backend | Target | Runtime | Dependencies |
|---------|--------|---------|--------------|
| `sqlite` | `native` | Native binary | `mizchi/sqlite` |
| `sqlite_js` | `js` | Node.js / Browser | `mizchi/sqlite`, `mizchi/js` |
| `d1` | `js` | Cloudflare Workers | `mizchi/cloudflare`, `mizchi/js` |
| `postgres` | `native` | Native binary | `mattn/postgres` |
| `postgres_js` | `js` | Node.js | `mizchi/npm_typed/pg`, `mizchi/js` |
| `mysql_js` | `js` | Node.js | `mizchi/js` (mysql2 npm package) |

## Features

- Generates type-safe MoonBit structs from SQL schemas
- Generates query functions with proper parameter binding
- Custom type overrides via `overrides` option
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
      url: "https://github.com/mizchi/sqlc_gen_moonbit/releases/download/v0.2.1/sqlc-gen-moonbit.wasm"
      sha256: "26eb1d10957f0f4f0197ab204a4ef5a0c9052f10ac40bdf242ae9e223cb5b820"
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

Check the [releases page](https://github.com/mizchi/sqlc_gen_moonbit/releases) for the latest version and sha256.

### Build from Source

```bash
moon update
moon build --target wasm ./cmd/wasm
# Output: target/wasm/release/build/cmd/wasm/wasm.wasm
```

Use local file in sqlc.yaml:

```yaml
plugins:
  - name: moonbit
    wasm:
      url: "file://./path/to/wasm.wasm"
      sha256: ""  # Optional for local files
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
    "mizchi/cloudflare": "0.1.6",
    "mizchi/js": "0.10.10",
    "moonbitlang/async": "0.16.0"
  }
}
```

```moonbit
///|
pub async fn handler(
  db : @cloudflare.D1Database,
) -> Unit raise @cloudflare.D1Error {
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

**For PostgreSQL (JS target with Node.js):**

Add dependencies to `moon.mod.json`:

```json
{
  "deps": {
    "mizchi/npm_typed": "0.1.2",
    "mizchi/js": "0.10.10",
    "moonbitlang/x": "0.4.38"
  }
}
```

Install npm dependencies:

```bash
npm install pg
```

```moonbit
///|
async fn run_tests_inner() -> Unit {
  let pool = @pg.Pool::new(
    host="localhost",
    port=5432,
    user="postgres",
    password="postgres",
    database="mydb",
  )

  // Create user (async, returns inserted id)
  let user_id = @gen.create_user(
    pool,
    @gen.CreateUserParams::new("Alice", "alice@example.com"),
  )

  // List users (async)
  let users = @gen.list_users(pool)

  // Get user by ID (async)
  match @gen.get_user(pool, @gen.GetUserParams::new(user_id)) {
    Some(user) => println("Found: \{user.name}")
    None => println("Not found")
  }
  pool.end()
}

///|
async fn run_tests() -> Unit noraise {
  run_tests_inner() catch {
    e => println("Error: \{e}")
  }
}

///|
fn main {
  @core.run_async(run_tests)
}
```

## Plugin Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `backend` | `"sqlite"` \| `"sqlite_js"` \| `"d1"` \| `"postgres"` \| `"postgres_js"` \| `"mysql_js"` | `"sqlite"` | Target backend |
| `validators` | `bool` | `false` | Generate validation functions |
| `json_schema` | `bool` | `false` | Generate JSON Schema |
| `overrides` | `array` | `[]` | Custom type mappings |

Example with all options:

```yaml
codegen:
  - plugin: moonbit
    out: "db/gen"
    options:
      backend: "d1"
      validators: true
      json_schema: true
      overrides:
        - column: "users.id"
          moonbit_type: "UserId"
        - db_type: "uuid"
          moonbit_type: "@mypackage.UUID"
```

### Type Overrides

Override default type mappings for specific columns or database types:

| Field | Type | Description |
|-------|------|-------------|
| `column` | `string` | Column name in `table.column` format |
| `db_type` | `string` | Database type (e.g., `INTEGER`, `TEXT`) |
| `moonbit_type` | `string` | MoonBit type (supports package paths like `@pkg.Type`) |

Column overrides take precedence over db_type overrides.

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
| `:execrows`| `Int`       | Returns number of affected rows |
| `:execlastid` | `Int64`  | Returns last inserted ID (for `INSERT ... RETURNING id`) |

## Examples

- [`examples/sqlite_native`](./examples/sqlite_native) - SQLite with native binding
- [`examples/sqlite_js`](./examples/sqlite_js) - SQLite with JS target (Node.js / Browser)
- [`examples/d1`](./examples/d1) - Cloudflare Worker with D1
- [`examples/postgres_native`](./examples/postgres_native) - PostgreSQL with native binding
- [`examples/postgres_js`](./examples/postgres_js) - PostgreSQL with JS target (Node.js)
- [`examples/mysql_js`](./examples/mysql_js) - MySQL with JS target (Node.js)

## Development

See [CONTRIBUTING.md](./CONTRIBUTING.md) for development workflow.

## License

Apache-2.0
