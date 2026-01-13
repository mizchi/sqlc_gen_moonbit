# Examples

Simple examples demonstrating sqlc-gen-moonbit usage.

## sqlite-basic

Minimal SQLite native example with basic CRUD operations.

```bash
cd examples/sqlite-basic
moon update
sqlc generate
moon run --target native app
```

## d1-basic

Minimal Cloudflare D1 example with basic CRUD operations.

```bash
cd examples/d1-basic
moon update
sqlc generate
moon check --target js
```

For comprehensive test cases, see the `tests/` directory.
