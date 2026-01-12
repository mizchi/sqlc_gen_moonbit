# sqlc_gen_moonbit development tasks

# Default: show available commands
default:
    @just --list

# Build the native sqlc plugin
build-plugin:
    moon build --target native ./cmd/native

# Build the wasm sqlc plugin
build-plugin-wasm:
    moon build --target wasm ./cmd/wasm

# Run all tests
test: test-sqlite-native test-d1

# Test sqlite-native example
test-sqlite-native:
    cd examples/sqlite-native && moon test --target native

# Test d1 example
test-d1:
    cd examples/d1 && moon test --target js

# Generate code for all examples
generate: build-plugin generate-sqlite-native generate-d1

# Generate code for sqlite-native example
generate-sqlite-native: build-plugin
    cd examples/sqlite-native && sqlc generate

# Generate code for d1 example
generate-d1: build-plugin
    cd examples/d1 && sqlc generate

# Build d1 worker for wrangler
build-d1:
    cd examples/d1 && moon build --target js

# Start d1 worker with wrangler (requires wrangler.toml and migrations)
dev-d1: build-d1
    cd examples/d1 && npx wrangler dev

# ===== D1 Migration Commands (Atlas) =====

# Generate new migration from schema diff
migrate-diff name:
    cd examples/d1 && atlas migrate diff {{name}} --env local

# Apply migrations to local D1
migrate-apply:
    cd examples/d1 && npx wrangler d1 migrations apply blog-db --local

# Apply migrations to remote D1
migrate-apply-remote:
    cd examples/d1 && npx wrangler d1 migrations apply blog-db --remote

# Show migration status
migrate-status:
    cd examples/d1 && atlas migrate status --env local

# Validate migrations
migrate-lint:
    cd examples/d1 && atlas migrate lint --env local

# Hash migrations (after manual edit)
migrate-hash:
    cd examples/d1 && atlas migrate hash --env local

# ===== Check & Format =====

# Check all MoonBit code
check:
    moon check --target native ./cmd/native
    moon check --target wasm ./cmd/wasm
    cd examples/sqlite-native && moon check
    cd examples/d1 && moon check

# Clean build artifacts
clean:
    moon clean
    cd examples/sqlite-native && moon clean
    cd examples/d1 && moon clean

# Format all MoonBit code
fmt:
    moon fmt
    cd examples/sqlite-native && moon fmt
    cd examples/d1 && moon fmt
