# sqlc_gen_moonbit development tasks

default:
    @just --list

# Build plugins and copy to bin/
build:
    moon build --target native ./cmd/native
    moon build --target wasm ./cmd/wasm
    mkdir -p bin
    cp _build/native/release/build/cmd/native/native.exe bin/sqlc-gen-moonbit
    cp _build/wasm/release/build/cmd/wasm/wasm.wasm bin/sqlc-gen-moonbit.wasm

# Run core tests (default)
test: test-core

# Run all tests including postgres (requires libpq)
test-all: test-core test-pg

# Core tests (no postgres)
test-core:
    moon test --target native ./lib/codegen
    cd tests/sqlite_native && moon test --target native
    cd tests/d1 && moon test --target js

# Postgres tests (requires libpq + DATABASE_URL/POSTGRES_TEST_URL)
test-pg:
    tools/pg/run.sh moon test --target native --package backend_postgres_native_test -C tests/postgres_native

# Generate code for examples
generate: build
    cd examples/sqlite_native && sqlc generate
    cd examples/d1 && sqlc generate

# D1 tasks
# action: build|dev|migrate-diff|migrate-apply|migrate-apply-remote|migrate-status|migrate-lint|migrate-hash
# name: required for migrate-diff
d1 action="build" name="":
    tools/d1/run.sh {{action}} {{name}}

# ===== Check & Format =====

# Update generated interfaces
info:
    moon info --target native cmd/native
    moon info --target wasm cmd/wasm
    moon info --target native lib/codegen
    moon info --target native lib/proto/analysis
    moon info --target native lib/proto/plugin
    moon info --target native lib/proto/vet
    moon info --target native tools/codegen
    cd examples/sqlite_native && moon info --target native
    cd examples/d1 && moon info --target js
    cd examples/postgres_native && moon info --target native
    cd tests/sqlite_native && moon info --target native
    cd tests/d1 && moon info --target js
    cd tests/postgres_native && moon info --target native

# Check all MoonBit code
check:
    moon check --target native ./cmd/native
    moon check --target wasm ./cmd/wasm
    cd examples/sqlite_native && moon check --target native
    cd examples/d1 && moon check --target js

# Clean build artifacts
clean:
    moon clean
    cd examples/sqlite_native && moon clean
    cd examples/d1 && moon clean

# Format all MoonBit code
fmt:
    moon fmt
    cd examples/sqlite_native && moon fmt
    cd examples/d1 && moon fmt
