# Contributing to sqlc-gen-moonbit

## Prerequisites

- [MoonBit](https://www.moonbitlang.com/) - `curl -fsSL https://cli.moonbitlang.com/install/unix.sh | bash`
- [Go](https://go.dev/) 1.21+ (for protoc-gen-mbt)
- [buf](https://buf.build/) (for protobuf generation)
- [sqlc](https://sqlc.dev/) (for testing)
- SQLite3 development libraries (for native tests)

## Project Structure

```
sqlc_gen_moonbit/
├── bin/                    # Distribution binaries
│   └── sqlc-gen-moonbit.wasm  # WASM plugin (committed)
├── cmd/
│   ├── native/             # Native plugin entry point
│   └── wasm/               # WASM plugin entry point
├── lib/
│   ├── codegen/            # Code generation logic
│   └── proto/              # Generated protobuf MoonBit code
├── examples/
│   ├── sqlite-native/      # SQLite native example
│   └── d1/                 # Cloudflare D1 example
└── vendor/
    ├── sqlc/               # sqlc protobuf definitions
    └── protoc-gen-mbt/     # Protobuf to MoonBit compiler
```

## Building

### WASM Plugin (for distribution)

```bash
moon update
moon build --target wasm ./cmd/wasm
cp target/wasm/release/build/cmd/wasm/wasm.wasm bin/sqlc-gen-moonbit.wasm
```

After updating the WASM, calculate and update the sha256:

```bash
shasum -a 256 bin/sqlc-gen-moonbit.wasm
```

Update the hash in:
- `README.mbt.md`
- `examples/sqlite-native/sqlc.yaml`
- `examples/d1/sqlc.yaml`

### Native Plugin (for development)

```bash
moon build --target native ./cmd/native
```

## Regenerating Protobuf Code

When sqlc's protobuf definitions change, regenerate the MoonBit code:

### 1. Build protoc-gen-mbt

```bash
cd vendor/protoc-gen-mbt
go build -o ../../bin/protoc-gen-mbt ./main.go
cd ../..
```

### 2. Run buf generate

```bash
buf generate --template buf.gen.yaml vendor/sqlc/protos
```

This regenerates `lib/proto/` directory with updated MoonBit types.

**Note**: After regeneration, you may need to:
- Remove generated `moon.mod.json` in `lib/proto/` (we use the root module)
- Move `lib/proto/proto/src/*` to `lib/proto/` if nested incorrectly

## Testing

### SQLite Native Tests

```bash
cd examples/sqlite-native
moon update
moon test --target native
```

Requires SQLite3 development libraries:
- macOS: `brew install sqlite`
- Ubuntu: `sudo apt-get install libsqlite3-dev`

### D1 Tests

```bash
cd examples/d1
npm install
moon update
moon test --target js
```

## Code Generation

To regenerate MoonBit code from SQL:

```bash
cd examples/sqlite-native
sqlc generate

cd ../d1
sqlc generate
```

## Development Workflow

1. Make changes to `lib/codegen/`
2. Build WASM: `moon build --target wasm ./cmd/wasm`
3. Copy to bin: `cp target/wasm/release/build/cmd/wasm/wasm.wasm bin/sqlc-gen-moonbit.wasm`
4. Test with sqlc: `cd examples/sqlite-native && sqlc generate`
5. Run tests: `moon test --target native`
6. Update sha256 in documentation

## Commit Guidelines

- Use conventional commits (feat:, fix:, chore:, etc.)
- Include `Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>` if AI-assisted
