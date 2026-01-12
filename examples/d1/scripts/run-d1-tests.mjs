#!/usr/bin/env node
// D1 Integration Tests Runner
//
// MoonBit D1 tests are now run via:
//   moon test --target js
//
// This runs the tests in gen/d1_integration_test.mbt using Miniflare.
//
// TypeScript tests can be run via:
//   npx vitest run
//
// The TypeScript tests are in test/d1.test.ts and use @cloudflare/vitest-pool-workers.

import { spawn } from "node:child_process";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const __dirname = dirname(fileURLToPath(import.meta.url));
const rootDir = join(__dirname, "..");

console.log("Running MoonBit D1 tests...");
console.log("Command: moon test --target js");
console.log("");

const proc = spawn("moon", ["test", "--target", "js"], {
  cwd: rootDir,
  stdio: "inherit",
});

proc.on("close", (code) => {
  if (code !== 0) {
    console.error(`\nTests failed with exit code ${code}`);
    process.exit(code);
  }
  console.log("\n✓ All MoonBit D1 tests passed!");
});
