import path from "node:path";
import { defineWorkersProject, readD1Migrations } from "@cloudflare/vitest-pool-workers/config";

export default defineWorkersProject(async () => {
  const migrationsPath = path.join(import.meta.dirname!, "migrations");
  const migrations = await readD1Migrations(migrationsPath);

  return {
    test: {
      include: ["test/**/*.test.ts"],
      exclude: [
        "**/node_modules/**",
        "**/.mooncakes/**",
        "**/dist/**",
        "**/cypress/**",
        "**/.{idea,git,cache,output,temp}/**",
      ],
      setupFiles: ["./test/apply-migrations.ts"],
      poolOptions: {
        workers: {
          singleWorker: true,
          wrangler: {
            configPath: "./wrangler.toml",
          },
          miniflare: {
            bindings: { TEST_MIGRATIONS: migrations },
          },
        },
      },
    },
  };
});
