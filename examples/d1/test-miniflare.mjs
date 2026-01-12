import { Miniflare } from "miniflare";

// Create Miniflare instance with D1
const mf = new Miniflare({
  modules: true,
  script: `export default { fetch() { return new Response("ok"); } }`,
  d1Databases: {
    DB: "test-db"
  }
});

// Get D1 database instance
const db = await mf.getD1Database("DB");

// Test D1 API
await db.exec(`CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)`);
await db.prepare("INSERT INTO users (name) VALUES (?)").bind("alice").run();
const result = await db.prepare("SELECT * FROM users").all();
console.log("D1 works!", JSON.stringify(result.results, null, 2));

await mf.dispose();
