// Cloudflare Worker entry point wrapper
// This imports the MoonBit-generated code and exports it in wrangler-compatible format
import { get_fetch_handler } from "./_build/js/release/build/app/app.js";

export default {
  fetch: get_fetch_handler(),
};
