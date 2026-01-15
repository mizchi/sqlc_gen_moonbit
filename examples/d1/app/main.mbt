// Cloudflare Worker example using sqlc-gen-moonbit

///|
using @js { promisify3 }

///|
using @http { type Response }

///|
using @url { type URL }

///|
extern "js" fn json_response(body : String, status : Int) -> Response =
  #| (body, status) => new Response(body, { status, headers: { "Content-Type": "application/json" } })

///|
fn get_db(env : @cloudflare.CloudflareEnv) -> @cloudflare.D1Database? {
  let env_js : @core.Any = @core.identity(env)
  let db_js = env_js._get("DB")
  if @core.typeof_(db_js) == "undefined" {
    None
  } else {
    Some(@core.identity(db_js))
  }
}

///|
pub fn get_handler() -> @cloudflare.CloudflareFetchHandler {
  let async_handler = async fn(
    req : @cloudflare.CloudflareRequest,
    env : @cloudflare.CloudflareEnv,
    _ctx : @cloudflare.CloudflareContext,
  ) -> Response {
    let url = URL::new(req.url)
    let pathname = url.pathname

    let db = match get_db(env) {
      Some(d) => d
      None => return json_response("{\"error\":\"D1 database not configured\"}", 500)
    }

    if pathname == "/users" {
      // List all users
      let users = @gen.list_users(db) catch {
        e => return json_response("{\"error\":\"" + e.to_string() + "\"}", 500)
      }
      let result = @core.new_object()
      result["users"] = @core.any(users)
      json_response(@json.JSON::stringify(result), 200)
    } else if pathname.has_prefix("/users/") {
      let id_str = pathname[7:].to_string()
      let id = @global.parse_int(id_str).unwrap_or(0).to_int64()

      // Get user by ID
      let user = @gen.get_user(db, @gen.GetUserParams::new(id)) catch {
        e => return json_response("{\"error\":\"" + e.to_string() + "\"}", 500)
      }
      match user {
        Some(u) => json_response(@json.JSON::stringify(@core.any(u)), 200)
        None => json_response("{\"error\":\"User not found\"}", 404)
      }
    } else if pathname == "/users/create" {
      // Create user from query params
      let params = url.searchParams()
      let name = params.get("name")
      let email = params.get("email")
      match (name, email) {
        (Some(n), Some(e)) => {
          let id = @gen.create_user(db, @gen.CreateUserParams::new(n, e)) catch {
            err => return json_response("{\"error\":\"" + err.to_string() + "\"}", 500)
          }
          json_response("{\"id\":" + id.to_string() + "}", 201)
        }
        _ => json_response("{\"error\":\"Missing name or email\"}", 400)
      }
    } else {
      json_response("{\"error\":\"Not Found\"}", 404)
    }
  }
  promisify3(async_handler)
}
