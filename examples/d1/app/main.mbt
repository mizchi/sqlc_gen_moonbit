// D1 Blog API Worker
// Build: moon build --target js
// Deploy: wrangler deploy

///|
using @js {promisify3}

///|
extern "js" fn json_response(body : String, status : Int) -> @http.Response =
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
fn user_to_json(u : @gen.ListUsersRow) -> @core.Any {
  let obj = @core.new_object()
  obj["id"] = u.id.to_int() |> @core.any
  obj["username"] = u.username |> @core.any
  obj["email"] = u.email |> @core.any
  obj["display_name"] = u.display_name |> @core.any
  obj["bio"] = match u.bio {
    Some(v) => @core.any(v)
    None => @core.null()
  }
  obj["avatar_url"] = match u.avatar_url {
    Some(v) => @core.any(v)
    None => @core.null()
  }
  obj["created_at"] = u.created_at |> @core.any
  obj["updated_at"] = u.updated_at |> @core.any
  obj
}

///|
fn post_to_json(p : @gen.ListPublishedPostsRow) -> @core.Any {
  let obj = @core.new_object()
  obj["id"] = p.id.to_int() |> @core.any
  obj["author_id"] = p.author_id.to_int() |> @core.any
  obj["category_id"] = match p.category_id {
    Some(v) => @core.any(v.to_int())
    None => @core.null()
  }
  obj["title"] = p.title |> @core.any
  obj["slug"] = p.slug |> @core.any
  obj["excerpt"] = match p.excerpt {
    Some(v) => @core.any(v)
    None => @core.null()
  }
  obj["status"] = p.status |> @core.any
  obj["published_at"] = match p.published_at {
    Some(v) => @core.any(v)
    None => @core.null()
  }
  obj["created_at"] = p.created_at |> @core.any
  obj["updated_at"] = p.updated_at |> @core.any
  obj
}

///|
fn post_detail_to_json(p : @gen.GetPostBySlugRow) -> @core.Any {
  let obj = @core.new_object()
  obj["id"] = p.id.to_int() |> @core.any
  obj["author_id"] = p.author_id.to_int() |> @core.any
  obj["category_id"] = match p.category_id {
    Some(v) => @core.any(v.to_int())
    None => @core.null()
  }
  obj["title"] = p.title |> @core.any
  obj["slug"] = p.slug |> @core.any
  obj["content"] = p.content |> @core.any
  obj["excerpt"] = match p.excerpt {
    Some(v) => @core.any(v)
    None => @core.null()
  }
  obj["status"] = p.status |> @core.any
  obj["published_at"] = match p.published_at {
    Some(v) => @core.any(v)
    None => @core.null()
  }
  obj["created_at"] = p.created_at |> @core.any
  obj["updated_at"] = p.updated_at |> @core.any
  obj
}

///|
fn category_to_json(c : @gen.ListCategoriesRow) -> @core.Any {
  let obj = @core.new_object()
  obj["id"] = c.id.to_int() |> @core.any
  obj["name"] = c.name |> @core.any
  obj["slug"] = c.slug |> @core.any
  obj["description"] = match c.description {
    Some(v) => @core.any(v)
    None => @core.null()
  }
  obj
}

///|
fn tag_to_json(t : @gen.ListTagsRow) -> @core.Any {
  let obj = @core.new_object()
  obj["id"] = t.id.to_int() |> @core.any
  obj["name"] = t.name |> @core.any
  obj["slug"] = t.slug |> @core.any
  obj
}

///|
pub fn get_fetch_handler() -> @cloudflare.CloudflareFetchHandler {
  let async_handler = async fn(
    req : @cloudflare.CloudflareRequest,
    env : @cloudflare.CloudflareEnv,
    _ctx : @cloudflare.CloudflareContext,
  ) -> @http.Response {
    let url = @url.URL::new(req.url)
    let pathname = url.pathname
    let db = match get_db(env) {
      Some(d) => d
      None =>
        return json_response("{\"error\":\"D1 database not configured\"}", 500)
    }

    // GET /api/users - list all users
    if pathname == "/api/users" {
      let users = @gen.list_users(db) catch {
        e => return json_response("{\"error\":\"" + e.to_string() + "\"}", 500)
      }
      let arr = @core.new_array()
      for u in users {
        @core.any(arr)._call("push", [user_to_json(u)]) |> ignore
      }
      let result = @core.new_object()
      result["users"] = arr
      result["count"] = users.length() |> @core.any
      return json_response(@json.JSON::stringify(result), 200)
    }

    // GET /api/users/:username
    if pathname.has_prefix("/api/users/") {
      let username = pathname[11:].to_string()
      if username.length() > 0 {
        let user = @gen.get_user_by_username(
          db,
          @gen.GetUserByUsernameParams::new(username),
        ) catch {
          e =>
            return json_response("{\"error\":\"" + e.to_string() + "\"}", 500)
        }
        match user {
          Some(u) => {
            let obj = @core.new_object()
            obj["id"] = u.id.to_int() |> @core.any
            obj["username"] = u.username |> @core.any
            obj["email"] = u.email |> @core.any
            obj["display_name"] = u.display_name |> @core.any
            obj["bio"] = match u.bio {
              Some(v) => @core.any(v)
              None => @core.null()
            }
            obj["avatar_url"] = match u.avatar_url {
              Some(v) => @core.any(v)
              None => @core.null()
            }
            obj["created_at"] = u.created_at |> @core.any
            obj["updated_at"] = u.updated_at |> @core.any
            return json_response(@json.JSON::stringify(obj), 200)
          }
          None => return json_response("{\"error\":\"User not found\"}", 404)
        }
      }
    }

    // GET /api/posts - list published posts
    if pathname == "/api/posts" {
      let limit_str = url.searchParams().get("limit")
      let limit = match limit_str {
        Some(s) => {
          let n = @global.parse_int(s)
          match n {
            Some(v) => v.to_int64()
            None => 20L
          }
        }
        None => 20L
      }
      let posts = @gen.list_published_posts(
        db,
        @gen.ListPublishedPostsParams::new(limit),
      ) catch {
        e => return json_response("{\"error\":\"" + e.to_string() + "\"}", 500)
      }
      let arr = @core.new_array()
      for p in posts {
        @core.any(arr)._call("push", [post_to_json(p)]) |> ignore
      }
      let result = @core.new_object()
      result["posts"] = arr
      result["count"] = posts.length() |> @core.any
      return json_response(@json.JSON::stringify(result), 200)
    }

    // GET /api/posts/:slug
    if pathname.has_prefix("/api/posts/") {
      let slug = pathname[11:].to_string()
      if slug.length() > 0 {
        let post = @gen.get_post_by_slug(
          db,
          @gen.GetPostBySlugParams::new(slug),
        ) catch {
          e =>
            return json_response("{\"error\":\"" + e.to_string() + "\"}", 500)
        }
        match post {
          Some(p) =>
            return json_response(
              @json.JSON::stringify(post_detail_to_json(p)),
              200,
            )
          None => return json_response("{\"error\":\"Post not found\"}", 404)
        }
      }
    }

    // GET /api/categories
    if pathname == "/api/categories" {
      let categories = @gen.list_categories(db) catch {
        e => return json_response("{\"error\":\"" + e.to_string() + "\"}", 500)
      }
      let arr = @core.new_array()
      for c in categories {
        @core.any(arr)._call("push", [category_to_json(c)]) |> ignore
      }
      let result = @core.new_object()
      result["categories"] = arr
      result["count"] = categories.length() |> @core.any
      return json_response(@json.JSON::stringify(result), 200)
    }

    // GET /api/tags
    if pathname == "/api/tags" {
      let tags = @gen.list_tags(db) catch {
        e => return json_response("{\"error\":\"" + e.to_string() + "\"}", 500)
      }
      let arr = @core.new_array()
      for t in tags {
        @core.any(arr)._call("push", [tag_to_json(t)]) |> ignore
      }
      let result = @core.new_object()
      result["tags"] = arr
      result["count"] = tags.length() |> @core.any
      return json_response(@json.JSON::stringify(result), 200)
    }

    // Default: API help
    let help = @core.new_object()
    help["name"] = "D1 Blog API" |> @core.any
    help["version"] = "1.0.0" |> @core.any
    let endpoints = @core.new_array()
    @core.any(endpoints)._call("push", ["GET /api/users" |> @core.any])
    |> ignore
    @core.any(endpoints)._call("push", ["GET /api/users/:username" |> @core.any])
    |> ignore
    @core.any(endpoints)._call("push", ["GET /api/posts?limit=N" |> @core.any])
    |> ignore
    @core.any(endpoints)._call("push", ["GET /api/posts/:slug" |> @core.any])
    |> ignore
    @core.any(endpoints)._call("push", ["GET /api/categories" |> @core.any])
    |> ignore
    @core.any(endpoints)._call("push", ["GET /api/tags" |> @core.any]) |> ignore
    help["endpoints"] = endpoints
    json_response(@json.JSON::stringify(help), 200)
  }
  promisify3(async_handler)
}
