// D1 Blog API Example - Cloudflare Workers
// Generated queries for a blog application with users, posts, categories, tags, and comments

// ============================================
// FFI bindings for Cloudflare Workers
// ============================================

/// Access D1 database from Workers env binding
fn get_d1(env : @cloudflare.CloudflareEnv) -> @cloudflare.D1Database? {
  let env_js : @core.Any = @core.identity(env)
  let db_js = env_js._get("DB")
  if @core.typeof_(db_js) == "undefined" {
    None
  } else {
    Some(@core.identity(db_js))
  }
}

/// Console logging for debugging
extern "js" fn console_log(msg : String) -> Unit =
  #| (msg) => console.log(msg)

// ============================================
// Blog Service - async functions demonstrating D1 usage
// ============================================

/// Initialize blog with sample data
pub async fn initialize_blog(db : @cloudflare.D1Database) -> Unit {
  console_log("Initializing blog data...")

  // Create a user (author)
  @gen.create_user(
    db,
    @gen.CreateUserParams::new(
      "alice",
      "alice@example.com",
      "Alice Developer",
      Some("MoonBit enthusiast and blogger"),
      Some("https://example.com/alice.png"),
    ),
  )

  // Create a category
  @gen.create_category(
    db,
    @gen.CreateCategoryParams::new("tech", "tech", Some("Technology articles")),
  )

  // Create tags
  @gen.create_tag(db, @gen.CreateTagParams::new("moonbit", "moonbit"))
  @gen.create_tag(db, @gen.CreateTagParams::new("wasm", "wasm"))

  console_log("Blog initialized!")
}

/// Create a new blog post
pub async fn create_blog_post(
  db : @cloudflare.D1Database,
  author_id : Int64,
  category_id : Int64?,
  title : String,
  slug : String,
  content : String,
  excerpt : String?
) -> Unit {
  @gen.create_post(
    db,
    @gen.CreatePostParams::new(
      author_id,
      category_id,
      title,
      slug,
      content,
      excerpt,
      "draft", // Default to draft status
    ),
  )
  console_log("Post created: " + title)
}

/// Get published posts for homepage
pub async fn get_homepage_posts(db : @cloudflare.D1Database, limit : Int64) -> Array[@gen.ListPublishedPostsRow] {
  @gen.list_published_posts(db, @gen.ListPublishedPostsParams::new(limit))
}

/// Get post by slug with comments
pub async fn get_post_with_comments(
  db : @cloudflare.D1Database,
  slug : String
) -> (@gen.GetPostBySlugRow?, Array[@gen.ListCommentsForPostRow]) {
  let post = @gen.get_post_by_slug(db, @gen.GetPostBySlugParams::new(slug))
  match post {
    Some(p) => {
      let comments = @gen.list_comments_for_post(
        db,
        @gen.ListCommentsForPostParams::new(p.id),
      )
      (Some(p), comments)
    }
    None => (None, [])
  }
}

/// Add a comment to a post
pub async fn add_comment(
  db : @cloudflare.D1Database,
  post_id : Int64,
  name : String,
  email : String,
  content : String
) -> Unit {
  @gen.create_comment(
    db,
    @gen.CreateCommentParams::new(post_id, name, email, content),
  )
}

/// Get all categories for navigation
pub async fn get_categories(db : @cloudflare.D1Database) -> Array[@gen.ListCategoriesRow] {
  @gen.list_categories(db)
}

/// Get all tags for sidebar
pub async fn get_tags(db : @cloudflare.D1Database) -> Array[@gen.ListTagsRow] {
  @gen.list_tags(db)
}

/// Get posts by author for author page
pub async fn get_author_posts(
  db : @cloudflare.D1Database,
  author_id : Int64
) -> Array[@gen.ListPostsByAuthorRow] {
  @gen.list_posts_by_author(db, @gen.ListPostsByAuthorParams::new(author_id))
}

/// Publish a draft post
pub async fn publish_blog_post(db : @cloudflare.D1Database, post_id : Int64) -> Unit {
  @gen.publish_post(db, @gen.PublishPostParams::new(post_id))
  console_log("Post published: " + post_id.to_string())
}

/// Get user profile
pub async fn get_user_profile(
  db : @cloudflare.D1Database,
  username : String
) -> @gen.GetUserByUsernameRow? {
  @gen.get_user_by_username(db, @gen.GetUserByUsernameParams::new(username))
}

// ============================================
// Main entry point for local testing
// ============================================

fn main {
  println("D1 Blog API Example")
  println("===================")
  println("")
  println("This example demonstrates sqlc-generated D1 queries for a blog.")
  println("Now using @cloudflare.D1Database type instead of @js.Any!")
  println("")
  println("Generated types: GetUserByIdRow, ListUsersRow, CreateUserParams, etc.")
  println("Generated queries: create_user, list_users, get_user_by_id, etc.")
  println("")
  println("Build: moon build --target js")
  println("Deploy: npx wrangler deploy")
}
