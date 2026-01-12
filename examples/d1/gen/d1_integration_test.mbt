// D1 Integration Tests (MoonBit)
// Run with: moon test --target js
// These tests use @cloudflare.Miniflare directly

///|
fn get_schema_sql() -> String {
  "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT NOT NULL UNIQUE, email TEXT NOT NULL UNIQUE, display_name TEXT NOT NULL, bio TEXT, avatar_url TEXT, created_at TEXT NOT NULL DEFAULT (datetime('now')), updated_at TEXT NOT NULL DEFAULT (datetime('now'))); CREATE TABLE IF NOT EXISTS categories (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE, slug TEXT NOT NULL UNIQUE, description TEXT); CREATE TABLE IF NOT EXISTS posts (id INTEGER PRIMARY KEY AUTOINCREMENT, author_id INTEGER NOT NULL REFERENCES users(id), category_id INTEGER REFERENCES categories(id), title TEXT NOT NULL, slug TEXT NOT NULL UNIQUE, content TEXT NOT NULL, excerpt TEXT, status TEXT NOT NULL DEFAULT 'draft' CHECK (status IN ('draft', 'published', 'archived')), published_at TEXT, created_at TEXT NOT NULL DEFAULT (datetime('now')), updated_at TEXT NOT NULL DEFAULT (datetime('now'))); CREATE TABLE IF NOT EXISTS tags (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE, slug TEXT NOT NULL UNIQUE); CREATE TABLE IF NOT EXISTS post_tags (post_id INTEGER NOT NULL REFERENCES posts(id) ON DELETE CASCADE, tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE, PRIMARY KEY (post_id, tag_id)); CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY AUTOINCREMENT, post_id INTEGER NOT NULL REFERENCES posts(id) ON DELETE CASCADE, author_name TEXT NOT NULL, author_email TEXT NOT NULL, content TEXT NOT NULL, is_approved INTEGER NOT NULL DEFAULT 0, created_at TEXT NOT NULL DEFAULT (datetime('now')));"
}

///|
async test "D1: create and get user" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")

  // Apply schema
  let _ = db.exec(get_schema_sql())

  // Create user
  create_user(
    db,
    CreateUserParams::new(
      "alice",
      "alice@example.com",
      "Alice Developer",
      Some("MoonBit enthusiast"),
      None,
    ),
  )

  // Get user by username
  let user = get_user_by_username(db, GetUserByUsernameParams::new("alice"))
  assert_true(user is Some(_))
  let u = user.unwrap()
  assert_eq(u.username, "alice")
  assert_eq(u.email, "alice@example.com")
  assert_eq(u.display_name, "Alice Developer")
  assert_eq(u.bio, Some("MoonBit enthusiast"))

  mf.dispose()
}

///|
async test "D1: create and list categories" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")

  // Apply schema
  let _ = db.exec(get_schema_sql())

  // Create category
  create_category(
    db,
    CreateCategoryParams::new("Technology", "tech", Some("Tech articles")),
  )

  // List categories
  let categories = list_categories(db)
  assert_eq(categories.length(), 1)
  let cat = categories[0]
  assert_eq(cat.name, "Technology")
  assert_eq(cat.slug, "tech")
  assert_eq(cat.description, Some("Tech articles"))

  mf.dispose()
}

///|
async test "D1: create and list tags" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")

  // Apply schema
  let _ = db.exec(get_schema_sql())

  // Create tags
  create_tag(db, CreateTagParams::new("MoonBit", "moonbit"))
  create_tag(db, CreateTagParams::new("WebAssembly", "wasm"))

  // List tags
  let tags = list_tags(db)
  assert_eq(tags.length(), 2)
  let tag_names = tags.map(fn(t) { t.name })
  assert_true(tag_names.contains("MoonBit"))
  assert_true(tag_names.contains("WebAssembly"))

  mf.dispose()
}

///|
async test "D1: post lifecycle - create, publish, delete" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")

  // Apply schema
  let _ = db.exec(get_schema_sql())

  // Create author
  create_user(
    db,
    CreateUserParams::new("author_test", "author@test.com", "Test Author", None, None),
  )
  let author = get_user_by_username(db, GetUserByUsernameParams::new("author_test"))
  let author_id = author.map(fn(u) { u.id }).unwrap_or(1L)

  // Create draft post
  create_post(
    db,
    CreatePostParams::new(
      author_id,
      None,
      "Test Post",
      "test-post-slug",
      "Test content",
      None,
      "draft",
    ),
  )

  // Verify post created as draft
  let post = get_post_by_slug(db, GetPostBySlugParams::new("test-post-slug"))
  assert_true(post is Some(_))
  let p = post.unwrap()
  assert_eq(p.title, "Test Post")
  assert_eq(p.status, "draft")

  // Publish post
  publish_post(db, PublishPostParams::new(p.id))

  // Verify published
  let published = get_post_by_slug(db, GetPostBySlugParams::new("test-post-slug"))
  assert_true(published is Some(_))
  let pp = published.unwrap()
  assert_eq(pp.status, "published")

  // Delete post
  delete_post(db, DeletePostParams::new(p.id))

  // Verify deleted
  let deleted = get_post_by_slug(db, GetPostBySlugParams::new("test-post-slug"))
  assert_true(deleted is None)

  mf.dispose()
}

///|
async test "D1: create comment (not approved by default)" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")

  // Apply schema
  let _ = db.exec(get_schema_sql())

  // Create author and post
  create_user(
    db,
    CreateUserParams::new("comment_author", "comment@test.com", "Comment Author", None, None),
  )
  let author = get_user_by_username(db, GetUserByUsernameParams::new("comment_author"))
  let author_id = author.map(fn(u) { u.id }).unwrap_or(1L)

  create_post(
    db,
    CreatePostParams::new(
      author_id,
      None,
      "Post for Comments",
      "post-for-comments-test",
      "Content",
      None,
      "published",
    ),
  )

  let post = get_post_by_slug(db, GetPostBySlugParams::new("post-for-comments-test"))
  let post_id = post.map(fn(p) { p.id }).unwrap_or(1L)

  // Create comment
  create_comment(
    db,
    CreateCommentParams::new(post_id, "John", "john@example.com", "Great!"),
  )

  // Comments are not approved by default, so list should be empty
  let comments = list_comments_for_post(db, ListCommentsForPostParams::new(post_id))
  assert_eq(comments.length(), 0)

  // Approve comment
  approve_all_comments_for_post(db, ApproveAllCommentsForPostParams::new(post_id))

  // Now should have approved comments
  let approved_comments = list_comments_for_post(db, ListCommentsForPostParams::new(post_id))
  assert_eq(approved_comments.length(), 1)
  assert_eq(approved_comments[0].author_name, "John")
  assert_eq(approved_comments[0].content, "Great!")

  mf.dispose()
}

///|
async test "D1: list users" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")

  // Apply schema
  let _ = db.exec(get_schema_sql())

  // Create multiple users
  create_user(
    db,
    CreateUserParams::new("user1", "user1@example.com", "User One", None, None),
  )
  create_user(
    db,
    CreateUserParams::new("user2", "user2@example.com", "User Two", None, None),
  )
  create_user(
    db,
    CreateUserParams::new("user3", "user3@example.com", "User Three", None, None),
  )

  // List users
  let users = list_users(db)
  assert_eq(users.length(), 3)

  mf.dispose()
}

///|
async test "D1: get user by id" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")

  // Apply schema
  let _ = db.exec(get_schema_sql())

  // Create user
  create_user(
    db,
    CreateUserParams::new("testuser", "testuser@example.com", "Test User", None, None),
  )

  // Get user by username first to get ID
  let user = get_user_by_username(db, GetUserByUsernameParams::new("testuser"))
  assert_true(user is Some(_))
  let user_id = user.unwrap().id

  // Get user by ID
  let user_by_id = get_user_by_id(db, GetUserByIdParams::new(user_id))
  assert_true(user_by_id is Some(_))
  assert_eq(user_by_id.unwrap().username, "testuser")

  mf.dispose()
}

///|
async test "D1: post with author join" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")

  // Apply schema
  let _ = db.exec(get_schema_sql())

  // Create author
  create_user(
    db,
    CreateUserParams::new("join_author", "join@test.com", "Join Author", None, None),
  )
  let author = get_user_by_username(db, GetUserByUsernameParams::new("join_author"))
  let author_id = author.unwrap().id

  // Create post
  create_post(
    db,
    CreatePostParams::new(
      author_id,
      None,
      "Join Test Post",
      "join-test-post",
      "Content for join test",
      Some("Excerpt"),
      "published",
    ),
  )

  // Get post with author
  let post = get_post_by_slug(db, GetPostBySlugParams::new("join-test-post"))
  let post_id = post.unwrap().id

  let post_with_author = get_post_with_author(db, GetPostWithAuthorParams::new(post_id))
  assert_true(post_with_author is Some(_))
  let pwa = post_with_author.unwrap()
  assert_eq(pwa.title, "Join Test Post")
  assert_eq(pwa.author_username, "join_author")
  assert_eq(pwa.author_display_name, "Join Author")

  mf.dispose()
}

///|
async test "D1: add and list tags for post" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")

  // Apply schema
  let _ = db.exec(get_schema_sql())

  // Create author
  create_user(
    db,
    CreateUserParams::new("tag_author", "tag@test.com", "Tag Author", None, None),
  )
  let author = get_user_by_username(db, GetUserByUsernameParams::new("tag_author"))
  let author_id = author.unwrap().id

  // Create post
  create_post(
    db,
    CreatePostParams::new(
      author_id,
      None,
      "Tagged Post",
      "tagged-post",
      "Content",
      None,
      "published",
    ),
  )
  let post = get_post_by_slug(db, GetPostBySlugParams::new("tagged-post"))
  let post_id = post.unwrap().id

  // Create tags
  let tag1 = get_or_create_tag(db, GetOrCreateTagParams::new("Rust", "rust"))
  let tag2 = get_or_create_tag(db, GetOrCreateTagParams::new("Go", "go"))
  let tag1_id = tag1.unwrap().id
  let tag2_id = tag2.unwrap().id

  // Add tags to post
  add_tag_to_post(db, AddTagToPostParams::new(post_id, tag1_id))
  add_tag_to_post(db, AddTagToPostParams::new(post_id, tag2_id))

  // List tags for post
  let tags = list_tags_for_post(db, ListTagsForPostParams::new(post_id))
  assert_eq(tags.length(), 2)

  // Remove tag
  remove_tag_from_post(db, RemoveTagFromPostParams::new(post_id, tag1_id))
  let tags_after = list_tags_for_post(db, ListTagsForPostParams::new(post_id))
  assert_eq(tags_after.length(), 1)
  assert_eq(tags_after[0].name, "Go")

  mf.dispose()
}

///|
async test "D1: list published posts" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")

  // Apply schema
  let _ = db.exec(get_schema_sql())

  // Create author
  create_user(
    db,
    CreateUserParams::new("pub_author", "pub@test.com", "Pub Author", None, None),
  )
  let author = get_user_by_username(db, GetUserByUsernameParams::new("pub_author"))
  let author_id = author.unwrap().id

  // Create draft and published posts
  create_post(
    db,
    CreatePostParams::new(
      author_id,
      None,
      "Draft Post",
      "draft-post",
      "Draft content",
      None,
      "draft",
    ),
  )
  create_post(
    db,
    CreatePostParams::new(
      author_id,
      None,
      "Published Post 1",
      "published-post-1",
      "Published content",
      None,
      "published",
    ),
  )
  create_post(
    db,
    CreatePostParams::new(
      author_id,
      None,
      "Published Post 2",
      "published-post-2",
      "Published content 2",
      None,
      "published",
    ),
  )

  // List published posts (should not include draft)
  let published = list_published_posts(db, ListPublishedPostsParams::new(10L))
  assert_eq(published.length(), 2)

  mf.dispose()
}
