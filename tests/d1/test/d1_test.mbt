// D1 Integration Tests (MoonBit)
// Run with: moon test --target js
// These tests use @cloudflare.Miniflare directly

///|
fn get_schema_sql() -> String {
  "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT NOT NULL UNIQUE, email TEXT NOT NULL UNIQUE, display_name TEXT NOT NULL, bio TEXT, avatar_url TEXT, created_at TEXT NOT NULL DEFAULT (datetime('now')), updated_at TEXT NOT NULL DEFAULT (datetime('now'))); CREATE TABLE IF NOT EXISTS categories (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE, slug TEXT NOT NULL UNIQUE, description TEXT); CREATE TABLE IF NOT EXISTS posts (id INTEGER PRIMARY KEY AUTOINCREMENT, author_id INTEGER NOT NULL REFERENCES users(id), category_id INTEGER REFERENCES categories(id), title TEXT NOT NULL, slug TEXT NOT NULL UNIQUE, content TEXT NOT NULL, excerpt TEXT, status TEXT NOT NULL DEFAULT 'draft', published_at TEXT, created_at TEXT NOT NULL DEFAULT (datetime('now')), updated_at TEXT NOT NULL DEFAULT (datetime('now'))); CREATE TABLE IF NOT EXISTS tags (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE, slug TEXT NOT NULL UNIQUE); CREATE TABLE IF NOT EXISTS post_tags (post_id INTEGER NOT NULL REFERENCES posts(id) ON DELETE CASCADE, tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE, PRIMARY KEY (post_id, tag_id)); CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY AUTOINCREMENT, post_id INTEGER NOT NULL REFERENCES posts(id) ON DELETE CASCADE, author_name TEXT NOT NULL, author_email TEXT NOT NULL, content TEXT NOT NULL, is_approved INTEGER NOT NULL DEFAULT 0, created_at TEXT NOT NULL DEFAULT (datetime('now')));"
}

///|
async test "D1: create and get user" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")
  let _ = db.exec(get_schema_sql())
  @gen.create_user(
    db,
    @gen.CreateUserParams::new(
      "alice",
      "alice@example.com",
      "Alice Developer",
      Some("MoonBit enthusiast"),
      None,
    ),
  )
  let user = @gen.get_user_by_username(
    db,
    @gen.GetUserByUsernameParams::new("alice"),
  )
  assert_true(user is Some(_))
  let u = user.unwrap()
  assert_eq(u.username, "alice")
  assert_eq(u.email, "alice@example.com")
  assert_eq(u.display_name, "Alice Developer")
  assert_eq(u.bio, Some("MoonBit enthusiast"))
  mf.dispose()
}

///|
async test "D1: list users and get by id" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")
  let _ = db.exec(get_schema_sql())
  @gen.create_user(db, @gen.CreateUserParams::new("alice", "alice@example.com", "Alice", None, None))
  @gen.create_user(db, @gen.CreateUserParams::new("bob", "bob@example.com", "Bob", None, None))
  @gen.create_user(db, @gen.CreateUserParams::new("charlie", "charlie@example.com", "Charlie", None, None))
  let users = @gen.list_users(db)
  assert_eq(users.length(), 3)
  let user = @gen.get_user_by_id(db, @gen.GetUserByIdParams::new(2L))
  assert_true(user is Some(_))
  assert_eq(user.unwrap().username, "bob")
  mf.dispose()
}

///|
async test "D1: create user returning id and delete" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")
  let _ = db.exec(get_schema_sql())
  let id1 = @gen.create_user_returning_id(db, @gen.CreateUserReturningIdParams::new("alice", "alice@example.com", "Alice", None, None))
  assert_eq(id1, 1L)
  let id2 = @gen.create_user_returning_id(db, @gen.CreateUserReturningIdParams::new("bob", "bob@example.com", "Bob", None, None))
  assert_eq(id2, 2L)
  let deleted = @gen.delete_user_by_id(db, @gen.DeleteUserByIdParams::new(1L))
  assert_eq(deleted, 1)
  let user = @gen.get_user_by_id(db, @gen.GetUserByIdParams::new(1L))
  assert_true(user is None)
  mf.dispose()
}

///|
async test "D1: update user display name" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")
  let _ = db.exec(get_schema_sql())
  @gen.create_user(db, @gen.CreateUserParams::new("alice", "alice@example.com", "Alice", None, None))
  let updated = @gen.update_user_display_name(db, @gen.UpdateUserDisplayNameParams::new("Alicia Developer", 1L))
  assert_eq(updated, 1)
  let user = @gen.get_user_by_id(db, @gen.GetUserByIdParams::new(1L))
  assert_eq(user.unwrap().display_name, "Alicia Developer")
  mf.dispose()
}

///|
async test "D1: categories CRUD" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")
  let _ = db.exec(get_schema_sql())
  @gen.create_category(db, @gen.CreateCategoryParams::new("Technology", "technology", Some("Tech posts")))
  @gen.create_category(db, @gen.CreateCategoryParams::new("Lifestyle", "lifestyle", None))
  let categories = @gen.list_categories(db)
  assert_eq(categories.length(), 2)
  let cat = @gen.get_category_by_id(db, @gen.GetCategoryByIdParams::new(1L))
  assert_true(cat is Some(_))
  assert_eq(cat.unwrap().name, "Technology")
  mf.dispose()
}

///|
async test "D1: posts CRUD" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")
  let _ = db.exec(get_schema_sql())
  @gen.create_user(db, @gen.CreateUserParams::new("alice", "alice@example.com", "Alice", None, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, None, "First Post", "first-post", "Content", None, "draft"))
  let post = @gen.get_post_by_id(db, @gen.GetPostByIdParams::new(1L))
  assert_true(post is Some(_))
  assert_eq(post.unwrap().title, "First Post")
  let post2 = @gen.get_post_by_slug(db, @gen.GetPostBySlugParams::new("first-post"))
  assert_true(post2 is Some(_))
  @gen.publish_post(db, @gen.PublishPostParams::new(1L))
  let published = @gen.get_post_by_id(db, @gen.GetPostByIdParams::new(1L))
  assert_eq(published.unwrap().status, "published")
  mf.dispose()
}

///|
async test "D1: list posts by author" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")
  let _ = db.exec(get_schema_sql())
  @gen.create_user(db, @gen.CreateUserParams::new("alice", "alice@example.com", "Alice", None, None))
  @gen.create_user(db, @gen.CreateUserParams::new("bob", "bob@example.com", "Bob", None, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, None, "Alice Post 1", "alice-1", "Content", None, "draft"))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, None, "Alice Post 2", "alice-2", "Content", None, "draft"))
  @gen.create_post(db, @gen.CreatePostParams::new(2L, None, "Bob Post 1", "bob-1", "Content", None, "draft"))
  let alice_posts = @gen.list_posts_by_author(db, @gen.ListPostsByAuthorParams::new(1L))
  assert_eq(alice_posts.length(), 2)
  let bob_posts = @gen.list_posts_by_author(db, @gen.ListPostsByAuthorParams::new(2L))
  assert_eq(bob_posts.length(), 1)
  mf.dispose()
}

///|
async test "D1: tags CRUD" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")
  let _ = db.exec(get_schema_sql())
  @gen.create_tag(db, @gen.CreateTagParams::new("MoonBit", "moonbit"))
  @gen.create_tag(db, @gen.CreateTagParams::new("WASM", "wasm"))
  let tags = @gen.list_tags(db)
  assert_eq(tags.length(), 2)
  mf.dispose()
}

///|
async test "D1: post tags relationship" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")
  let _ = db.exec(get_schema_sql())
  @gen.create_user(db, @gen.CreateUserParams::new("alice", "alice@example.com", "Alice", None, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, None, "Post", "post", "Content", None, "draft"))
  @gen.create_tag(db, @gen.CreateTagParams::new("MoonBit", "moonbit"))
  @gen.create_tag(db, @gen.CreateTagParams::new("WASM", "wasm"))
  @gen.add_tag_to_post(db, @gen.AddTagToPostParams::new(1L, 1L))
  @gen.add_tag_to_post(db, @gen.AddTagToPostParams::new(1L, 2L))
  let tags = @gen.list_tags_for_post(db, @gen.ListTagsForPostParams::new(1L))
  assert_eq(tags.length(), 2)
  @gen.remove_tag_from_post(db, @gen.RemoveTagFromPostParams::new(1L, 1L))
  let tags2 = @gen.list_tags_for_post(db, @gen.ListTagsForPostParams::new(1L))
  assert_eq(tags2.length(), 1)
  mf.dispose()
}

///|
async test "D1: comments" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")
  let _ = db.exec(get_schema_sql())
  @gen.create_user(db, @gen.CreateUserParams::new("alice", "alice@example.com", "Alice", None, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, None, "Post", "post", "Content", None, "draft"))
  @gen.create_comment(db, @gen.CreateCommentParams::new(1L, "Bob", "bob@example.com", "Great!"))
  @gen.create_comment(db, @gen.CreateCommentParams::new(1L, "Charlie", "charlie@example.com", "Nice!"))
  // Not approved yet
  let comments = @gen.list_comments_for_post(db, @gen.ListCommentsForPostParams::new(1L))
  assert_eq(comments.length(), 0)
  @gen.approve_comment(db, @gen.ApproveCommentParams::new(1L))
  let comments2 = @gen.list_comments_for_post(db, @gen.ListCommentsForPostParams::new(1L))
  assert_eq(comments2.length(), 1)
  mf.dispose()
}

///|
async test "D1: post with author JOIN" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")
  let _ = db.exec(get_schema_sql())
  @gen.create_user(db, @gen.CreateUserParams::new("alice", "alice@example.com", "Alice Dev", Some("Bio"), None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, None, "Post", "post", "Content", None, "published"))
  let post = @gen.get_post_with_author(db, @gen.GetPostWithAuthorParams::new(1L))
  assert_true(post is Some(_))
  let p = post.unwrap()
  assert_eq(p.author_username, "alice")
  assert_eq(p.author_display_name, "Alice Dev")
  mf.dispose()
}

///|
async test "D1: pagination" {
  let mf = @cloudflare.Miniflare::new({
    ..@cloudflare.MiniflareOptions::default(),
    d1Databases: Some(["TEST_DB"]),
  })
  mf.ready()
  let db = mf.get_d1_database("TEST_DB")
  let _ = db.exec(get_schema_sql())
  @gen.create_user(db, @gen.CreateUserParams::new("alice", "a@example.com", "Alice", None, None))
  @gen.create_user(db, @gen.CreateUserParams::new("bob", "b@example.com", "Bob", None, None))
  @gen.create_user(db, @gen.CreateUserParams::new("charlie", "c@example.com", "Charlie", None, None))
  @gen.create_user(db, @gen.CreateUserParams::new("diana", "d@example.com", "Diana", None, None))
  let page1 = @gen.list_users_paginated(db, @gen.ListUsersPaginatedParams::new(0L, 2L))
  assert_eq(page1.length(), 2)
  assert_eq(page1[0].id, 1L)
  let page2 = @gen.list_users_paginated(db, @gen.ListUsersPaginatedParams::new(2L, 2L))
  assert_eq(page2.length(), 2)
  assert_eq(page2[0].id, 3L)
  mf.dispose()
}
