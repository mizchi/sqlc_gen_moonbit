// SQLite Native Integration Tests

///|
fn cstring(s : String) -> Bytes {
  @encoding.encode(@encoding.UTF8, s)
}

///|
fn open_memory_db() -> @sqlite.Sqlite3 {
  @sqlite.sqlite_open_v2(
    cstring(":memory:"),
    @sqlite.SQLITE_OPEN_READWRITE | @sqlite.SQLITE_OPEN_CREATE | @sqlite.SQLITE_OPEN_MEMORY,
    Bytes::new(0),
  )
}

///|
fn exec_sql(db : @sqlite.Sqlite3, sql : String) -> Unit {
  let stmt = @sqlite.sqlite_prepare(db, cstring(sql))
  @sqlite.sqlite_step(stmt) |> ignore
  @sqlite.sqlite_finalize(stmt)
}

///|
fn create_schema(db : @sqlite.Sqlite3) -> Unit {
  exec_sql(db, "CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, email TEXT NOT NULL UNIQUE, age INTEGER, balance REAL DEFAULT 0.0, is_active INTEGER NOT NULL DEFAULT 1, bio TEXT, created_at TEXT NOT NULL DEFAULT (datetime('now')))")
  exec_sql(db, "CREATE TABLE posts (id INTEGER PRIMARY KEY AUTOINCREMENT, author_id INTEGER NOT NULL REFERENCES users(id), title TEXT NOT NULL, content TEXT NOT NULL, view_count INTEGER NOT NULL DEFAULT 0, is_published INTEGER NOT NULL DEFAULT 0, published_at TEXT, created_at TEXT NOT NULL DEFAULT (datetime('now')))")
  exec_sql(db, "CREATE TABLE tags (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE)")
  exec_sql(db, "CREATE TABLE post_tags (post_id INTEGER NOT NULL, tag_id INTEGER NOT NULL, PRIMARY KEY (post_id, tag_id))")
}

// ============================================
// Basic User CRUD Tests
// ============================================

///|
test "create and get user by id" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(
    db,
    @gen.CreateUserParams::new("Alice", "alice@example.com", Some(25L), Some(100.5), 1L, Some("Developer")),
  )
  let user = @gen.get_user_by_id(db, @gen.GetUserByIdParams::new(1L))
  assert_true(user is Some(_))
  let u = user.unwrap()
  assert_eq(u.name, "Alice")
  assert_eq(u.email, "alice@example.com")
  assert_eq(u.age, Some(25L))
  assert_eq(u.bio, Some("Developer"))
}

///|
test "create and get user by email" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(
    db,
    @gen.CreateUserParams::new("Bob", "bob@example.com", None, None, 1L, None),
  )
  let user = @gen.get_user_by_email(db, @gen.GetUserByEmailParams::new("bob@example.com"))
  assert_true(user is Some(_))
  assert_eq(user.unwrap().name, "Bob")
}

///|
test "list users ordered by name" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Charlie", "charlie@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Bob", "bob@example.com", None, None, 1L, None))
  let users = @gen.list_users(db)
  assert_eq(users.length(), 3)
  assert_eq(users[0].name, "Alice")
  assert_eq(users[1].name, "Bob")
  assert_eq(users[2].name, "Charlie")
}

///|
test "get non-existent user returns None" {
  let db = open_memory_db()
  create_schema(db)
  let user = @gen.get_user_by_id(db, @gen.GetUserByIdParams::new(999L))
  assert_true(user is None)
}

///|
test "create user returning id (:execlastid)" {
  let db = open_memory_db()
  create_schema(db)
  let id1 = @gen.create_user_returning_id(
    db,
    @gen.CreateUserReturningIdParams::new("Alice", "alice@example.com", None, None, 1L, None),
  )
  assert_eq(id1, 1L)
  let id2 = @gen.create_user_returning_id(
    db,
    @gen.CreateUserReturningIdParams::new("Bob", "bob@example.com", None, None, 1L, None),
  )
  assert_eq(id2, 2L)
}

///|
test "delete user by id (:execrows)" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  let deleted = @gen.delete_user_by_id(db, @gen.DeleteUserByIdParams::new(1L))
  assert_eq(deleted, 1)
  let user = @gen.get_user_by_id(db, @gen.GetUserByIdParams::new(1L))
  assert_true(user is None)
  // Delete non-existent
  let deleted2 = @gen.delete_user_by_id(db, @gen.DeleteUserByIdParams::new(999L))
  assert_eq(deleted2, 0)
}

///|
test "update user name (:execrows)" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  let updated = @gen.update_user_name(db, @gen.UpdateUserNameParams::new("Alicia", 1L))
  assert_eq(updated, 1)
  let user = @gen.get_user_by_id(db, @gen.GetUserByIdParams::new(1L))
  assert_eq(user.unwrap().name, "Alicia")
}

// ============================================
// NULL Value Handling Tests
// ============================================

///|
test "update user bio with NULL" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(
    db,
    @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, Some("Original bio")),
  )
  // Update to new value
  let updated = @gen.update_user_bio(db, @gen.UpdateUserBioParams::new(Some("New bio"), 1L))
  assert_eq(updated, 1)
  let user = @gen.get_user_by_id(db, @gen.GetUserByIdParams::new(1L))
  assert_eq(user.unwrap().bio, Some("New bio"))
}

///|
test "get users with and without bio" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, Some("Has bio")))
  @gen.create_user(db, @gen.CreateUserParams::new("Bob", "bob@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Charlie", "charlie@example.com", None, None, 1L, Some("Also has bio")))
  let with_bio = @gen.get_users_with_bio(db)
  assert_eq(with_bio.length(), 2)
  let without_bio = @gen.get_users_without_bio(db)
  assert_eq(without_bio.length(), 1)
  assert_eq(without_bio[0].name, "Bob")
}

// ============================================
// REAL Type Tests
// ============================================

///|
test "update user balance (REAL type)" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, Some(100.0), 1L, None))
  let updated = @gen.update_user_balance(db, @gen.UpdateUserBalanceParams::new(Some(250.75), 1L))
  assert_eq(updated, 1)
  let user = @gen.get_user_by_id(db, @gen.GetUserByIdParams::new(1L))
  assert_eq(user.unwrap().balance, Some(250.75))
}

///|
test "get users with positive balance" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, Some(100.0), 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Bob", "bob@example.com", None, Some(0.0), 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Charlie", "charlie@example.com", None, Some(50.0), 1L, None))
  let users = @gen.get_users_with_positive_balance(db)
  assert_eq(users.length(), 2)
  // Ordered by balance DESC
  assert_eq(users[0].name, "Alice")
  assert_eq(users[1].name, "Charlie")
}

// ============================================
// Boolean Operations Tests
// ============================================

///|
test "activate and deactivate user" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  // Deactivate
  let deactivated = @gen.deactivate_user(db, @gen.DeactivateUserParams::new(1L))
  assert_eq(deactivated, 1)
  let user = @gen.get_user_by_id(db, @gen.GetUserByIdParams::new(1L))
  assert_eq(user.unwrap().is_active, 0L)
  // Activate
  let activated = @gen.activate_user(db, @gen.ActivateUserParams::new(1L))
  assert_eq(activated, 1)
  let user2 = @gen.get_user_by_id(db, @gen.GetUserByIdParams::new(1L))
  assert_eq(user2.unwrap().is_active, 1L)
}

///|
test "list active users only" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Bob", "bob@example.com", None, None, 0L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Charlie", "charlie@example.com", None, None, 1L, None))
  let active = @gen.list_active_users(db)
  assert_eq(active.length(), 2)
}

///|
test "count active users" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Bob", "bob@example.com", None, None, 0L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Charlie", "charlie@example.com", None, None, 1L, None))
  let count = @gen.count_active_users(db)
  assert_eq(count.unwrap().count, 2L)
}

// ============================================
// Search and Pagination Tests
// ============================================

///|
test "search users by name with LIKE" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice Smith", "alice@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Bob Smith", "bob@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Charlie Brown", "charlie@example.com", None, None, 1L, None))
  let smiths = @gen.search_users_by_name(db, @gen.SearchUsersByNameParams::new("%Smith%"))
  assert_eq(smiths.length(), 2)
}

///|
test "list users with limit" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Bob", "bob@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Charlie", "charlie@example.com", None, None, 1L, None))
  let users = @gen.list_users_with_limit(db, @gen.ListUsersWithLimitParams::new(2L))
  assert_eq(users.length(), 2)
}

///|
test "get user count" {
  let db = open_memory_db()
  create_schema(db)
  let count0 = @gen.get_user_count(db)
  assert_eq(count0.unwrap().count, 0L)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Bob", "bob@example.com", None, None, 1L, None))
  let count2 = @gen.get_user_count(db)
  assert_eq(count2.unwrap().count, 2L)
}

// ============================================
// Post CRUD Tests
// ============================================

///|
test "create and get post" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "First Post", "Content here", 0L))
  let post = @gen.get_post_by_id(db, @gen.GetPostByIdParams::new(1L))
  assert_true(post is Some(_))
  let p = post.unwrap()
  assert_eq(p.title, "First Post")
  assert_eq(p.is_published, 0L)
}

///|
test "create post returning id" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  let post_id = @gen.create_post_returning_id(
    db,
    @gen.CreatePostReturningIdParams::new(1L, "First Post", "Content", 0L),
  )
  assert_eq(post_id, 1L)
}

///|
test "publish and delete post" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Draft Post", "Content", 0L))
  // Publish
  let published = @gen.publish_post(db, @gen.PublishPostParams::new(1L))
  assert_eq(published, 1)
  let post = @gen.get_post_by_id(db, @gen.GetPostByIdParams::new(1L))
  assert_eq(post.unwrap().is_published, 1L)
  // Delete
  let deleted = @gen.delete_post(db, @gen.DeletePostParams::new(1L))
  assert_eq(deleted, 1)
  let post2 = @gen.get_post_by_id(db, @gen.GetPostByIdParams::new(1L))
  assert_true(post2 is None)
}

///|
test "list posts by author" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Bob", "bob@example.com", None, None, 1L, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Alice Post 1", "Content", 0L))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Alice Post 2", "Content", 0L))
  @gen.create_post(db, @gen.CreatePostParams::new(2L, "Bob Post 1", "Content", 0L))
  let alice_posts = @gen.list_posts_by_author(db, @gen.ListPostsByAuthorParams::new(1L))
  assert_eq(alice_posts.length(), 2)
  let bob_posts = @gen.list_posts_by_author(db, @gen.ListPostsByAuthorParams::new(2L))
  assert_eq(bob_posts.length(), 1)
}

///|
test "increment view count" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Post", "Content", 0L))
  let post1 = @gen.get_post_by_id(db, @gen.GetPostByIdParams::new(1L))
  assert_eq(post1.unwrap().view_count, 0L)
  @gen.increment_view_count(db, @gen.IncrementViewCountParams::new(1L)) |> ignore
  @gen.increment_view_count(db, @gen.IncrementViewCountParams::new(1L)) |> ignore
  @gen.increment_view_count(db, @gen.IncrementViewCountParams::new(1L)) |> ignore
  let post2 = @gen.get_post_by_id(db, @gen.GetPostByIdParams::new(1L))
  assert_eq(post2.unwrap().view_count, 3L)
}

// ============================================
// JOIN Query Tests
// ============================================

///|
test "get post with author (JOIN)" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Alice Post", "Content", 1L))
  let post_with_author = @gen.get_post_with_author(db, @gen.GetPostWithAuthorParams::new(1L))
  assert_true(post_with_author is Some(_))
  let pwa = post_with_author.unwrap()
  assert_eq(pwa.title, "Alice Post")
  assert_eq(pwa.author_name, "Alice")
  assert_eq(pwa.author_email, "alice@example.com")
}

///|
test "list posts with authors (JOIN)" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Bob", "bob@example.com", None, None, 1L, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Alice Post", "Content", 0L))
  @gen.create_post(db, @gen.CreatePostParams::new(2L, "Bob Post", "Content", 0L))
  let posts = @gen.list_posts_with_authors(db)
  assert_eq(posts.length(), 2)
}

// ============================================
// Aggregation Tests
// ============================================

///|
test "get post count by author" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Post 1", "Content", 0L))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Post 2", "Content", 0L))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Post 3", "Content", 0L))
  let count = @gen.get_post_count_by_author(db, @gen.GetPostCountByAuthorParams::new(1L))
  assert_eq(count.unwrap().post_count, 3L)
}

///|
test "get author stats (COUNT + SUM)" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Post 1", "Content", 0L))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Post 2", "Content", 0L))
  @gen.increment_view_count(db, @gen.IncrementViewCountParams::new(1L)) |> ignore
  @gen.increment_view_count(db, @gen.IncrementViewCountParams::new(1L)) |> ignore
  @gen.increment_view_count(db, @gen.IncrementViewCountParams::new(2L)) |> ignore
  let stats = @gen.get_author_stats(db, @gen.GetAuthorStatsParams::new(1L))
  assert_true(stats is Some(_))
  assert_eq(stats.unwrap().post_count, 2L)
}

// ============================================
// Tag Tests
// ============================================

///|
test "create and list tags" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_tag(db, @gen.CreateTagParams::new("MoonBit"))
  @gen.create_tag(db, @gen.CreateTagParams::new("WASM"))
  @gen.create_tag(db, @gen.CreateTagParams::new("Database"))
  let tags = @gen.list_tags(db)
  assert_eq(tags.length(), 3)
  // Ordered by name
  assert_eq(tags[0].name, "Database")
  assert_eq(tags[1].name, "MoonBit")
  assert_eq(tags[2].name, "WASM")
}

///|
test "create tag returning id" {
  let db = open_memory_db()
  create_schema(db)
  let id1 = @gen.create_tag_returning_id(db, @gen.CreateTagReturningIdParams::new("Tag1"))
  let id2 = @gen.create_tag_returning_id(db, @gen.CreateTagReturningIdParams::new("Tag2"))
  assert_eq(id1, 1L)
  assert_eq(id2, 2L)
}

///|
test "get tag by name" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_tag(db, @gen.CreateTagParams::new("MoonBit"))
  let tag = @gen.get_tag_by_name(db, @gen.GetTagByNameParams::new("MoonBit"))
  assert_true(tag is Some(_))
  assert_eq(tag.unwrap().name, "MoonBit")
  let not_found = @gen.get_tag_by_name(db, @gen.GetTagByNameParams::new("NotExists"))
  assert_true(not_found is None)
}

///|
test "delete tag" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_tag(db, @gen.CreateTagParams::new("ToDelete"))
  let deleted = @gen.delete_tag(db, @gen.DeleteTagParams::new(1L))
  assert_eq(deleted, 1)
  let tag = @gen.get_tag_by_name(db, @gen.GetTagByNameParams::new("ToDelete"))
  assert_true(tag is None)
}

// ============================================
// Post-Tag Relationship Tests
// ============================================

///|
test "add and list tags for post" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Tagged Post", "Content", 0L))
  @gen.create_tag(db, @gen.CreateTagParams::new("MoonBit"))
  @gen.create_tag(db, @gen.CreateTagParams::new("WASM"))
  @gen.add_tag_to_post(db, @gen.AddTagToPostParams::new(1L, 1L))
  @gen.add_tag_to_post(db, @gen.AddTagToPostParams::new(1L, 2L))
  let tags = @gen.list_tags_for_post(db, @gen.ListTagsForPostParams::new(1L))
  assert_eq(tags.length(), 2)
}

///|
test "remove tag from post" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Post", "Content", 0L))
  @gen.create_tag(db, @gen.CreateTagParams::new("Tag1"))
  @gen.create_tag(db, @gen.CreateTagParams::new("Tag2"))
  @gen.add_tag_to_post(db, @gen.AddTagToPostParams::new(1L, 1L))
  @gen.add_tag_to_post(db, @gen.AddTagToPostParams::new(1L, 2L))
  let removed = @gen.remove_tag_from_post(db, @gen.RemoveTagFromPostParams::new(1L, 1L))
  assert_eq(removed, 1)
  let tags = @gen.list_tags_for_post(db, @gen.ListTagsForPostParams::new(1L))
  assert_eq(tags.length(), 1)
  assert_eq(tags[0].name, "Tag2")
}

///|
test "get tag count for post" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Post", "Content", 0L))
  @gen.create_tag(db, @gen.CreateTagParams::new("Tag1"))
  @gen.create_tag(db, @gen.CreateTagParams::new("Tag2"))
  @gen.create_tag(db, @gen.CreateTagParams::new("Tag3"))
  @gen.add_tag_to_post(db, @gen.AddTagToPostParams::new(1L, 1L))
  @gen.add_tag_to_post(db, @gen.AddTagToPostParams::new(1L, 2L))
  @gen.add_tag_to_post(db, @gen.AddTagToPostParams::new(1L, 3L))
  let count = @gen.get_tag_count_for_post(db, @gen.GetTagCountForPostParams::new(1L))
  assert_eq(count.unwrap().tag_count, 3L)
}

// ============================================
// Cursor-based Pagination Tests
// ============================================

///|
test "list users paginated (cursor-based)" {
  let db = open_memory_db()
  create_schema(db)
  // Create 5 users
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Bob", "bob@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Charlie", "charlie@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Diana", "diana@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Eve", "eve@example.com", None, None, 1L, None))
  // First page (cursor=0, limit=2)
  let page1 = @gen.list_users_paginated(db, @gen.ListUsersPaginatedParams::new(0L, 2L))
  assert_eq(page1.length(), 2)
  assert_eq(page1[0].id, 1L)
  assert_eq(page1[1].id, 2L)
  // Second page (cursor=2, limit=2)
  let page2 = @gen.list_users_paginated(db, @gen.ListUsersPaginatedParams::new(2L, 2L))
  assert_eq(page2.length(), 2)
  assert_eq(page2[0].id, 3L)
  assert_eq(page2[1].id, 4L)
  // Third page (cursor=4, limit=2)
  let page3 = @gen.list_users_paginated(db, @gen.ListUsersPaginatedParams::new(4L, 2L))
  assert_eq(page3.length(), 1)
  assert_eq(page3[0].id, 5L)
  // Beyond last page
  let page4 = @gen.list_users_paginated(db, @gen.ListUsersPaginatedParams::new(5L, 2L))
  assert_eq(page4.length(), 0)
}

///|
test "list posts by author paginated (cursor-based)" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  // Create 4 posts for Alice
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Post 1", "Content", 0L))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Post 2", "Content", 0L))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Post 3", "Content", 0L))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Post 4", "Content", 0L))
  // First page
  let page1 = @gen.list_posts_by_author_paginated(db, @gen.ListPostsByAuthorPaginatedParams::new(1L, 0L, 2L))
  assert_eq(page1.length(), 2)
  assert_eq(page1[0].title, "Post 1")
  assert_eq(page1[1].title, "Post 2")
  // Second page
  let page2 = @gen.list_posts_by_author_paginated(db, @gen.ListPostsByAuthorPaginatedParams::new(1L, 2L, 2L))
  assert_eq(page2.length(), 2)
  assert_eq(page2[0].title, "Post 3")
  assert_eq(page2[1].title, "Post 4")
  // Beyond last page
  let page3 = @gen.list_posts_by_author_paginated(db, @gen.ListPostsByAuthorPaginatedParams::new(1L, 4L, 2L))
  assert_eq(page3.length(), 0)
}

///|
test "list posts with authors paginated (JOIN + cursor-based)" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  @gen.create_user(db, @gen.CreateUserParams::new("Bob", "bob@example.com", None, None, 1L, None))
  // Create posts alternating authors
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Alice Post 1", "Content", 0L))
  @gen.create_post(db, @gen.CreatePostParams::new(2L, "Bob Post 1", "Content", 0L))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Alice Post 2", "Content", 0L))
  @gen.create_post(db, @gen.CreatePostParams::new(2L, "Bob Post 2", "Content", 0L))
  // First page
  let page1 = @gen.list_posts_with_authors_paginated(db, @gen.ListPostsWithAuthorsPaginatedParams::new(0L, 2L))
  assert_eq(page1.length(), 2)
  assert_eq(page1[0].author_name, "Alice")
  assert_eq(page1[1].author_name, "Bob")
  // Second page
  let page2 = @gen.list_posts_with_authors_paginated(db, @gen.ListPostsWithAuthorsPaginatedParams::new(2L, 2L))
  assert_eq(page2.length(), 2)
  assert_eq(page2[0].author_name, "Alice")
  assert_eq(page2[1].author_name, "Bob")
}

///|
test "list published posts paginated (cursor-based)" {
  let db = open_memory_db()
  create_schema(db)
  @gen.create_user(db, @gen.CreateUserParams::new("Alice", "alice@example.com", None, None, 1L, None))
  // Create posts with mixed published status
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Draft 1", "Content", 0L))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Published 1", "Content", 1L))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Draft 2", "Content", 0L))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Published 2", "Content", 1L))
  @gen.create_post(db, @gen.CreatePostParams::new(1L, "Published 3", "Content", 1L))
  // First page - only published posts
  let page1 = @gen.list_published_posts_paginated(db, @gen.ListPublishedPostsPaginatedParams::new(0L, 2L))
  assert_eq(page1.length(), 2)
  assert_eq(page1[0].title, "Published 1")
  assert_eq(page1[1].title, "Published 2")
  // Second page
  let page2 = @gen.list_published_posts_paginated(db, @gen.ListPublishedPostsPaginatedParams::new(4L, 2L))
  assert_eq(page2.length(), 1)
  assert_eq(page2[0].title, "Published 3")
}

// ============================================
// Edge Case Tests
// ============================================

///|
test "empty table operations" {
  let db = open_memory_db()
  create_schema(db)
  let users = @gen.list_users(db)
  assert_eq(users.length(), 0)
  let count = @gen.get_user_count(db)
  assert_eq(count.unwrap().count, 0L)
  let active = @gen.count_active_users(db)
  assert_eq(active.unwrap().count, 0L)
}
