// Tests for generated validators

///|
test "CreateUserParams::validate - valid params" {
  let params = CreateUserParams::new(
    "alice",
    "alice@example.com",
    "Alice Developer",
    Some("A developer"),
    None,
  )
  inspect(params.validate(), content="Ok(())")
}

///|
test "CreateUserParams::validate - empty username" {
  let params = CreateUserParams::new(
    "",
    "alice@example.com",
    "Alice Developer",
    None,
    None,
  )
  inspect(params.validate(), content="Err(\"username is required\")")
}

///|
test "CreateUserParams::validate - empty email" {
  let params = CreateUserParams::new("alice", "", "Alice Developer", None, None)
  inspect(params.validate(), content="Err(\"email is required\")")
}

///|
test "CreateUserParams::validate - empty display_name" {
  let params = CreateUserParams::new(
    "alice",
    "alice@example.com",
    "",
    None,
    None,
  )
  inspect(params.validate(), content="Err(\"display_name is required\")")
}

///|
test "GetUserByUsernameParams::validate - valid" {
  let params = GetUserByUsernameParams::new("alice")
  inspect(params.validate(), content="Ok(())")
}

///|
test "GetUserByUsernameParams::validate - empty username" {
  let params = GetUserByUsernameParams::new("")
  inspect(params.validate(), content="Err(\"username is required\")")
}

///|
test "CreatePostParams::validate - valid params" {
  let params = CreatePostParams::new(
    1L,
    Some(1L),
    "My First Post",
    "my-first-post",
    "This is the content of my first post.",
    Some("A brief excerpt"),
    "draft",
  )
  inspect(params.validate(), content="Ok(())")
}

///|
test "CreatePostParams::validate - empty title" {
  let params = CreatePostParams::new(
    1L,
    None,
    "",
    "my-first-post",
    "Content here",
    None,
    "draft",
  )
  inspect(params.validate(), content="Err(\"title is required\")")
}

///|
test "CreatePostParams::validate - empty slug" {
  let params = CreatePostParams::new(
    1L,
    None,
    "My Post",
    "",
    "Content here",
    None,
    "draft",
  )
  inspect(params.validate(), content="Err(\"slug is required\")")
}

///|
test "CreatePostParams::validate - empty content" {
  let params = CreatePostParams::new(
    1L,
    None,
    "My Post",
    "my-post",
    "",
    None,
    "draft",
  )
  inspect(params.validate(), content="Err(\"content is required\")")
}

///|
test "CreatePostParams::validate - empty status" {
  let params = CreatePostParams::new(
    1L,
    None,
    "My Post",
    "my-post",
    "Content here",
    None,
    "",
  )
  inspect(params.validate(), content="Err(\"status is required\")")
}

///|
test "CreateTagParams::validate - valid" {
  let params = CreateTagParams::new("moonbit", "moonbit")
  inspect(params.validate(), content="Ok(())")
}

///|
test "CreateTagParams::validate - empty name" {
  let params = CreateTagParams::new("", "moonbit")
  inspect(params.validate(), content="Err(\"name is required\")")
}

///|
test "CreateTagParams::validate - empty slug" {
  let params = CreateTagParams::new("moonbit", "")
  inspect(params.validate(), content="Err(\"slug is required\")")
}

///|
test "CreateCommentParams::validate - valid" {
  let params = CreateCommentParams::new(
    1L, "John Doe", "john@example.com", "Great post!",
  )
  inspect(params.validate(), content="Ok(())")
}

///|
test "CreateCommentParams::validate - empty author_name" {
  let params = CreateCommentParams::new(
    1L, "", "john@example.com", "Great post!",
  )
  inspect(params.validate(), content="Err(\"author_name is required\")")
}

///|
test "CreateCommentParams::validate - empty author_email" {
  let params = CreateCommentParams::new(1L, "John Doe", "", "Great post!")
  inspect(params.validate(), content="Err(\"author_email is required\")")
}

///|
test "CreateCommentParams::validate - empty content" {
  let params = CreateCommentParams::new(1L, "John Doe", "john@example.com", "")
  inspect(params.validate(), content="Err(\"content is required\")")
}

///|
test "CreateCategoryParams::validate - valid" {
  let params = CreateCategoryParams::new(
    "tech",
    "tech",
    Some("Technology articles"),
  )
  inspect(params.validate(), content="Ok(())")
}

///|
test "CreateCategoryParams::validate - empty name" {
  let params = CreateCategoryParams::new("", "tech", None)
  inspect(params.validate(), content="Err(\"name is required\")")
}

///|
test "CreateCategoryParams::validate - empty slug" {
  let params = CreateCategoryParams::new("tech", "", None)
  inspect(params.validate(), content="Err(\"slug is required\")")
}

///|
test "GetPostBySlugParams::validate - valid" {
  let params = GetPostBySlugParams::new("my-post")
  inspect(params.validate(), content="Ok(())")
}

///|
test "GetPostBySlugParams::validate - empty slug" {
  let params = GetPostBySlugParams::new("")
  inspect(params.validate(), content="Err(\"slug is required\")")
}

// Tests for Int64-only params (should always pass validation)

///|
test "GetUserByIdParams::validate - always valid" {
  let params = GetUserByIdParams::new(1L)
  inspect(params.validate(), content="Ok(())")
}

///|
test "GetCategoryByIdParams::validate - always valid" {
  let params = GetCategoryByIdParams::new(1L)
  inspect(params.validate(), content="Ok(())")
}

///|
test "GetPostByIdParams::validate - always valid" {
  let params = GetPostByIdParams::new(1L)
  inspect(params.validate(), content="Ok(())")
}

///|
test "ListPublishedPostsParams::validate - always valid" {
  let params = ListPublishedPostsParams::new(10L)
  inspect(params.validate(), content="Ok(())")
}

///|
test "ListPostsByAuthorParams::validate - always valid" {
  let params = ListPostsByAuthorParams::new(1L)
  inspect(params.validate(), content="Ok(())")
}

///|
test "PublishPostParams::validate - always valid" {
  let params = PublishPostParams::new(1L)
  inspect(params.validate(), content="Ok(())")
}

///|
test "DeletePostParams::validate - always valid" {
  let params = DeletePostParams::new(1L)
  inspect(params.validate(), content="Ok(())")
}

///|
test "ListCommentsForPostParams::validate - always valid" {
  let params = ListCommentsForPostParams::new(1L)
  inspect(params.validate(), content="Ok(())")
}
