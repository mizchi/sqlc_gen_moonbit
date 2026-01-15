-- ============================================
-- Basic User CRUD
-- ============================================

-- name: GetUserById :one
SELECT * FROM users WHERE id = $1;

-- name: GetUserByEmail :one
SELECT * FROM users WHERE email = $1;

-- name: ListUsers :many
SELECT * FROM users ORDER BY name;

-- name: ListActiveUsers :many
SELECT * FROM users WHERE is_active = TRUE ORDER BY name;

-- name: CreateUser :exec
INSERT INTO users (name, email, age, balance, is_active, bio)
VALUES ($1, $2, $3, $4, $5, $6);

-- name: CreateUserReturningId :execlastid
INSERT INTO users (name, email, age, balance, is_active, bio)
VALUES ($1, $2, $3, $4, $5, $6)
RETURNING id;

-- name: DeleteUserById :execrows
DELETE FROM users WHERE id = $1;

-- name: UpdateUserName :execrows
UPDATE users SET name = $2 WHERE id = $1;

-- ============================================
-- NULL value handling
-- ============================================

-- name: UpdateUserBio :execrows
UPDATE users SET bio = $2 WHERE id = $1;

-- name: GetUsersWithBio :many
SELECT * FROM users WHERE bio IS NOT NULL ORDER BY name;

-- name: GetUsersWithoutBio :many
SELECT * FROM users WHERE bio IS NULL ORDER BY name;

-- ============================================
-- NUMERIC type operations
-- ============================================

-- name: UpdateUserBalance :execrows
UPDATE users SET balance = $2 WHERE id = $1;

-- name: GetUsersWithPositiveBalance :many
SELECT * FROM users WHERE balance > 0.0 ORDER BY balance DESC;

-- ============================================
-- Boolean operations
-- ============================================

-- name: ActivateUser :execrows
UPDATE users SET is_active = TRUE WHERE id = $1;

-- name: DeactivateUser :execrows
UPDATE users SET is_active = FALSE WHERE id = $1;

-- name: CountActiveUsers :one
SELECT COUNT(*) as count FROM users WHERE is_active = TRUE;

-- ============================================
-- Search and pagination
-- ============================================

-- name: SearchUsersByName :many
SELECT * FROM users WHERE name LIKE $1 ORDER BY name;

-- name: ListUsersWithLimit :many
SELECT * FROM users ORDER BY id LIMIT $1;

-- name: GetUserCount :one
SELECT COUNT(*) as count FROM users;

-- ============================================
-- Post CRUD
-- ============================================

-- name: CreatePost :exec
INSERT INTO posts (author_id, title, content, is_published)
VALUES ($1, $2, $3, $4);

-- name: CreatePostReturningId :execlastid
INSERT INTO posts (author_id, title, content, is_published)
VALUES ($1, $2, $3, $4)
RETURNING id;

-- name: GetPostById :one
SELECT * FROM posts WHERE id = $1;

-- name: ListPostsByAuthor :many
SELECT * FROM posts WHERE author_id = $1 ORDER BY created_at DESC;

-- name: PublishPost :execrows
UPDATE posts SET is_published = TRUE, published_at = CURRENT_TIMESTAMP WHERE id = $1;

-- name: DeletePost :execrows
DELETE FROM posts WHERE id = $1;

-- name: IncrementViewCount :execrows
UPDATE posts SET view_count = view_count + 1 WHERE id = $1;

-- ============================================
-- JOIN queries
-- ============================================

-- name: GetPostWithAuthor :one
SELECT
  p.id, p.title, p.content, p.view_count, p.is_published,
  p.published_at, p.created_at,
  u.id as author_id, u.name as author_name, u.email as author_email
FROM posts p
INNER JOIN users u ON p.author_id = u.id
WHERE p.id = $1;

-- name: ListPostsWithAuthors :many
SELECT
  p.id, p.title, p.view_count, p.is_published, p.created_at,
  u.id as author_id, u.name as author_name
FROM posts p
INNER JOIN users u ON p.author_id = u.id
ORDER BY p.created_at DESC;

-- ============================================
-- Aggregation queries
-- ============================================

-- name: GetPostCountByAuthor :one
SELECT COUNT(*) as post_count FROM posts WHERE author_id = $1;

-- name: GetAuthorStats :one
SELECT
  COUNT(*) as post_count,
  COALESCE(SUM(view_count), 0) as total_views
FROM posts WHERE author_id = $1;

-- ============================================
-- Tag operations
-- ============================================

-- name: CreateTag :exec
INSERT INTO tags (name) VALUES ($1);

-- name: CreateTagReturningId :execlastid
INSERT INTO tags (name) VALUES ($1)
RETURNING id;

-- name: GetTagByName :one
SELECT * FROM tags WHERE name = $1;

-- name: ListTags :many
SELECT * FROM tags ORDER BY name;

-- name: DeleteTag :execrows
DELETE FROM tags WHERE id = $1;

-- ============================================
-- Post-Tag relationship
-- ============================================

-- name: AddTagToPost :exec
INSERT INTO post_tags (post_id, tag_id) VALUES ($1, $2);

-- name: RemoveTagFromPost :execrows
DELETE FROM post_tags WHERE post_id = $1 AND tag_id = $2;

-- name: ListTagsForPost :many
SELECT t.id, t.name
FROM tags t
INNER JOIN post_tags pt ON t.id = pt.tag_id
WHERE pt.post_id = $1
ORDER BY t.name;

-- name: GetTagCountForPost :one
SELECT COUNT(*) as tag_count FROM post_tags WHERE post_id = $1;

-- ============================================
-- Cursor-based pagination
-- ============================================

-- name: ListUsersPaginated :many
SELECT * FROM users WHERE id > $1 ORDER BY id LIMIT $2;

-- name: ListPostsByAuthorPaginated :many
SELECT * FROM posts WHERE author_id = $1 AND id > $2 ORDER BY id LIMIT $3;

-- name: ListPostsWithAuthorsPaginated :many
SELECT
  p.id, p.title, p.view_count, p.is_published, p.created_at,
  u.id as author_id, u.name as author_name
FROM posts p
INNER JOIN users u ON p.author_id = u.id
WHERE p.id > $1
ORDER BY p.id
LIMIT $2;

-- name: ListPublishedPostsPaginated :many
SELECT * FROM posts WHERE is_published = TRUE AND id > $1 ORDER BY id LIMIT $2;
