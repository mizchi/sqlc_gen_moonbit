-- ============================================
-- Basic User CRUD
-- ============================================

-- name: GetUserById :one
SELECT * FROM users WHERE id = ?;

-- name: GetUserByEmail :one
SELECT * FROM users WHERE email = ?;

-- name: ListUsers :many
SELECT * FROM users ORDER BY name;

-- name: ListActiveUsers :many
SELECT * FROM users WHERE is_active = 1 ORDER BY name;

-- name: CreateUser :exec
INSERT INTO users (name, email, age, balance, is_active, bio)
VALUES (?, ?, ?, ?, ?, ?);

-- name: CreateUserReturningId :execlastid
INSERT INTO users (name, email, age, balance, is_active, bio)
VALUES (?, ?, ?, ?, ?, ?);

-- name: DeleteUserById :execrows
DELETE FROM users WHERE id = ?;

-- name: UpdateUserName :execrows
UPDATE users SET name = ? WHERE id = ?;

-- ============================================
-- NULL value handling
-- ============================================

-- name: UpdateUserBio :execrows
UPDATE users SET bio = ? WHERE id = ?;

-- name: GetUsersWithBio :many
SELECT * FROM users WHERE bio IS NOT NULL ORDER BY name;

-- name: GetUsersWithoutBio :many
SELECT * FROM users WHERE bio IS NULL ORDER BY name;

-- ============================================
-- REAL type operations
-- ============================================

-- name: UpdateUserBalance :execrows
UPDATE users SET balance = ? WHERE id = ?;

-- name: GetUsersWithPositiveBalance :many
SELECT * FROM users WHERE balance > 0.0 ORDER BY balance DESC;

-- ============================================
-- Boolean operations
-- ============================================

-- name: ActivateUser :execrows
UPDATE users SET is_active = 1 WHERE id = ?;

-- name: DeactivateUser :execrows
UPDATE users SET is_active = 0 WHERE id = ?;

-- name: CountActiveUsers :one
SELECT COUNT(*) as count FROM users WHERE is_active = 1;

-- ============================================
-- Search and pagination
-- ============================================

-- name: SearchUsersByName :many
SELECT * FROM users WHERE name LIKE ? ORDER BY name;

-- name: ListUsersWithLimit :many
SELECT * FROM users ORDER BY id LIMIT ?;

-- name: GetUserCount :one
SELECT COUNT(*) as count FROM users;

-- ============================================
-- Post CRUD
-- ============================================

-- name: CreatePost :exec
INSERT INTO posts (author_id, title, content, is_published)
VALUES (?, ?, ?, ?);

-- name: CreatePostReturningId :execlastid
INSERT INTO posts (author_id, title, content, is_published)
VALUES (?, ?, ?, ?);

-- name: GetPostById :one
SELECT * FROM posts WHERE id = ?;

-- name: ListPostsByAuthor :many
SELECT * FROM posts WHERE author_id = ? ORDER BY created_at DESC;

-- name: PublishPost :execrows
UPDATE posts SET is_published = 1, published_at = datetime('now') WHERE id = ?;

-- name: DeletePost :execrows
DELETE FROM posts WHERE id = ?;

-- name: IncrementViewCount :execrows
UPDATE posts SET view_count = view_count + 1 WHERE id = ?;

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
WHERE p.id = ?;

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
SELECT COUNT(*) as post_count FROM posts WHERE author_id = ?;

-- name: GetAuthorStats :one
SELECT
  COUNT(*) as post_count,
  COALESCE(SUM(view_count), 0) as total_views
FROM posts WHERE author_id = ?;

-- ============================================
-- Tag operations
-- ============================================

-- name: CreateTag :exec
INSERT INTO tags (name) VALUES (?);

-- name: CreateTagReturningId :execlastid
INSERT INTO tags (name) VALUES (?);

-- name: GetTagByName :one
SELECT * FROM tags WHERE name = ?;

-- name: ListTags :many
SELECT * FROM tags ORDER BY name;

-- name: DeleteTag :execrows
DELETE FROM tags WHERE id = ?;

-- ============================================
-- Post-Tag relationship
-- ============================================

-- name: AddTagToPost :exec
INSERT INTO post_tags (post_id, tag_id) VALUES (?, ?);

-- name: RemoveTagFromPost :execrows
DELETE FROM post_tags WHERE post_id = ? AND tag_id = ?;

-- name: ListTagsForPost :many
SELECT t.id, t.name
FROM tags t
INNER JOIN post_tags pt ON t.id = pt.tag_id
WHERE pt.post_id = ?
ORDER BY t.name;

-- name: GetTagCountForPost :one
SELECT COUNT(*) as tag_count FROM post_tags WHERE post_id = ?;
