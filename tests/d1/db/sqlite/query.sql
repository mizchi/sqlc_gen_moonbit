-- ============================================
-- User Queries
-- ============================================

-- name: GetUserById :one
SELECT * FROM users WHERE id = ?;

-- name: GetUserByUsername :one
SELECT * FROM users WHERE username = ?;

-- name: ListUsers :many
SELECT * FROM users ORDER BY created_at DESC;

-- name: CreateUser :exec
INSERT INTO users (username, email, display_name, bio, avatar_url)
VALUES (?, ?, ?, ?, ?);

-- name: CreateUserReturningId :execlastid
INSERT INTO users (username, email, display_name, bio, avatar_url)
VALUES (?, ?, ?, ?, ?);

-- name: DeleteUserById :execrows
DELETE FROM users WHERE id = ?;

-- name: UpdateUserDisplayName :execrows
UPDATE users SET display_name = ? WHERE id = ?;

-- ============================================
-- Category Queries
-- ============================================

-- name: GetCategoryById :one
SELECT * FROM categories WHERE id = ?;

-- name: ListCategories :many
SELECT * FROM categories ORDER BY name;

-- name: CreateCategory :exec
INSERT INTO categories (name, slug, description) VALUES (?, ?, ?);

-- ============================================
-- Post Queries
-- ============================================

-- name: GetPostById :one
SELECT * FROM posts WHERE id = ?;

-- name: GetPostBySlug :one
SELECT * FROM posts WHERE slug = ?;

-- name: ListPublishedPosts :many
SELECT * FROM posts
WHERE status = 'published'
ORDER BY published_at DESC
LIMIT ?;

-- name: ListPostsByAuthor :many
SELECT * FROM posts
WHERE author_id = ?
ORDER BY created_at DESC;

-- name: CreatePost :exec
INSERT INTO posts (author_id, category_id, title, slug, content, excerpt, status)
VALUES (?, ?, ?, ?, ?, ?, ?);

-- name: PublishPost :exec
UPDATE posts
SET status = 'published', published_at = datetime('now'), updated_at = datetime('now')
WHERE id = ?;

-- name: DeletePost :exec
DELETE FROM posts WHERE id = ?;

-- ============================================
-- Tag Queries
-- ============================================

-- name: ListTags :many
SELECT * FROM tags ORDER BY name;

-- name: CreateTag :exec
INSERT INTO tags (name, slug) VALUES (?, ?);

-- ============================================
-- Comment Queries
-- ============================================

-- name: ListCommentsForPost :many
SELECT * FROM comments
WHERE post_id = ? AND is_approved = 1
ORDER BY created_at ASC;

-- name: CreateComment :exec
INSERT INTO comments (post_id, author_name, author_email, content)
VALUES (?, ?, ?, ?);

-- ============================================
-- JOIN Queries
-- ============================================

-- name: GetPostWithAuthor :one
SELECT
  p.id, p.title, p.slug, p.content, p.excerpt, p.status,
  p.published_at, p.created_at, p.updated_at,
  u.id as author_id, u.username as author_username,
  u.display_name as author_display_name, u.avatar_url as author_avatar_url
FROM posts p
INNER JOIN users u ON p.author_id = u.id
WHERE p.id = ?;

-- name: GetPostWithAuthorBySlug :one
SELECT
  p.id, p.title, p.slug, p.content, p.excerpt, p.status,
  p.published_at, p.created_at, p.updated_at,
  u.id as author_id, u.username as author_username,
  u.display_name as author_display_name, u.avatar_url as author_avatar_url
FROM posts p
INNER JOIN users u ON p.author_id = u.id
WHERE p.slug = ?;

-- name: ListPostsWithAuthors :many
SELECT
  p.id, p.title, p.slug, p.excerpt, p.status,
  p.published_at, p.created_at,
  u.id as author_id, u.username as author_username,
  u.display_name as author_display_name
FROM posts p
INNER JOIN users u ON p.author_id = u.id
WHERE p.status = 'published'
ORDER BY p.published_at DESC
LIMIT ?;

-- name: GetPostWithCategory :one
SELECT
  p.id, p.title, p.slug, p.content, p.status,
  c.id as category_id, c.name as category_name, c.slug as category_slug
FROM posts p
LEFT JOIN categories c ON p.category_id = c.id
WHERE p.id = ?;

-- name: ListPostsWithDetails :many
SELECT
  p.id, p.title, p.slug, p.excerpt, p.status, p.published_at,
  u.id as author_id, u.username as author_username, u.display_name as author_display_name,
  c.id as category_id, c.name as category_name
FROM posts p
INNER JOIN users u ON p.author_id = u.id
LEFT JOIN categories c ON p.category_id = c.id
WHERE p.status = 'published'
ORDER BY p.published_at DESC
LIMIT ?;

-- name: ListTagsForPost :many
SELECT t.id, t.name, t.slug
FROM tags t
INNER JOIN post_tags pt ON t.id = pt.tag_id
WHERE pt.post_id = ?
ORDER BY t.name;

-- name: ListPostsWithTagCount :many
SELECT
  p.id, p.title, p.slug, p.status,
  COUNT(pt.tag_id) as tag_count
FROM posts p
LEFT JOIN post_tags pt ON p.id = pt.post_id
GROUP BY p.id
ORDER BY tag_count DESC
LIMIT ?;

-- name: GetPostCommentsWithCount :one
SELECT
  p.id, p.title,
  COUNT(c.id) as comment_count,
  SUM(CASE WHEN c.is_approved = 1 THEN 1 ELSE 0 END) as approved_count
FROM posts p
LEFT JOIN comments c ON p.id = c.post_id
WHERE p.id = ?
GROUP BY p.id;

-- ============================================
-- Batch Operations (using post_tags junction table)
-- ============================================

-- name: AddTagToPost :exec
INSERT INTO post_tags (post_id, tag_id) VALUES (?, ?);

-- name: RemoveTagFromPost :exec
DELETE FROM post_tags WHERE post_id = ? AND tag_id = ?;

-- name: RemoveAllTagsFromPost :exec
DELETE FROM post_tags WHERE post_id = ?;

-- name: GetOrCreateTag :one
INSERT INTO tags (name, slug) VALUES (?, ?)
ON CONFLICT (slug) DO UPDATE SET name = excluded.name
RETURNING *;

-- name: ApproveComment :exec
UPDATE comments SET is_approved = 1 WHERE id = ?;

-- name: ApproveAllCommentsForPost :exec
UPDATE comments SET is_approved = 1 WHERE post_id = ?;

-- name: DeleteAllCommentsForPost :exec
DELETE FROM comments WHERE post_id = ?;

-- ============================================
-- Cursor-based Pagination
-- ============================================

-- name: ListUsersPaginated :many
SELECT * FROM users WHERE id > ? ORDER BY id LIMIT ?;

-- name: ListPostsPaginated :many
SELECT * FROM posts WHERE id > ? ORDER BY id LIMIT ?;

-- name: ListPublishedPostsPaginated :many
SELECT * FROM posts
WHERE status = 'published' AND id > ?
ORDER BY id
LIMIT ?;

-- name: ListPostsByAuthorPaginated :many
SELECT * FROM posts
WHERE author_id = ? AND id > ?
ORDER BY id
LIMIT ?;

-- name: ListPostsWithAuthorsPaginated :many
SELECT
  p.id, p.title, p.slug, p.excerpt, p.status,
  p.published_at, p.created_at,
  u.id as author_id, u.username as author_username,
  u.display_name as author_display_name
FROM posts p
INNER JOIN users u ON p.author_id = u.id
WHERE p.status = 'published' AND p.id > ?
ORDER BY p.id
LIMIT ?;
