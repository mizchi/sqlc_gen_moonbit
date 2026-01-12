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
