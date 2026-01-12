// Generated SQL queries (extracted from sqlc-gen-moonbit output)
// This file is used for testing D1 queries with vitest

export const SQL = {
  getUserById: `
SELECT id, username, email, display_name, bio, avatar_url, created_at, updated_at FROM users WHERE id = ?`,

  getUserByUsername: `SELECT id, username, email, display_name, bio, avatar_url, created_at, updated_at FROM users WHERE username = ?`,

  listUsers: `SELECT id, username, email, display_name, bio, avatar_url, created_at, updated_at FROM users ORDER BY created_at DESC`,

  createUser: `INSERT INTO users (username, email, display_name, bio, avatar_url)
VALUES (?, ?, ?, ?, ?)`,

  getCategoryById: `
SELECT id, name, slug, description FROM categories WHERE id = ?`,

  listCategories: `SELECT id, name, slug, description FROM categories ORDER BY name`,

  createCategory: `INSERT INTO categories (name, slug, description) VALUES (?, ?, ?)`,

  getPostById: `
SELECT id, author_id, category_id, title, slug, content, excerpt, status, published_at, created_at, updated_at FROM posts WHERE id = ?`,

  getPostBySlug: `SELECT id, author_id, category_id, title, slug, content, excerpt, status, published_at, created_at, updated_at FROM posts WHERE slug = ?`,

  listPublishedPosts: `SELECT id, author_id, category_id, title, slug, content, excerpt, status, published_at, created_at, updated_at FROM posts
WHERE status = 'published'
ORDER BY published_at DESC
LIMIT ?`,

  listPostsByAuthor: `SELECT id, author_id, category_id, title, slug, content, excerpt, status, published_at, created_at, updated_at FROM posts
WHERE author_id = ?
ORDER BY created_at DESC`,

  createPost: `INSERT INTO posts (author_id, category_id, title, slug, content, excerpt, status)
VALUES (?, ?, ?, ?, ?, ?, ?)`,

  publishPost: `UPDATE posts
SET status = 'published', published_at = datetime('now'), updated_at = datetime('now')
WHERE id = ?`,

  deletePost: `DELETE FROM posts WHERE id = ?`,

  listTags: `
SELECT id, name, slug FROM tags ORDER BY name`,

  createTag: `INSERT INTO tags (name, slug) VALUES (?, ?)`,

  listCommentsForPost: `
SELECT id, post_id, author_name, author_email, content, is_approved, created_at FROM comments
WHERE post_id = ? AND is_approved = 1
ORDER BY created_at ASC`,

  createComment: `INSERT INTO comments (post_id, author_name, author_email, content)
VALUES (?, ?, ?, ?)`,
};

// Types (matching MoonBit generated types)
export interface User {
  id: number;
  username: string;
  email: string;
  display_name: string;
  bio: string | null;
  avatar_url: string | null;
  created_at: string;
  updated_at: string;
}

export interface Category {
  id: number;
  name: string;
  slug: string;
  description: string | null;
}

export interface Post {
  id: number;
  author_id: number;
  category_id: number | null;
  title: string;
  slug: string;
  content: string;
  excerpt: string | null;
  status: string;
  published_at: string | null;
  created_at: string;
  updated_at: string;
}

export interface Tag {
  id: number;
  name: string;
  slug: string;
}

export interface Comment {
  id: number;
  post_id: number;
  author_name: string;
  author_email: string;
  content: string;
  is_approved: number;
  created_at: string;
}

// Query functions
export async function createUser(
  db: D1Database,
  username: string,
  email: string,
  displayName: string,
  bio: string | null,
  avatarUrl: string | null
): Promise<D1Result> {
  return db.prepare(SQL.createUser).bind(username, email, displayName, bio, avatarUrl).run();
}

export async function getUserById(db: D1Database, id: number): Promise<User | null> {
  return db.prepare(SQL.getUserById).bind(id).first<User>();
}

export async function getUserByUsername(db: D1Database, username: string): Promise<User | null> {
  return db.prepare(SQL.getUserByUsername).bind(username).first<User>();
}

export async function listUsers(db: D1Database): Promise<User[]> {
  const result = await db.prepare(SQL.listUsers).all<User>();
  return result.results;
}

export async function createCategory(
  db: D1Database,
  name: string,
  slug: string,
  description: string | null
): Promise<D1Result> {
  return db.prepare(SQL.createCategory).bind(name, slug, description).run();
}

export async function getCategoryById(db: D1Database, id: number): Promise<Category | null> {
  return db.prepare(SQL.getCategoryById).bind(id).first<Category>();
}

export async function listCategories(db: D1Database): Promise<Category[]> {
  const result = await db.prepare(SQL.listCategories).all<Category>();
  return result.results;
}

export async function createPost(
  db: D1Database,
  authorId: number,
  categoryId: number | null,
  title: string,
  slug: string,
  content: string,
  excerpt: string | null,
  status: string
): Promise<D1Result> {
  return db.prepare(SQL.createPost).bind(authorId, categoryId, title, slug, content, excerpt, status).run();
}

export async function getPostById(db: D1Database, id: number): Promise<Post | null> {
  return db.prepare(SQL.getPostById).bind(id).first<Post>();
}

export async function getPostBySlug(db: D1Database, slug: string): Promise<Post | null> {
  return db.prepare(SQL.getPostBySlug).bind(slug).first<Post>();
}

export async function listPublishedPosts(db: D1Database, limit: number): Promise<Post[]> {
  const result = await db.prepare(SQL.listPublishedPosts).bind(limit).all<Post>();
  return result.results;
}

export async function listPostsByAuthor(db: D1Database, authorId: number): Promise<Post[]> {
  const result = await db.prepare(SQL.listPostsByAuthor).bind(authorId).all<Post>();
  return result.results;
}

export async function publishPost(db: D1Database, id: number): Promise<D1Result> {
  return db.prepare(SQL.publishPost).bind(id).run();
}

export async function deletePost(db: D1Database, id: number): Promise<D1Result> {
  return db.prepare(SQL.deletePost).bind(id).run();
}

export async function createTag(db: D1Database, name: string, slug: string): Promise<D1Result> {
  return db.prepare(SQL.createTag).bind(name, slug).run();
}

export async function listTags(db: D1Database): Promise<Tag[]> {
  const result = await db.prepare(SQL.listTags).all<Tag>();
  return result.results;
}

export async function createComment(
  db: D1Database,
  postId: number,
  authorName: string,
  authorEmail: string,
  content: string
): Promise<D1Result> {
  return db.prepare(SQL.createComment).bind(postId, authorName, authorEmail, content).run();
}

export async function listCommentsForPost(db: D1Database, postId: number): Promise<Comment[]> {
  const result = await db.prepare(SQL.listCommentsForPost).bind(postId).all<Comment>();
  return result.results;
}
