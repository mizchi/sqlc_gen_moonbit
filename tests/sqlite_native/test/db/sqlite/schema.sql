-- Users table with various column types
CREATE TABLE users (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE,
  age INTEGER,
  balance REAL DEFAULT 0.0,
  is_active INTEGER NOT NULL DEFAULT 1,
  bio TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- Posts table for JOIN tests
CREATE TABLE posts (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  author_id INTEGER NOT NULL REFERENCES users(id),
  title TEXT NOT NULL,
  content TEXT NOT NULL,
  view_count INTEGER NOT NULL DEFAULT 0,
  is_published INTEGER NOT NULL DEFAULT 0,
  published_at TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- Tags table for many-to-many relationship
CREATE TABLE tags (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE
);

-- Junction table for posts and tags
CREATE TABLE post_tags (
  post_id INTEGER NOT NULL REFERENCES posts(id) ON DELETE CASCADE,
  tag_id INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE,
  PRIMARY KEY (post_id, tag_id)
);

-- ============================================
-- Indexes for query optimization
-- ============================================

-- Optimize ListPostsByAuthor
CREATE INDEX idx_posts_author_id ON posts(author_id);

-- Optimize ORDER BY created_at DESC
CREATE INDEX idx_posts_created_at ON posts(created_at DESC);

-- Optimize published posts queries (partial index)
CREATE INDEX idx_posts_is_published ON posts(is_published) WHERE is_published = 1;

-- Optimize active user queries
CREATE INDEX idx_users_is_active ON users(is_active);

-- Optimize ORDER BY name
CREATE INDEX idx_users_name ON users(name);
