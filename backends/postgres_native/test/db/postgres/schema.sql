-- Users table with various column types
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  email VARCHAR(255) NOT NULL UNIQUE,
  age INTEGER,
  balance NUMERIC(10,2) DEFAULT 0.0,
  is_active BOOLEAN NOT NULL DEFAULT TRUE,
  bio TEXT,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Posts table for JOIN tests
CREATE TABLE posts (
  id SERIAL PRIMARY KEY,
  author_id INTEGER NOT NULL REFERENCES users(id),
  title VARCHAR(255) NOT NULL,
  content TEXT NOT NULL,
  view_count INTEGER NOT NULL DEFAULT 0,
  is_published BOOLEAN NOT NULL DEFAULT FALSE,
  published_at TIMESTAMP,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Tags table for many-to-many relationship
CREATE TABLE tags (
  id SERIAL PRIMARY KEY,
  name VARCHAR(100) NOT NULL UNIQUE
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
CREATE INDEX idx_posts_is_published ON posts(is_published) WHERE is_published = TRUE;

-- Optimize active user queries
CREATE INDEX idx_users_is_active ON users(is_active);

-- Optimize ORDER BY name
CREATE INDEX idx_users_name ON users(name);
