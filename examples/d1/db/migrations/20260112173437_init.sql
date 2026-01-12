-- Create "users" table
CREATE TABLE `users` (
  `id` integer NULL PRIMARY KEY AUTOINCREMENT,
  `username` text NOT NULL,
  `email` text NOT NULL,
  `display_name` text NOT NULL,
  `bio` text NULL,
  `avatar_url` text NULL,
  `created_at` text NOT NULL DEFAULT (datetime('now')),
  `updated_at` text NOT NULL DEFAULT (datetime('now'))
);
-- Create index "users_username" to table: "users"
CREATE UNIQUE INDEX `users_username` ON `users` (`username`);
-- Create index "users_email" to table: "users"
CREATE UNIQUE INDEX `users_email` ON `users` (`email`);
-- Create "categories" table
CREATE TABLE `categories` (
  `id` integer NULL PRIMARY KEY AUTOINCREMENT,
  `name` text NOT NULL,
  `slug` text NOT NULL,
  `description` text NULL
);
-- Create index "categories_name" to table: "categories"
CREATE UNIQUE INDEX `categories_name` ON `categories` (`name`);
-- Create index "categories_slug" to table: "categories"
CREATE UNIQUE INDEX `categories_slug` ON `categories` (`slug`);
-- Create "posts" table
CREATE TABLE `posts` (
  `id` integer NULL PRIMARY KEY AUTOINCREMENT,
  `author_id` integer NOT NULL,
  `category_id` integer NULL,
  `title` text NOT NULL,
  `slug` text NOT NULL,
  `content` text NOT NULL,
  `excerpt` text NULL,
  `status` text NOT NULL DEFAULT 'draft',
  `published_at` text NULL,
  `created_at` text NOT NULL DEFAULT (datetime('now')),
  `updated_at` text NOT NULL DEFAULT (datetime('now')),
  CONSTRAINT `0` FOREIGN KEY (`category_id`) REFERENCES `categories` (`id`) ON UPDATE NO ACTION ON DELETE NO ACTION,
  CONSTRAINT `1` FOREIGN KEY (`author_id`) REFERENCES `users` (`id`) ON UPDATE NO ACTION ON DELETE NO ACTION,
  CHECK (status IN ('draft', 'published', 'archived'))
);
-- Create index "posts_slug" to table: "posts"
CREATE UNIQUE INDEX `posts_slug` ON `posts` (`slug`);
-- Create index "idx_posts_author" to table: "posts"
CREATE INDEX `idx_posts_author` ON `posts` (`author_id`);
-- Create index "idx_posts_category" to table: "posts"
CREATE INDEX `idx_posts_category` ON `posts` (`category_id`);
-- Create index "idx_posts_status" to table: "posts"
CREATE INDEX `idx_posts_status` ON `posts` (`status`);
-- Create index "idx_posts_published_at" to table: "posts"
CREATE INDEX `idx_posts_published_at` ON `posts` (`published_at`);
-- Create "tags" table
CREATE TABLE `tags` (
  `id` integer NULL PRIMARY KEY AUTOINCREMENT,
  `name` text NOT NULL,
  `slug` text NOT NULL
);
-- Create index "tags_name" to table: "tags"
CREATE UNIQUE INDEX `tags_name` ON `tags` (`name`);
-- Create index "tags_slug" to table: "tags"
CREATE UNIQUE INDEX `tags_slug` ON `tags` (`slug`);
-- Create "post_tags" table
CREATE TABLE `post_tags` (
  `post_id` integer NOT NULL,
  `tag_id` integer NOT NULL,
  PRIMARY KEY (`post_id`, `tag_id`),
  CONSTRAINT `0` FOREIGN KEY (`tag_id`) REFERENCES `tags` (`id`) ON UPDATE NO ACTION ON DELETE CASCADE,
  CONSTRAINT `1` FOREIGN KEY (`post_id`) REFERENCES `posts` (`id`) ON UPDATE NO ACTION ON DELETE CASCADE
);
-- Create index "idx_post_tags_post" to table: "post_tags"
CREATE INDEX `idx_post_tags_post` ON `post_tags` (`post_id`);
-- Create index "idx_post_tags_tag" to table: "post_tags"
CREATE INDEX `idx_post_tags_tag` ON `post_tags` (`tag_id`);
-- Create "comments" table
CREATE TABLE `comments` (
  `id` integer NULL PRIMARY KEY AUTOINCREMENT,
  `post_id` integer NOT NULL,
  `author_name` text NOT NULL,
  `author_email` text NOT NULL,
  `content` text NOT NULL,
  `is_approved` integer NOT NULL DEFAULT 0,
  `created_at` text NOT NULL DEFAULT (datetime('now')),
  CONSTRAINT `0` FOREIGN KEY (`post_id`) REFERENCES `posts` (`id`) ON UPDATE NO ACTION ON DELETE CASCADE
);
-- Create index "idx_comments_post" to table: "comments"
CREATE INDEX `idx_comments_post` ON `comments` (`post_id`);
