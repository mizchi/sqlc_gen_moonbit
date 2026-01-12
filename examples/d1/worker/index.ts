// Cloudflare Worker entry point for D1 Blog API

import * as queries from "./queries";

export interface Env {
  DB: D1Database;
}

export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    const url = new URL(request.url);
    const path = url.pathname;

    try {
      // GET /users
      if (path === "/users" && request.method === "GET") {
        const users = await queries.listUsers(env.DB);
        return Response.json(users);
      }

      // POST /users
      if (path === "/users" && request.method === "POST") {
        const body = await request.json<{
          username: string;
          email: string;
          display_name: string;
          bio?: string;
          avatar_url?: string;
        }>();
        await queries.createUser(
          env.DB,
          body.username,
          body.email,
          body.display_name,
          body.bio ?? null,
          body.avatar_url ?? null
        );
        return new Response("Created", { status: 201 });
      }

      // GET /users/:id
      const userMatch = path.match(/^\/users\/(\d+)$/);
      if (userMatch && request.method === "GET") {
        const user = await queries.getUserById(env.DB, parseInt(userMatch[1]));
        if (!user) {
          return new Response("Not found", { status: 404 });
        }
        return Response.json(user);
      }

      // GET /categories
      if (path === "/categories" && request.method === "GET") {
        const categories = await queries.listCategories(env.DB);
        return Response.json(categories);
      }

      // POST /categories
      if (path === "/categories" && request.method === "POST") {
        const body = await request.json<{
          name: string;
          slug: string;
          description?: string;
        }>();
        await queries.createCategory(env.DB, body.name, body.slug, body.description ?? null);
        return new Response("Created", { status: 201 });
      }

      // GET /posts
      if (path === "/posts" && request.method === "GET") {
        const limit = parseInt(url.searchParams.get("limit") ?? "10");
        const posts = await queries.listPublishedPosts(env.DB, limit);
        return Response.json(posts);
      }

      // POST /posts
      if (path === "/posts" && request.method === "POST") {
        const body = await request.json<{
          author_id: number;
          category_id?: number;
          title: string;
          slug: string;
          content: string;
          excerpt?: string;
          status: string;
        }>();
        await queries.createPost(
          env.DB,
          body.author_id,
          body.category_id ?? null,
          body.title,
          body.slug,
          body.content,
          body.excerpt ?? null,
          body.status
        );
        return new Response("Created", { status: 201 });
      }

      // GET /posts/:slug
      const postMatch = path.match(/^\/posts\/([^/]+)$/);
      if (postMatch && request.method === "GET") {
        const post = await queries.getPostBySlug(env.DB, postMatch[1]);
        if (!post) {
          return new Response("Not found", { status: 404 });
        }
        return Response.json(post);
      }

      // POST /posts/:id/publish
      const publishMatch = path.match(/^\/posts\/(\d+)\/publish$/);
      if (publishMatch && request.method === "POST") {
        await queries.publishPost(env.DB, parseInt(publishMatch[1]));
        return new Response("Published", { status: 200 });
      }

      // GET /tags
      if (path === "/tags" && request.method === "GET") {
        const tags = await queries.listTags(env.DB);
        return Response.json(tags);
      }

      // POST /tags
      if (path === "/tags" && request.method === "POST") {
        const body = await request.json<{ name: string; slug: string }>();
        await queries.createTag(env.DB, body.name, body.slug);
        return new Response("Created", { status: 201 });
      }

      // GET /posts/:id/comments
      const commentsMatch = path.match(/^\/posts\/(\d+)\/comments$/);
      if (commentsMatch && request.method === "GET") {
        const comments = await queries.listCommentsForPost(env.DB, parseInt(commentsMatch[1]));
        return Response.json(comments);
      }

      // POST /posts/:id/comments
      if (commentsMatch && request.method === "POST") {
        const body = await request.json<{
          author_name: string;
          author_email: string;
          content: string;
        }>();
        await queries.createComment(
          env.DB,
          parseInt(commentsMatch[1]),
          body.author_name,
          body.author_email,
          body.content
        );
        return new Response("Created", { status: 201 });
      }

      return new Response("Not found", { status: 404 });
    } catch (error) {
      console.error(error);
      return new Response(`Error: ${error}`, { status: 500 });
    }
  },
};
