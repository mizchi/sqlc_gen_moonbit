-- name: GetUser :one
SELECT * FROM users WHERE id = $1;

-- name: ListUsers :many
SELECT * FROM users ORDER BY id;

-- name: CreateUser :execlastid
INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id;

-- name: UpdateUser :execrows
UPDATE users SET name = $2, email = $3 WHERE id = $1;

-- name: DeleteUser :exec
DELETE FROM users WHERE id = $1;
