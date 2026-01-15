-- name: GetUser :one
SELECT * FROM users WHERE id = ?;

-- name: ListUsers :many
SELECT * FROM users ORDER BY name;

-- name: CreateUser :execlastid
INSERT INTO users (name, email) VALUES (?, ?);

-- name: UpdateUser :execrows
UPDATE users SET name = ?, email = ? WHERE id = ?;

-- name: DeleteUser :execrows
DELETE FROM users WHERE id = ?;
