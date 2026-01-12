-- @query GetUser :one
-- @param id INTEGER
-- @returns id INTEGER, name TEXT, email TEXT
SELECT id, name, email FROM users WHERE id = ?;

-- @query ListUsers :many
-- @returns id INTEGER, name TEXT, email TEXT
SELECT id, name, email FROM users ORDER BY name;

-- @query CreateUser :exec
-- @param name TEXT
-- @param email TEXT
INSERT INTO users (name, email) VALUES (?, ?);

-- @query UpdateUserEmail :exec
-- @param id INTEGER
-- @param email TEXT
UPDATE users SET email = ? WHERE id = ?;

-- @query DeleteUser :exec
-- @param id INTEGER
DELETE FROM users WHERE id = ?;
