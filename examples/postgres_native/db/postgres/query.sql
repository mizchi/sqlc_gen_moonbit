-- name: get_user :one
SELECT id, name, uuid_var, date_var, time_var, timestamp_var, timestampz_var, bool_var, bigserial_var, int64_var, pg_int8_var, int_var, interval_var, timetz_var, smallint_var, int2_var, json_var, jsonb_var, bytes_var, float_var, double_var, numeric_var FROM test_users WHERE id = $1;

-- name: GetByDate :many
SELECT id, name, uuid_var, date_var, time_var, timestamp_var, timestampz_var, bool_var, bigserial_var, int64_var, pg_int8_var, int_var, interval_var, timetz_var, smallint_var, int2_var, json_var, jsonb_var, bytes_var, float_var, double_var, numeric_var FROM test_users WHERE date_var = $1;

-- name: GetByUuid :many
SELECT id, name, uuid_var, date_var, time_var, timestamp_var, timestampz_var, bool_var, bigserial_var, int64_var, pg_int8_var, int_var, interval_var, timetz_var, smallint_var, int2_var, json_var, jsonb_var, bytes_var, float_var, double_var, numeric_var  FROM test_users WHERE uuid_var = $1;

-- name: ListUsers :many
SELECT id, name, uuid_var, date_var, time_var, timestamp_var, timestampz_var, bool_var, bigserial_var, int64_var, pg_int8_var, int_var, interval_var, timetz_var, smallint_var, int2_var, json_var, jsonb_var, bytes_var, float_var, double_var, numeric_var  FROM test_users ORDER BY id;

-- name: CreateUser :execlastid
INSERT INTO test_users (name, bigserial_var, int64_var, pg_int8_var, int_var, bool_var, uuid_var, date_var, timestampz_var, timestamp_var, time_var, interval_var, timetz_var, smallint_var, int2_var, json_var, jsonb_var, bytes_var, float_var, double_var, numeric_var) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21) RETURNING id;

-- name: UpdateUser :execrows
UPDATE test_users SET name = $2, uuid_var = $3, date_var = $4 WHERE id = $1;

-- name: DeleteUser :exec
DELETE FROM test_users WHERE id = $1;

-- name: GetUserNN :one
SELECT id, name, uuid_var, date_var, time_var, timestamp_var, timestampz_var, bool_var, bigserial_var, int64_var, pg_int8_var, int_var, interval_var, timetz_var, smallint_var, int2_var, json_var, jsonb_var, bytes_var, float_var, double_var, numeric_var FROM test_users_not_null WHERE id = $1;

-- name: GetByDateNN :many
SELECT id, name, uuid_var, date_var, time_var, timestamp_var, timestampz_var, bool_var, bigserial_var, int64_var, pg_int8_var, int_var, interval_var, timetz_var, smallint_var, int2_var, json_var, jsonb_var, bytes_var, float_var, double_var, numeric_var  FROM test_users_not_null WHERE date_var = $1;

-- name: GetByUuidNN :many
SELECT id, name, uuid_var, date_var, time_var, timestamp_var, timestampz_var, bool_var, bigserial_var, int64_var, pg_int8_var, int_var, interval_var, timetz_var, smallint_var, int2_var, json_var, jsonb_var, bytes_var, float_var, double_var, numeric_var  FROM test_users_not_null WHERE uuid_var = $1;

-- name: ListUsersNN :many
SELECT id, name, uuid_var, date_var, time_var, timestamp_var, timestampz_var, bool_var, bigserial_var, int64_var, pg_int8_var, int_var, interval_var, timetz_var, smallint_var, int2_var, json_var, jsonb_var, bytes_var, float_var, double_var, numeric_var  FROM test_users_not_null ORDER BY id;

-- name: CreateUserNN :execlastid
INSERT INTO test_users_not_null (name, bigserial_var, int64_var, pg_int8_var, int_var, bool_var, uuid_var, date_var, timestampz_var, timestamp_var, time_var, interval_var, timetz_var, smallint_var, int2_var, json_var, jsonb_var, bytes_var, float_var, double_var, numeric_var) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21) RETURNING id;

-- name: UpdateUserNN :execrows
UPDATE test_users_not_null SET name = $2, uuid_var = $3, date_var = $4 WHERE id = $1;

-- name: DeleteUserNN :exec
DELETE FROM test_users_not_null WHERE id = $1;
