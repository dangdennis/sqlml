-- name: GetUser :one
-- Fetch a single user by id.
SELECT id, email, display_name, status, balance, created_at
FROM users
WHERE id = :id;

-- name: SearchUsers :many
-- Users in an organization whose email matches a pattern, newest first.
SELECT id, email, created_at
FROM users
WHERE organization_id = :organization_id
  AND email ILIKE :email_pattern
ORDER BY created_at DESC
LIMIT :limit;

-- name: CountPostsByUser :many
-- Shows both nullability edge cases at once. posts.title is NOT NULL in the
-- schema but nullable here because of the LEFT JOIN, so attnotnull alone would
-- get it wrong. count(...) is a computed column with no origin, so Postgres
-- reports nothing and the "!" alias pins it to non-null.
SELECT
  u.email,
  p.title AS "title?",
  coalesce(count(p.id), 0) AS "post_count!"
FROM users u
LEFT JOIN posts p ON p.author_id = u.id
GROUP BY u.email, p.title;

-- name: DeleteUser :exec
DELETE FROM users WHERE id = :id;

-- name: GetUserFull :one
-- Selects every column of users, so it should share the model type.
SELECT id, organization_id, email, display_name, status, balance, created_at
FROM users
WHERE id = :id;

-- name: SetDisplayName :exec
-- display_name is nullable, so it becomes an optional argument and the
-- generated function takes a trailing ().
UPDATE users SET display_name = :display_name? WHERE id = :id;

-- name: CreateUser :exec
-- display_name is nullable, so it becomes an optional argument.
INSERT INTO users (id, organization_id, email, display_name, status, balance)
VALUES (:id, :organization_id, :email, :display_name?, :status, :balance);

-- name: GetUsersByIds :many
-- An array parameter: = ANY(...) is how you write a dynamic IN list.
SELECT id, email FROM users WHERE id = ANY(:ids);

-- name: PutTagSet :exec
INSERT INTO tag_sets (id, owner, tags, scores, states, meta)
VALUES (:id, :owner, :tags, :scores, :states, :meta)
ON CONFLICT (id) DO UPDATE
  SET tags = excluded.tags, scores = excluded.scores,
      states = excluded.states, meta = excluded.meta;

-- name: GetTagSet :one
SELECT tags, scores, states, meta FROM tag_sets WHERE id = :id;

-- name: GetUserStrict :one!
-- The row must exist; absence is an error, so the row comes back unwrapped.
SELECT id, email, display_name, status, balance, created_at
FROM users
WHERE id = :id;
