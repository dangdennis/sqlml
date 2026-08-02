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

-- name: FindUsers :many
-- Optional blocks: each /*? ... */ clause is included only when its parameter
-- is supplied. Every combination is verified against the database at codegen.
SELECT id, email, status, balance
FROM users
WHERE organization_id = :org
  /*? AND email ILIKE :email */
  /*? AND status = :status */
  /*? AND balance >= :min_balance */
ORDER BY email
LIMIT :limit;

-- name: PutBooking :exec
INSERT INTO bookings (id, on_date, at_time, duration)
VALUES (:id, :on_date, :at_time, :duration)
ON CONFLICT (id) DO UPDATE
  SET on_date = excluded.on_date, at_time = excluded.at_time,
      duration = excluded.duration;

-- name: GetBooking :one!
SELECT on_date, at_time, duration FROM bookings WHERE id = :id;

-- name: HasMetaKey :many
-- The jsonb ? operator: a regression sentinel for drivers that must not
-- re-parse SQL through their own placeholder grammar.
SELECT id FROM tag_sets WHERE meta ? :key;

-- name: BulkAddUsers :copy
-- Bulk-load users in one COPY round-trip. All-or-nothing: any bad row
-- aborts the whole load server-side.
INSERT INTO users (id, organization_id, email, display_name, status, balance)
VALUES (:id, :organization_id, :email, :display_name?, :status, :balance);

-- name: CountUsersByOrg :one!
SELECT count(*) AS "n!" FROM users WHERE organization_id = :org;

-- name: DeleteUsersByOrg :exec
DELETE FROM users WHERE organization_id = :org;
