-- name: GetUser :one
-- Fetch a single user by id.
SELECT id, email, display_name, status, created_at
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
-- Shows two things at once: a LEFT JOIN (posts.title is NOT NULL in the
-- schema but nullable in this result), and a computed column where Postgres
-- reports no origin column, so the "!" alias pins it to non-null.
SELECT
  u.email,
  p.title,
  coalesce(count(p.id), 0) AS "post_count!"
FROM users u
LEFT JOIN posts p ON p.author_id = u.id
GROUP BY u.email, p.title;

-- name: DeleteUser :exec
DELETE FROM users WHERE id = :id;
