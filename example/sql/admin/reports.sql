-- name: CountUsersByStatus :many
-- In a subdirectory, to prove discovery recurses.
SELECT status, count(*) AS "n!" FROM users GROUP BY status;
