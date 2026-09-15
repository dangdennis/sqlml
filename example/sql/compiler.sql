-- name: PutCompilerValue :exec
INSERT INTO compiler_a.values (id, payload, other, payloads, matrix, positives, spans, words)
VALUES (:id, :payload?, :other?, :payloads, :matrix, :positives, :spans, :words)
ON CONFLICT (id) DO UPDATE SET payload = excluded.payload, other = excluded.other,
  payloads = excluded.payloads, matrix = excluded.matrix, positives = excluded.positives,
  spans = excluded.spans, words = excluded.words;

-- name: GetCompilerValue :one!
SELECT payload, other, payloads, matrix, positives, spans, words
FROM compiler_a.values WHERE id = :id;

-- name: OuterJoinCompiler :one!
SELECT rhs.id, rhs.payload
FROM (VALUES (1)) AS lhs(id)
LEFT JOIN compiler_a.values AS rhs ON rhs.id = -999;

-- name: CopyCompilerValue :copy
INSERT INTO compiler_a.values (id, payload, other, payloads, matrix, positives, spans, words)
VALUES (:id, :payload?, :other?, :payloads, :matrix, :positives, :spans, :words);

-- name: BinaryContainers :one!
SELECT :bytes::bytea AS bytes, :items::bytea[] AS items;
