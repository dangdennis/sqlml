CREATE TYPE user_status AS ENUM ('active', 'banned');

CREATE TABLE users (
  id              uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id uuid NOT NULL,
  email           text NOT NULL UNIQUE,
  display_name    text,
  status          user_status NOT NULL DEFAULT 'active',
  balance         numeric(12,2) NOT NULL DEFAULT 0,
  created_at      timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE posts (
  id        bigserial PRIMARY KEY,
  author_id uuid NOT NULL REFERENCES users(id),
  title     text NOT NULL,
  body      text
);
