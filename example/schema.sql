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

-- Arrays: text[], int[], and an array of an enum.
CREATE TABLE tag_sets (
  id     uuid PRIMARY KEY,
  owner  uuid NOT NULL,
  tags   text[] NOT NULL DEFAULT '{}',
  scores int[] NOT NULL DEFAULT '{}',
  states user_status[] NOT NULL DEFAULT '{}',
  meta   jsonb NOT NULL DEFAULT '{}'
);

-- Temporal types beyond timestamptz.
CREATE TABLE bookings (
  id       uuid PRIMARY KEY,
  on_date  date NOT NULL,
  at_time  time NOT NULL,
  duration interval NOT NULL
);

-- Compiler/type-system fixtures: nested containers and schema-scoped identities.
CREATE SCHEMA IF NOT EXISTS compiler_a;
CREATE SCHEMA IF NOT EXISTS compiler_b;
CREATE TYPE compiler_a.status AS ENUM ('open', 'closed');
CREATE TYPE compiler_b.status AS ENUM ('open', 'archived');
CREATE DOMAIN compiler_a.positive AS bigint CHECK (VALUE > 0);
CREATE DOMAIN compiler_a.positive_list AS compiler_a.positive[];
CREATE TYPE compiler_a.payload AS (
  label text,
  states compiler_a.status[],
  amount compiler_a.positive,
  span int8range
);
CREATE TYPE compiler_b.payload AS (label text, state compiler_b.status);
CREATE TYPE compiler_a.text_range AS RANGE (subtype = text, collation = "C");
CREATE TABLE compiler_a.values (
  id integer PRIMARY KEY,
  payload compiler_a.payload,
  other compiler_b.payload,
  payloads compiler_a.payload[],
  matrix bigint[],
  positives compiler_a.positive_list,
  spans int8multirange,
  words compiler_a.text_range
);
