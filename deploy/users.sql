-- Deploy users
-- requires: enable_uuid

BEGIN;

  SET client_min_messages = 'warning';

  CREATE TABLE users (
      user_id        uuid          PRIMARY KEY DEFAULT uuid_generate_v4()
    , email     TEXT          NOT NULL
    , password  TEXT          NOT NULL
    , created_at TIMESTAMPTZ  NOT NULL DEFAULT NOW()
  );


COMMIT;
