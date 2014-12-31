-- Deploy add_unique_constraint_to_users_email
-- requires: users

BEGIN;

  ALTER TABLE
    users
  ADD CONSTRAINT
    users_email_unique
  UNIQUE
    (email);

COMMIT;
