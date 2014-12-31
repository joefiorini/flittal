-- Revert add_unique_constraint_to_users_email

BEGIN;

  ALTER TABLE
    users
  DROP CONSTRAINT
    users_email_unique;

COMMIT;
