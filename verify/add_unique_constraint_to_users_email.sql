-- Verify add_unique_constraint_to_users_email

BEGIN;

  SELECT
    1/COUNT(*)
  FROM
    pg_catalog.pg_constraint
  WHERE
    contype = 'u'
  AND
    conname = 'users_email_unique';

ROLLBACK;
