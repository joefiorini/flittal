-- Verify users

BEGIN;

  SELECT user_id, email, password, created_at FROM users WHERE false;

ROLLBACK;
