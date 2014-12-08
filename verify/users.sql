-- Verify users

BEGIN;

  SELECT id, email, password, created_at FROM diagrammer.users WHERE false;

ROLLBACK;
