-- Revert users

BEGIN;

  DROP TABLE diagrammer.users;

COMMIT;
