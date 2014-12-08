-- Revert enable_uuid

BEGIN;

  DROP EXTENSION "uuid-ossp";

COMMIT;
