-- Deploy enable_uuid
-- requires: appschema

BEGIN;

  CREATE EXTENSION "uuid-ossp";

COMMIT;
