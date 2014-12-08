-- Verify enable_uuid

BEGIN;

  SELECT 1/count(*) FROM pg_extension WHERE extname = 'uuid-ossp';

ROLLBACK;
