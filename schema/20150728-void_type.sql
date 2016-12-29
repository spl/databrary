-- Add data_type value: void

-- Create a new enum, extended with 'void'
CREATE TYPE data_type_new AS ENUM ('text', 'numeric', 'date', 'void');
COMMENT ON TYPE data_type_new
  IS 'Types of measurement data corresponding to measure_* tables.';
-- Update all relevant tables to change column type from old enum to new enum,
-- casting the values
ALTER TABLE "metric"
  ALTER COLUMN "type" TYPE data_type_new
  USING "type"::text::data_type_new;    -- Cast
-- Drop old enum
DROP TYPE data_type;
-- Rename new enum
ALTER TYPE data_type_new RENAME TO data_type;
