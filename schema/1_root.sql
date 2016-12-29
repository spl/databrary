-- Rename data_type value: number -> numeric

-- Create a new enum, replacing 'number' with 'numeric'
CREATE TYPE data_type_new AS ENUM ('text', 'numeric', 'date');
COMMENT ON TYPE data_type_new
  IS 'Types of measurement data corresponding to measure_* tables.';
-- Update all relevant tables to change column type from old enum to new enum,
-- replacing 'number' with 'numeric' and casting the other values
ALTER TABLE "metric"
  ALTER COLUMN "type" TYPE data_type_new
  USING CASE
    WHEN "type" = 'number' THEN 'numeric'  -- Replace
    ELSE "type"::text::data_type_new       -- Cast
  END;
-- Drop old enum
DROP TYPE data_type;
-- Rename new enum
ALTER TYPE data_type_new RENAME TO data_type;
