CREATE OR REPLACE FUNCTION "excerpt_shift" () RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
	-- if we ever support "stretching" timeseries this will be wrong
	shift interval := COALESCE(lower(NEW.segment), '0') - COALESCE(lower(OLD.segment), '0');
BEGIN
	IF NEW.segment <> OLD.segment THEN
		UPDATE excerpt SET segment = segment_shift(segment, shift) WHERE asset = NEW.asset;
	END IF;
	RETURN null;
END; $$;
