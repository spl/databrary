CREATE OR REPLACE FUNCTION "volume_owners_update" ("vol" integer) RETURNS void LANGUAGE plpgsql AS $$
DECLARE
	own text[];
BEGIN
	IF vol IS NULL THEN
		TRUNCATE volume_owners;
		INSERT INTO volume_owners SELECT * FROM volume_owners_view;
	ELSE
		SELECT owners INTO own FROM volume_owners_view WHERE volume = vol;
		UPDATE volume_owners SET owners = COALESCE(own, '{}') WHERE volume = vol;
		IF NOT FOUND THEN
			INSERT INTO volume_owners VALUES (vol, COALESCE(own, '{}'));
		END IF;
	END IF;
END; $$;
