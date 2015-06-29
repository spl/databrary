CREATE VIEW "volume_owners_view" ("volume", "owners") AS
	SELECT volume, array_agg(party || ':' || name || COALESCE(', ' || prename, '')) FROM volume_access JOIN party ON party = party.id WHERE individual = 'ADMIN' GROUP BY volume;

CREATE TABLE "volume_owners" (
	"volume" integer NOT NULL Primary Key References "volume" ON UPDATE CASCADE ON DELETE CASCADE,
	"owners" text[] NOT NULL Default '{}'
);
COMMENT ON TABLE "volume_owners" IS 'Materialized version of volume_owners_view.';

CREATE FUNCTION "volume_owners_update" ("vol" integer) RETURNS void LANGUAGE plpgsql AS $$
DECLARE
	own text[];
BEGIN
	IF vol IS NULL THEN
		TRUNCATE volume_owners;
		INSERT INTO volume_owners SELECT * FROM volume_owners_view;
	ELSE
		SELECT owners INTO own FROM volume_owners_view WHERE volume = vol;
		UPDATE volume_owners SET owners = own WHERE volume = vol;
		IF NOT FOUND THEN
			INSERT INTO volume_owners VALUES (vol, own);
		END IF;
	END IF;
END; $$;

CREATE FUNCTION "volume_owners_update_old" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	PERFORM volume_owners_update(OLD.volume);
	RETURN null;
END; $$;
CREATE TRIGGER "volume_owners_update_old" AFTER DELETE OR UPDATE ON "volume_access" FOR EACH ROW WHEN (OLD.individual = 'ADMIN') EXECUTE PROCEDURE "volume_owners_update_old" ();
CREATE FUNCTION "volume_owners_update_new" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	PERFORM volume_owners_update(NEW.volume);
	RETURN null;
END; $$;
CREATE TRIGGER "volume_owners_update_new" AFTER INSERT OR UPDATE ON "volume_access" FOR EACH ROW WHEN (NEW.individual = 'ADMIN') EXECUTE PROCEDURE "volume_owners_update_new" ();
CREATE FUNCTION "volume_owners_refresh" () RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
	PERFORM volume_owners_update(NULL);
	RETURN null;
END; $$;
CREATE TRIGGER "volume_owners_truncate" AFTER TRUNCATE ON "volume_access" EXECUTE PROCEDURE "volume_owners_refresh" ();

INSERT INTO volume_owners SELECT * FROM volume_owners_view;
