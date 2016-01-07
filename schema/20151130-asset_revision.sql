ALTER TABLE "asset_revision" ADD Check ("orig" <> "asset");

CREATE TABLE "asset_replace" (
) INHERITS ("asset_revision");
COMMENT ON TABLE "asset_replace" IS 'Replacement assets provided by the user.';

INSERT INTO asset_replace (orig, asset) SELECT orig, asset FROM ONLY asset_revision;
TRUNCATE ONLY asset_revision;
ALTER TABLE "asset_revision" ADD Check (false) NO INHERIT;

CREATE FUNCTION "asset_replace" ("asset_old" integer, "asset_new" integer) RETURNS void STRICT LANGUAGE plpgsql AS $$
BEGIN
	PERFORM asset FROM asset_replace WHERE orig = asset_new;
	IF FOUND THEN -- avoid cycles
		RAISE 'Asset % already replaced', asset_new;
	END IF;
	INSERT INTO asset_replace (orig, asset) VALUES (asset_old, asset_new);
	UPDATE slot_asset SET asset = asset_new WHERE asset = asset_old;
END; $$;
DROP FUNCTION "asset_supersede" (integer, integer);

COMMENT ON TABLE "transcode" IS 'Format conversions that are being or have been applied to transform orig asset.';
