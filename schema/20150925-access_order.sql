ALTER TABLE "volume_access" ADD "sort" smallint;
ALTER TABLE audit."volume_access" ADD "sort" smallint;
CREATE OR REPLACE VIEW "volume_owners_view" ("volume", "owners") AS
	SELECT volume, array_agg(party || ':' || name || COALESCE(', ' || prename, '') ORDER BY sort) FROM volume_access JOIN party ON party = party.id WHERE individual = 'ADMIN' GROUP BY volume;
SELECT volume_owners_update(NULL);
