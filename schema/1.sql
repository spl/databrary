ALTER TYPE audit_action SET SCHEMA audit;
ALTER TYPE audit.audit_action RENAME TO action;
COMMENT ON TYPE audit.action IS 'The various activities for which we keep audit records (in audit or a derived table).';

ALTER TABLE "measure_number" RENAME TO "measure_numeric";
ALTER INDEX "measure_number_pkey" RENAME TO "measure_numeric_pkey";
ALTER TABLE "measure_numeric" RENAME CONSTRAINT "measure_number_metric_fkey" TO "measure_numeric_metric_fkey";
ALTER TABLE "measure_numeric" RENAME CONSTRAINT "measure_number_record_fkey" TO "measure_numeric_record_fkey";

DROP VIEW audit."measure_text";
DROP VIEW audit."measure_number";
DROP VIEW audit."measure_date";
ALTER TABLE audit."audit" ALTER "audit_time" TYPE "timestamptz";
CREATE VIEW audit."measure_text" AS
	SELECT * FROM audit.measure;
CREATE TRIGGER "measure_i" INSTEAD OF INSERT ON audit."measure_text" FOR EACH ROW EXECUTE PROCEDURE audit."measure_i" ();
CREATE VIEW audit."measure_numeric" AS
	SELECT * FROM audit.measure;
CREATE TRIGGER "measure_i" INSTEAD OF INSERT ON audit."measure_numeric" FOR EACH ROW EXECUTE PROCEDURE audit."measure_i" ();
CREATE VIEW audit."measure_date" AS
	SELECT * FROM audit.measure;
CREATE TRIGGER "measure_i" INSTEAD OF INSERT ON audit."measure_date" FOR EACH ROW EXECUTE PROCEDURE audit."measure_i" ();


ALTER TABLE "party" RENAME "sortname" TO "name";
ALTER TABLE audit."party" RENAME "sortname" TO "name";

ALTER TABLE "account" ADD Unique ("email");
ALTER TABLE "account" DROP "openid";
ALTER TABLE audit."account" DROP "openid";

DROP INDEX audit."audit_login_idx";
CREATE INDEX "audit_login_idx" ON audit."account" ("id", "audit_time") WHERE "audit_action" IN ('attempt', 'open');
COMMENT ON INDEX audit."audit_login_idx" IS 'Allow efficient determination of recent login attempts for security.';

DROP VIEW "volume_access_view";
DROP VIEW "authorize_view";
DROP VIEW "authorize_valid";
DROP MATERIALIZED VIEW "authorize_inherit";
ALTER TABLE "authorize" ALTER "expires" TYPE "timestamptz";
ALTER TABLE audit."authorize" ALTER "expires" TYPE "timestamptz";
CREATE MATERIALIZED VIEW "authorize_inherit" AS
	WITH RECURSIVE aa AS (
		SELECT * FROM authorize
		UNION
		SELECT a.child, aa.parent, CASE
		         WHEN aa.site = 'ADMIN' THEN LEAST(a.site, 'EDIT')
			 WHEN aa.site = 'EDIT' THEN LEAST(a.site, 'READ')
			 ELSE LEAST(aa.site, a.site, 'PUBLIC')
		       END, 'NONE', LEAST(a.expires, aa.expires)
	          FROM aa JOIN authorize a ON aa.child = a.parent
	) SELECT * FROM aa
	UNION ALL SELECT id, id, 'ADMIN', 'ADMIN', NULL FROM party WHERE id >= 0
	UNION ALL SELECT id, -1, 'ADMIN', 'NONE', NULL FROM party WHERE id >= 0;
COMMENT ON MATERIALIZED VIEW "authorize_inherit" IS 'Transitive inheritance closure of authorize.';
CREATE INDEX ON "authorize_inherit" ("parent", "child");
CREATE VIEW "authorize_valid" AS
	SELECT * FROM authorize WHERE expires IS NULL OR expires > CURRENT_TIMESTAMP;
COMMENT ON VIEW "authorize_valid" IS 'Active records from "authorize"';
CREATE VIEW "authorize_view" ("child", "parent", "site", "member") AS
	SELECT child, parent, MAX(site), MAX(member)
	  FROM authorize_inherit
	 WHERE expires IS NULL OR expires > CURRENT_TIMESTAMP
	 GROUP BY parent, child;
COMMENT ON VIEW "authorize_view" IS 'Expanded list of effective, active authorizations.';
CREATE VIEW "volume_access_view" ("volume", "party", "access") AS
	SELECT volume, party, individual FROM volume_access
	UNION ALL
	SELECT volume, child, MAX(LEAST(children, CASE WHEN children <= 'SHARED' THEN site ELSE member END))
	  FROM volume_access JOIN authorize_view ON party = parent GROUP BY volume, child;
COMMENT ON VIEW "volume_access_view" IS 'Expanded list of effective volume access.';

ALTER TABLE "volume" ADD "doi" varchar(20) Unique;
COMMENT ON COLUMN "volume"."doi" IS 'DOIs issued for volumes (currently via EZID).';
ALTER TABLE audit."volume" ADD "doi" varchar(20);
UPDATE volume SET doi = volume_doi.doi FROM volume_doi WHERE volume.id = volume_doi.volume;
DROP TABLE "volume_doi";

DROP FUNCTION "volume_creation" (integer);
CREATE FUNCTION "volume_creation" ("volume" integer) RETURNS timestamptz LANGUAGE sql STABLE STRICT AS
	$$ SELECT max("audit_time") FROM audit."volume" WHERE "id" = $1 AND "audit_action" = 'add' $$;

ALTER TABLE "funder" DROP "party";

ALTER TABLE "format" ALTER "extension" TYPE varchar(8)[] USING CASE WHEN extension IS NULL THEN ARRAY[]::varchar(8)[] ELSE ARRAY[extension] END,
	ALTER "extension" SET NOT NULL;
UPDATE format SET extension = ARRAY['mpg','mpeg'] WHERE extension = '{mpg}';
UPDATE format SET extension = ARRAY['cha','chat'] WHERE extension = '{cha}';
UPDATE format SET extension = ARRAY['jpg','jpeg'] WHERE extension = '{jpg}';
UPDATE format SET extension = ARRAY['mts','m2ts'] WHERE extension = '{mts}';
DELETE FROM format WHERE mimetype = 'text/html';

ALTER TABLE "transcode" ALTER "start" TYPE "timestamptz",
	DROP CONSTRAINT "transcode_owner_fkey",
	ADD Foreign Key ("owner") References "account";

DROP MATERIALIZED VIEW "comment_thread";
ALTER TABLE "comment" ALTER "time" TYPE "timestamptz";
CREATE MATERIALIZED VIEW "comment_thread" AS
	WITH RECURSIVE t AS (
		SELECT *, ARRAY[id]::integer[] AS thread
		  FROM comment WHERE parent IS NULL
		UNION ALL
		SELECT c.*, t.thread || c.id
		  FROM comment c JOIN t ON c.parent = t.id
	) SELECT * FROM t;
CREATE INDEX "comment_thread_slot_idx" ON "comment_thread" ("container", "segment");
CREATE INDEX ON "comment_thread" ("who");
COMMENT ON MATERIALIZED VIEW "comment_thread" IS 'Comments along with their parent-defined path (top-down).  Parents must never form a cycle or this will not terminate.';

ALTER TABLE "measure" RENAME TO "measure_abstract";
ALTER INDEX "measure_pkey" RENAME TO "measure_abstract_pkey";
ALTER TABLE "measure_abstract" RENAME CONSTRAINT "measure_check" TO "measure_abstract_check";
ALTER TABLE "measure_abstract" RENAME CONSTRAINT "measure_metric_fkey" TO "measure_abstract_metric_fkey";
ALTER TABLE "measure_abstract" RENAME CONSTRAINT "measure_record_fkey" TO "measure_abstract_record_fkey";

DROP VIEW "measures";
DROP VIEW "measure_view";
DROP VIEW "measure_all";
CREATE VIEW "measure" AS
	SELECT record, metric, datum FROM measure_text UNION ALL
	SELECT record, metric, text(datum) FROM measure_numeric UNION ALL
	SELECT record, metric, text(datum) FROM measure_date;
COMMENT ON VIEW "measure" IS 'Data from all measure tables, coerced to text.';

CREATE FUNCTION "measure_insert" () RETURNS trigger LANGUAGE plpgsql AS $insert$
DECLARE
	type_name name;
BEGIN
	SELECT type INTO type_name FROM metric WHERE id = NEW.metric;
	IF type_name IS NULL THEN
		RETURN NULL;
	END IF;
	EXECUTE $$INSERT INTO measure_$$ || type_name || $$ VALUES ($1, $2, $3::$$ || type_name || $$) RETURNING text(datum)$$ INTO NEW.datum USING NEW.record, NEW.metric, NEW.datum;
	RETURN NEW;
END; $insert$;
CREATE TRIGGER "measure_insert" INSTEAD OF INSERT ON "measure" FOR EACH ROW EXECUTE PROCEDURE "measure_insert" ();

CREATE FUNCTION "measure_update" () RETURNS trigger LANGUAGE plpgsql AS $update$
DECLARE
	type_name name;
BEGIN
	SELECT type INTO type_name FROM metric WHERE id = NEW.metric AND id = OLD.metric;
	IF type_name IS NULL THEN
		RETURN NULL;
	END IF;
	EXECUTE $$UPDATE measure_$$ || type_name || $$ SET record = $1, datum = $2::$$ || type_name || $$ WHERE record = $3 AND metric = $4 RETURNING text(datum)$$ INTO NEW.datum USING NEW.record, NEW.datum, OLD.record, OLD.metric;
	RETURN NEW;
END; $update$;
CREATE TRIGGER "measure_update" INSTEAD OF UPDATE ON "measure" FOR EACH ROW EXECUTE PROCEDURE "measure_update" ();

CREATE FUNCTION "measure_delete" () RETURNS trigger LANGUAGE plpgsql AS $delete$
DECLARE
	type_name name;
BEGIN
	SELECT type INTO type_name FROM metric WHERE id = OLD.metric;
	IF type_name IS NULL THEN
		RETURN NULL;
	END IF;
	EXECUTE $$DELETE FROM measure_$$ || type_name || $$ WHERE record = $1 AND metric = $2 RETURNING text(datum)$$ INTO OLD.datum USING OLD.record, OLD.metric;
	RETURN OLD;
END; $delete$;
CREATE TRIGGER "measure_delete" INSTEAD OF DELETE ON "measure" FOR EACH ROW EXECUTE PROCEDURE "measure_delete" ();

CREATE VIEW "measures" ("record", "measures") AS
	SELECT record, array_agg(metric || ':' || datum ORDER BY metric) FROM measure GROUP BY record;
COMMENT ON VIEW "measures" IS 'All measures for each record aggregated into a single array.';

ALTER TABLE "token" ALTER "expires" TYPE timestamptz;

TRUNCATE "session";
ALTER TABLE "session"
	ADD "verf" char(16) NOT NULL,
	ADD "superuser" boolean NOT NULL DEFAULT false;

-- don't care about data here:
DROP TABLE "upload";
CREATE TABLE "upload" (
	"token" char(32) NOT NULL Primary Key,
	"expires" timestamptz NOT NULL,
	"account" integer NOT NULL References "account" ON DELETE CASCADE,
	"volume" integer NOT NULL References "volume" ON DELETE CASCADE,
	"filename" text NOT NULL,
	"size" bigint NOT NULL Check ("size" >= 0)
) INHERITS ("account_token");
COMMENT ON TABLE "upload" IS 'Tokens issued to track active uploads.';

ALTER TABLE audit."analytic" ALTER "data" DROP NOT NULL;
UPDATE audit.analytic SET data = data->'data';
