ALTER TABLE "record" ALTER "category" SET NOT NULL;
ALTER TABLE "record_measures" ALTER "category" SET NOT NULL;

INSERT INTO "metric" ("id", "name", "release", "type") VALUES (-1000, 'indicator', 'PUBLIC', 'void');
INSERT INTO "record_template" VALUES (-800, -1000);

CREATE TABLE "volume_metric" (
	"volume" integer NOT NULL References "volume",
	"category" smallint NOT NULL References "record_category" ON UPDATE CASCADE ON DELETE CASCADE,
	"metric" integer NOT NULL References "metric" ON UPDATE CASCADE ON DELETE CASCADE,
	Primary Key ("volume", "category", "metric")
);

INSERT INTO volume_metric SELECT DISTINCT volume, category, COALESCE(metric, -1000) FROM record LEFT JOIN measure_abstract ON record.id = record;
