ALTER TABLE "record" ALTER "category" SET NOT NULL;
ALTER TABLE "record_measures" ALTER "category" SET NOT NULL;

CREATE TABLE "volume_metric" (
	"volume" integer NOT NULL References "volume",
	"category" smallint NOT NULL References "record_category" ON UPDATE CASCADE ON DELETE CASCADE,
	"metric" integer NOT NULL References "metric" ON UPDATE CASCADE ON DELETE CASCADE,
	Primary Key ("volume", "category", "metric")
);

INSERT INTO volume_metric SELECT DISTINCT volume, category, metric FROM record JOIN measure_abstract ON record.id = record;
