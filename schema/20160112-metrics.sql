ALTER TABLE "record_category" RENAME TO "category";
ALTER INDEX "record_category_pkey" RENAME TO "category_pkey";
ALTER INDEX "record_category_name_key" RENAME TO "category_name_key";
ALTER SEQUENCE "record_category_id_seq" RENAME TO "category_id_seq";
ALTER SEQUENCE "category_id_seq" RESTART 1;

ALTER TABLE "record"
	DROP CONSTRAINT "record_category_fkey",
	ADD Foreign Key ("category") References "category" ON UPDATE CASCADE;

DROP TABLE "record_template";
ALTER TABLE "volume_metric" DROP CONSTRAINT "volume_metric_metric_fkey";
ALTER TABLE "measure_abstract" DROP CONSTRAINT "measure_abstract_metric_fkey";
ALTER TABLE "measure_text" DROP CONSTRAINT "measure_text_metric_fkey";
ALTER TABLE "measure_numeric" DROP CONSTRAINT "measure_numeric_metric_fkey";
ALTER TABLE "measure_date" DROP CONSTRAINT "measure_date_metric_fkey";

DROP VIEW "volume_text";
DROP TABLE "metric";

CREATE TABLE "metric" (
	"id" serial NOT NULL Primary Key,
	"category" smallint NOT NULL References "category" ON UPDATE CASCADE,
	"name" varchar(64) NOT NULL,
	"release" release,
	"type" data_type NOT NULL,
	"options" text[],
	"assumed" text,
	"description" text,
	"required" boolean,
	Unique ("category", "name")
);
ALTER TABLE "metric"
	ALTER "name" SET STORAGE EXTERNAL;
COMMENT ON TABLE "metric" IS 'Types of measurements for data stored in measure_$type tables.';
COMMENT ON COLUMN "metric"."options" IS '(Suggested) options for text enumerations, not enforced.';
COMMENT ON COLUMN "metric"."required" IS 'Indicates the default status of this field in volume designs: on by default (not null), or required on (true).';

CREATE FUNCTION update_metric("old" integer) RETURNS void LANGUAGE plpgsql AS $$
BEGIN
	UPDATE volume_metric SET metric = currval('metric_id_seq') WHERE category = currval('category_id_seq') AND metric = old;
	UPDATE measure_abstract SET metric = currval('metric_id_seq') FROM record WHERE record = id AND category = currval('category_id_seq') AND metric = old;
END; $$;

UPDATE category SET id = nextval('category_id_seq'), description = 'An individual human subject whose data are used or represented' WHERE name = 'participant';
INSERT INTO "metric" ("category", "name", "release", "type", "description", "required")			VALUES (currval('category_id_seq'), 'ID', 'PUBLIC', 'text', 'A unique, anonymized, primary identifier, such as participant ID', true);
SELECT update_metric(-900);
INSERT INTO "metric" ("category", "name", "release", "type", "description")				VALUES (currval('category_id_seq'), 'info', 'PUBLIC', 'text', 'Other information or alternate identifier');
SELECT update_metric(-90);
INSERT INTO "metric" ("category", "name", "release", "type", "description")				VALUES (currval('category_id_seq'), 'description', 'PUBLIC', 'text', 'A longer explanation or description');
SELECT update_metric(-600);
INSERT INTO "metric" ("category", "name", "type", "description", "required")				VALUES (currval('category_id_seq'), 'birthdate', 'date', 'Date of birth (used with session date to calculate age; you can also use the group category to designate age groups)', false);
SELECT update_metric(-590);
INSERT INTO "metric" ("category", "name", "release", "type", "options", "description", "required")	VALUES (currval('category_id_seq'), 'gender', 'PUBLIC', 'text', ARRAY['Female','Male'], '"Male", "Female", or any other relevant gender', false);
SELECT update_metric(-580);
INSERT INTO "metric" ("category", "name", "release", "type", "options", "description", "required")	VALUES (currval('category_id_seq'), 'race', 'PUBLIC', 'text', ARRAY['American Indian or Alaska Native','Asian','Native Hawaiian or Other Pacific Islander','Black or African American','White','Multiple'], 'As classified by NIH, or user-defined classification', false);
SELECT update_metric(-550);
INSERT INTO "metric" ("category", "name", "release", "type", "options", "description", "required")	VALUES (currval('category_id_seq'), 'ethnicity', 'PUBLIC', 'text', ARRAY['Not Hispanic or Latino','Hispanic or Latino'], 'As classified by NIH (Hispanic/Non-Hispanic), or user-defined classification', false);
SELECT update_metric(-540);
INSERT INTO "metric" ("category", "name", "release", "type", "description")				VALUES (currval('category_id_seq'), 'gestational age', 'PUBLIC', 'numeric', 'Pregnancy age in weeks between last menstrual period and birth (or pre-natal observation)');
SELECT update_metric(-530);
INSERT INTO "metric" ("category", "name", "release", "type", "options", "description")			VALUES (currval('category_id_seq'), 'pregnancy term', 'PUBLIC', 'text', Array['Full term', 'Preterm'], '"Full term", "Preterm", or other gestational term');
INSERT INTO "metric" ("category", "name", "release", "type", "description")				VALUES (currval('category_id_seq'), 'birth weight', 'PUBLIC', 'numeric', 'Weight at birth (in grams, e.g., 3250)');
INSERT INTO "metric" ("category", "name", "type", "assumed", "description", "required")			VALUES (currval('category_id_seq'), 'disability', 'text', 'typical', 'Any developmental, physical, or mental disability or disabilities', false);
SELECT update_metric(-520);
INSERT INTO "metric" ("category", "name", "release", "type", "assumed", "description", "required")	VALUES (currval('category_id_seq'), 'language', 'PUBLIC', 'text', 'English', 'Primary language(s) spoken by and to participant', false);
SELECT update_metric(-510);
INSERT INTO "metric" ("category", "name", "release", "type", "assumed", "description")			VALUES (currval('category_id_seq'), 'country', 'PUBLIC', 'text', 'US', 'Country where participant was born');
SELECT update_metric(-150);
INSERT INTO "metric" ("category", "name", "release", "type", "options", "description")			VALUES (currval('category_id_seq'), 'state', 'PUBLIC', 'text', ARRAY['AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','MD','MA','MI','MN','MS','MO','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY'], 'State/territory where participant was born');
SELECT update_metric(-140);
INSERT INTO "metric" ("category", "name", "release", "type", "options", "description")	                VALUES (currval('category_id_seq'), 'setting', 'PUBLIC', 'text', ARRAY['Lab','Home','Classroom','Outdoor','Clinic'], 'The physical context of the participant (please do not use for new data: see the context category instead)');
SELECT update_metric(-180);

UPDATE category SET id = nextval('category_id_seq'), description = 'Indicates that the methods used were not finalized or were non-standard' WHERE name = 'pilot';
INSERT INTO "metric" ("category", "name", "release", "type", "required")				VALUES (currval('category_id_seq'), 'pilot', 'PUBLIC', 'void', false);
SELECT update_metric(-1000);
INSERT INTO "metric" ("category", "name", "release", "type", "description")				VALUES (currval('category_id_seq'), 'name', 'PUBLIC', 'text', 'A label or identifier referring to the pilot method');
SELECT update_metric(-900);
INSERT INTO "metric" ("category", "name", "release", "type", "description")				VALUES (currval('category_id_seq'), 'description', 'PUBLIC', 'text', 'A longer explanation or description of the pilot method');
SELECT update_metric(-600);

UPDATE category SET id = nextval('category_id_seq'), description = 'Indicates that data were not usable' WHERE name = 'exclusion';
INSERT INTO "metric" ("category", "name", "release", "type")						VALUES (currval('category_id_seq'), 'excluded', 'PUBLIC', 'void');
SELECT update_metric(-1000);
INSERT INTO "metric" ("category", "name", "release", "type", "description")				VALUES (currval('category_id_seq'), 'name', 'PUBLIC', 'text', 'A label or identifier referring to the exclusion criterion');
SELECT update_metric(-900);
INSERT INTO "metric" ("category", "name", "release", "type", "options", "description", "required")	VALUES (currval('category_id_seq'), 'reason', 'PUBLIC', 'text', ARRAY['Did not meet inclusion criteria','Procedural/experimenter error','Withdrew/fussy/tired','Outlier'], 'The reason for excluding these data', false);
SELECT update_metric(-700);
INSERT INTO "metric" ("category", "name", "release", "type", "description")				VALUES (currval('category_id_seq'), 'description', 'PUBLIC', 'text', 'A longer explanation or description of the reason for excluding data');
SELECT update_metric(-600);

UPDATE category SET id = nextval('category_id_seq'), description = 'An experimenter-determined manipulation (within or between sessions)' WHERE name = 'condition';
INSERT INTO "metric" ("category", "name", "release", "type", "description", "required")			VALUES (currval('category_id_seq'), 'name', 'PUBLIC', 'text', 'A label or identifier for the condition', true);
SELECT update_metric(-900);
INSERT INTO "metric" ("category", "name", "release", "type", "description")				VALUES (currval('category_id_seq'), 'description', 'PUBLIC', 'text', 'A longer explanation or description of the condition');
SELECT update_metric(-600);
INSERT INTO "metric" ("category", "name", "release", "type", "description")				VALUES (currval('category_id_seq'), 'info', 'PUBLIC', 'text', 'Other information or alternate identifier');
SELECT update_metric(-90);

UPDATE category SET id = nextval('category_id_seq'), description = 'A grouping determined by an aspect of the data (participant ability, age, grade level, experience, longitudinal visit, measurements used/available)' WHERE name = 'group';
INSERT INTO "metric" ("category", "name", "release", "type", "description", "required")			VALUES (currval('category_id_seq'), 'name', 'PUBLIC', 'text', 'A label or identifier for the grouping', true);
SELECT update_metric(-900);
INSERT INTO "metric" ("category", "name", "release", "type", "description")				VALUES (currval('category_id_seq'), 'description', 'PUBLIC', 'text', 'A longer explanation or description of the grouping');
SELECT update_metric(-600);
INSERT INTO "metric" ("category", "name", "release", "type", "description")				VALUES (currval('category_id_seq'), 'info', 'PUBLIC', 'text', 'Other information or alternate identifier');
SELECT update_metric(-90);

UPDATE category SET id = nextval('category_id_seq'), description = 'A particular task, activity, or phase of the session or study' WHERE name = 'task';
INSERT INTO "metric" ("category", "name", "release", "type", "description", "required")			VALUES (currval('category_id_seq'), 'name', 'PUBLIC', 'text', 'A label or identifier for the task', true);
SELECT update_metric(-900);
INSERT INTO "metric" ("category", "name", "release", "type", "description", "required")			VALUES (currval('category_id_seq'), 'description', 'PUBLIC', 'text', 'A longer explanation or description of the task', false);
SELECT update_metric(-600);
INSERT INTO "metric" ("category", "name", "release", "type", "description")				VALUES (currval('category_id_seq'), 'info', 'PUBLIC', 'text', 'Other information or alternate identifier');
SELECT update_metric(-90);

UPDATE category SET id = nextval('category_id_seq'), description = 'A particular setting or other aspect of where/when/how data were collected' WHERE name = 'context';
INSERT INTO "metric" ("category", "name", "release", "type", "description")				VALUES (currval('category_id_seq'), 'name', 'PUBLIC', 'text', 'A label or identifier for the context');
SELECT update_metric(-900);
INSERT INTO "metric" ("category", "name", "release", "type", "options", "description", "required")	VALUES (currval('category_id_seq'), 'setting', 'PUBLIC', 'text', ARRAY['Lab','Home','Classroom','Outdoor','Clinic'], 'The physical context', true);
SELECT update_metric(-180);
INSERT INTO "metric" ("category", "name", "release", "type", "assumed", "description")			VALUES (currval('category_id_seq'), 'language', 'PUBLIC', 'text', 'English', 'Language used in this context');
SELECT update_metric(-510);
INSERT INTO "metric" ("category", "name", "release", "type", "assumed", "description", "required")	VALUES (currval('category_id_seq'), 'country', 'PUBLIC', 'text', 'US', 'Country of data collection', false);
SELECT update_metric(-150);
INSERT INTO "metric" ("category", "name", "release", "type", "options", "description", "required")	VALUES (currval('category_id_seq'), 'state', 'PUBLIC', 'text', ARRAY['AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','MD','MA','MI','MN','MS','MO','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY'], 'State/territory of data collection', false);
SELECT update_metric(-140);

DROP FUNCTION update_metric(integer);

ALTER TABLE "volume_metric" ADD Foreign Key ("metric") References "metric" ON UPDATE CASCADE ON DELETE CASCADE,
	DROP "category",
	ADD Primary Key ("volume", "metric");
ALTER TABLE "measure_abstract" ADD Foreign Key ("metric") References "metric" ON UPDATE CASCADE;
ALTER TABLE "measure_text" ADD Foreign Key ("metric") References "metric" ON UPDATE CASCADE;
ALTER TABLE "measure_numeric" ADD Foreign Key ("metric") References "metric" ON UPDATE CASCADE;
ALTER TABLE "measure_date" ADD Foreign Key ("metric") References "metric" ON UPDATE CASCADE;

CREATE VIEW "volume_text" ("volume", "text") AS
	SELECT id, name FROM volume 
	UNION ALL SELECT id, body FROM volume WHERE body IS NOT NULL
	UNION ALL SELECT volume, COALESCE(prename || ' ', '') || name FROM volume_access JOIN party ON party.id = party WHERE individual >= 'ADMIN'
	UNION ALL SELECT volume, head FROM volume_citation
	UNION ALL SELECT volume, year::text FROM volume_citation WHERE year IS NOT NULL
	UNION ALL SELECT volume, name FROM volume_funding JOIN funder ON funder = fundref_id
	UNION ALL SELECT volume, name FROM container WHERE name IS NOT NULL
	UNION ALL SELECT volume, name FROM asset JOIN slot_asset ON asset.id = asset WHERE name IS NOT NULL
	UNION ALL SELECT volume, datum FROM record JOIN measure_text ON record.id = record JOIN metric ON metric = metric.id WHERE metric.release >= 'PUBLIC'
	UNION ALL SELECT volume, tag.name FROM tag JOIN tag_use ON tag.id = tag JOIN container ON container = container.id; -- might want DISTINCT here
COMMENT ON VIEW "volume_text" IS 'All (searchable) text data associated with a volume.';
