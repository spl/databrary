ALTER TABLE "record_category" ADD "description" text;
ALTER TABLE "metric" ADD "description" text;

UPDATE record_category SET description = 'An individual subject depicted, represented, or otherwise contributing data' WHERE name = 'participant';
UPDATE record_category SET description = 'A grouping determined by an aspect of the data (participant ability, age, experience, longitudinal visit, measurements used/available)' WHERE name = 'group';
UPDATE record_category SET description = 'Indicates that the methods used were not finalized or were non-standard' WHERE name = 'pilot';
UPDATE record_category SET description = 'Indicates that data were not usable for a study' WHERE name = 'exclusion';
UPDATE record_category SET description = 'An experimenter-determined manipulation (within or between sessions)' WHERE name = 'condition';
UPDATE record_category SET description = 'A particular task, activity, or phase of the session or study' WHERE name = 'task';
UPDATE record_category SET description = 'A particular setting or other variable aspect of where/when/how data were collected' WHERE name = 'context';

UPDATE metric SET description = 'A primary, unique, anonymized identifier, label, or name' WHERE name = 'ID';
UPDATE metric SET description = 'A reason for a label (often for an exclusion)' WHERE name = 'reason';
DELETE FROM metric WHERE name = 'summary';
UPDATE metric SET description = 'A longer explanation or description of this label' WHERE name = 'description';
UPDATE metric SET description = 'The date of birth of an individual, or start/inception date for other labels (used with session date to calculate age)' WHERE name = 'birthdate';
UPDATE metric SET description = '"Male", "Female", or any other relevant gender label' WHERE name = 'gender';
UPDATE metric SET description = 'Usually as categorized by NIH' WHERE name = 'race';
UPDATE metric SET description = 'Usually as categorized by NIH (Hispanic/Non-Hispanic)' WHERE name = 'ethnicity';
UPDATE metric SET description = 'Any developmental, physical, or mental disability' WHERE name = 'disability';
UPDATE metric SET description = 'Primary language relevant to this label, spoken by this participant, or used in this context' WHERE name = 'language';
UPDATE metric SET description = 'The physical context of this label' WHERE name = 'setting';
UPDATE metric SET description = 'Relevant country of origin, setting, or otherwise' WHERE name = 'country';
UPDATE metric SET description = 'Relevant state/territory, usually within specified country' WHERE name = 'state';
UPDATE metric SET description = 'Other information or alternate identifier for this label' WHERE name = 'info';

UPDATE metric SET id = -530, description = 'Pregnancy age in weeks between last menstrual period and birth (or pre-natal observation)', name = 'gestational age' WHERE name = 'gestational age (weeks)';
INSERT INTO "metric" ("id", "name", "release", "type", "description") SELECT -530, 'gestational age', 'PUBLIC', 'numeric', 'Pregnancy age in weeks between last menstrual period and birth (or pre-natal observation)' EXCEPT SELECT id, name, release, type, description FROM metric WHERE name = 'gestational age';

-- leftover from 20150727-metric_release
CREATE OR REPLACE VIEW "volume_text" ("volume", "text") AS
	SELECT id, name FROM volume 
	UNION ALL SELECT id, body FROM volume WHERE body IS NOT NULL
	UNION ALL SELECT volume, COALESCE(prename || ' ', '') || name FROM volume_access JOIN party ON party.id = party WHERE individual >= 'ADMIN'
	UNION ALL SELECT volume, head FROM volume_citation
	UNION ALL SELECT volume, year::text FROM volume_citation WHERE year IS NOT NULL
	UNION ALL SELECT volume, name FROM volume_funding JOIN funder ON funder = fundref_id
	UNION ALL SELECT volume, name FROM container WHERE name IS NOT NULL
	UNION ALL SELECT volume, name FROM asset JOIN slot_asset ON asset.id = asset WHERE name IS NOT NULL
	UNION ALL SELECT volume, datum FROM record JOIN measure_text ON record.id = record JOIN metric ON metric = metric.id WHERE metric.release >= 'PUBLIC'
	UNION ALL SELECT volume, tag.name FROM tag JOIN tag_use ON tag.id = tag JOIN container ON container = container.id;
