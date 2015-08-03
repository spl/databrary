ALTER TABLE "record_category" ADD "description" text;
ALTER TABLE "metric" ADD "description" text;

UPDATE record_category SET description = 'An individual subject depicted, represented, or otherwise contributing data' WHERE name = 'participant';
UPDATE record_category SET description = 'A grouping determined by an aspect of the data (participant ability, age, experience, measurements used/available)' WHERE name = 'group';
UPDATE record_category SET description = 'Indicates that the methods used were not finalized or were non-standard' WHERE name = 'pilot';
UPDATE record_category SET description = 'Indicates that data were not usable for a study' WHERE name = 'exclusion';
UPDATE record_category SET description = 'An experimenter-determined manipulation (within or between sessions)' WHERE name = 'condition';
UPDATE record_category SET description = 'A particular task, activity, or phase of the session or study' WHERE name = 'task';
UPDATE record_category SET description = 'A particular setting or other variable aspect of where/when/how data were collected' WHERE name = 'context';

UPDATE metric SET description = 'A primary, unique, anonymized identifier, label, or name' WHERE name = 'ID';
UPDATE metric SET description = 'A reason for a label (often for an exclusion)' WHERE name = 'reason';
UPDATE metric SET description = 'A short, one-line summary of this label' WHERE name = 'summary';
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
