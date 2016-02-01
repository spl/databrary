UPDATE metric SET options = ARRAY['American Indian or Alaska Native','Asian','Native Hawaiian or Other Pacific Islander','Black or African American','White','More than one','Unknown or not reported'] WHERE name = 'race' AND options IS NOT NULL;
UPDATE metric SET options = ARRAY['Not Hispanic or Latino','Hispanic or Latino','Unknown or not reported'] WHERE name = 'ethnicity' AND options IS NOT NULL;

UPDATE measure_text SET datum = 'More than one' FROM metric WHERE metric = id AND name = 'race' AND options IS NOT NULL AND datum IN ('Multiple');
UPDATE measure_text SET datum = 'Unknown or not reported' FROM metric WHERE metric = id AND name IN ('race', 'ethnicity') AND options IS NOT NULL AND datum IN ('Did not report', 'Unknown');
