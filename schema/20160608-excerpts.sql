CREATE SCHEMA old_excerpts;
COMMENT ON SCHEMA old_excerpts IS 'Objects that had been set to the old EXCERPTS release level but downgrated to SHARED upon the language change of June 2016.';

CREATE TABLE old_excerpts."slot_release" (
	"container" integer NOT NULL,
	"segment" segment NOT NULL,
	Primary Key ("container", "segment"),
	Foreign Key ("container", "segment") References "slot_release" ON DELETE CASCADE ON UPDATE CASCADE
) INHERITS ("slot");
INSERT INTO old_excerpts.slot_release SELECT container, segment FROM slot_release WHERE release = 'EXCERPTS';
UPDATE slot_release SET release = 'SHARED' WHERE release = 'EXCERPTS';

CREATE TABLE old_excerpts."asset" (
	"id" integer NOT NULL Primary Key References "asset" ON DELETE CASCADE ON UPDATE CASCADE
);
INSERT INTO old_excerpts.asset SELECT id FROM asset WHERE release = 'EXCERPTS';
UPDATE asset SET release = 'SHARED' WHERE release = 'EXCERPTS';

CREATE TABLE old_excerpts."excerpt" (
	"asset" integer NOT NULL,
	"segment" segment NOT NULL,
	Primary Key ("asset", "segment"),
	Foreign Key ("asset", "segment") References "excerpt" ON DELETE CASCADE ON UPDATE CASCADE
);
INSERT INTO old_excerpts.excerpt SELECT asset, segment FROM excerpt WHERE release = 'EXCERPTS';
UPDATE excerpt SET release = 'SHARED' WHERE release = 'EXCERPTS';
ALTER TABLE old_excerpts."excerpt" ALTER "segment" SET STORAGE PLAIN;
