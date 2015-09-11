INSERT INTO "format" ("id", "mimetype", "extension", "name") VALUES (-600, 'audio/mpeg',							ARRAY['mp3'], 'MPEG-1 or MPEG-2 audio layer III');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('audio/aac',								        ARRAY['aac'], 'Advanced Audio Coding');
INSERT INTO "format" ("mimetype", "extension", "name") VALUES ('audio/x-ms-wma',								ARRAY['wma'], 'Windows Media audio');
UPDATE "format" SET "name" = 'Windows Media video' WHERE "name" = 'Windows Media Video';
