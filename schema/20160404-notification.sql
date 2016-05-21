CREATE TYPE notice_delivery AS ENUM ('none', 'site', 'weekly', 'daily', 'async');
COMMENT ON TYPE notice_delivery IS 'The different ways in which notifications can be delivered, as chosen by user.  While ''site'' means only online, ''weekly'', ''daily'', and ''async'' also generate email.';

CREATE TABLE "notice" (
	"id" smallserial NOT NULL Primary Key,
	"name" varchar(64) NOT NULL Unique,
	"delivery" notice_delivery NOT NULL
);
COMMENT ON TABLE "notice" IS 'The different classes of notifications about which users can set delivery preferences.';

INSERT INTO "notice" ("name", "delivery") VALUES ('AccountChange',		'async');
INSERT INTO "notice" ("name", "delivery") VALUES ('AuthorizeRequest',		'site');
INSERT INTO "notice" ("name", "delivery") VALUES ('AuthorizeGranted',		'async');
INSERT INTO "notice" ("name", "delivery") VALUES ('AuthorizeExpiring',		'daily');
INSERT INTO "notice" ("name", "delivery") VALUES ('AuthorizeExpired',		'daily');
INSERT INTO "notice" ("name", "delivery") VALUES ('AuthorizeChildRequest',	'async');
INSERT INTO "notice" ("name", "delivery") VALUES ('AuthorizeChildGranted',	'none');
INSERT INTO "notice" ("name", "delivery") VALUES ('AuthorizeChildExpiring',	'daily');
INSERT INTO "notice" ("name", "delivery") VALUES ('AuthorizeChildExpired',	'daily');
INSERT INTO "notice" ("name", "delivery") VALUES ('VolumeAssist',		'none');
INSERT INTO "notice" ("name", "delivery") VALUES ('VolumeCreated',		'daily');
INSERT INTO "notice" ("name", "delivery") VALUES ('VolumeSharing',		'daily');
INSERT INTO "notice" ("name", "delivery") VALUES ('VolumeAccessOther',		'daily');
INSERT INTO "notice" ("name", "delivery") VALUES ('VolumeAccess',		'daily');
INSERT INTO "notice" ("name", "delivery") VALUES ('ReleaseSlot',		'daily');
INSERT INTO "notice" ("name", "delivery") VALUES ('ReleaseAsset',		'daily');
INSERT INTO "notice" ("name", "delivery") VALUES ('ReleaseExcerpt',		'daily');
INSERT INTO "notice" ("name", "delivery") VALUES ('ExcerptVolume',		'none');
INSERT INTO "notice" ("name", "delivery") VALUES ('CommentVolume',		'site');
INSERT INTO "notice" ("name", "delivery") VALUES ('CommentReply',		'site');
INSERT INTO "notice" ("name", "delivery") VALUES ('TagVolume',			'none');
INSERT INTO "notice" ("name", "delivery") VALUES ('SharedVolume',		'none');
INSERT INTO "notice" ("name", "delivery") VALUES ('Newsletter',			'async');

CREATE TABLE "notify" (
	"target" integer NOT NULL References "account" ON DELETE CASCADE,
	"notice" smallint NOT NULL References "notice" ON DELETE CASCADE ON UPDATE CASCADE,
	"delivery" notice_delivery NOT NULL,
	Primary Key ("target", "notice")
);
COMMENT ON TABLE "notify" IS 'Delivery preferences about notice types, overriding the default.';

CREATE VIEW "notify_view" ("target", "notice", "delivery") AS
	SELECT account.id, notice.id, COALESCE(notify.delivery, notice.delivery)
	FROM account CROSS JOIN notice
	LEFT JOIN notify ON account.id = target AND notice.id = notice;
COMMENT ON VIEW "notify_view" IS 'Full set of delivery preferences for all users and notices.';

CREATE TABLE "notification" (
	"id" serial NOT NULL Primary Key,
	"target" integer NOT NULL References "account" ON DELETE CASCADE,
	"notice" smallint NOT NULL References "notice" ON DELETE CASCADE ON UPDATE CASCADE,
	"time" timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"delivered" notice_delivery NOT NULL DEFAULT 'none',
	"agent" integer NOT NULL References "party" ON DELETE CASCADE,
	"party" integer References "party" ON DELETE CASCADE,
	"volume" integer References "volume" ON DELETE CASCADE,
	"permission" permission,
	"container" integer References "container" ON DELETE CASCADE,
	"segment" segment,
	"asset" integer References "asset" ON DELETE CASCADE,
	"release" release,
	"comment" integer References "comment" ON DELETE CASCADE,
	"tag" integer References "tag" ON DELETE CASCADE
);
CREATE INDEX "notification_party_idx" ON "notification" ("target");
COMMENT ON TABLE "notification" IS 'List of active notification messages.';

