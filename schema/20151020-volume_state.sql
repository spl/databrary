CREATE TABLE "volume_state" (
	"volume" integer NOT NULL References "volume" ON DELETE CASCADE ON UPDATE CASCADE,
	"key" varchar(64) NOT NULL,
	"value" jsonb NOT NULL,
	"public" boolean NOT NULL,
	Primary Key ("volume", "key")
);
COMMENT ON TABLE "volume_state" IS 'Client-side per-volume state, primarily used for saved reports.';
