ALTER TABLE "slot_release" DROP Constraint "slot_release_singleton_segment_excl",
	ADD Constraint "slot_release_overlap_excl" Exclude USING gist (singleton("container") WITH =, "segment" WITH &&) DEFERRABLE;
ALTER TABLE "excerpt" DROP Constraint "excerpt_singleton_segment_excl",
	ADD Constraint "excerpt_overlap_excl" Exclude USING gist (singleton("asset") WITH =, "segment" WITH &&) DEFERRABLE;
ALTER TABLE "tag_use" DROP Constraint "tag_use_singleton_singleton1_singleton2_segment_excl",
	ADD Constraint "tag_use_overlap_excl" Exclude USING gist (singleton("tag") WITH =, singleton("who") WITH =, singleton("container") WITH =, "segment" WITH &&) DEFERRABLE;
ALTER TABLE "keyword_use" DROP Constraint "keyword_use_singleton_singleton1_segment_excl",
	ADD Constraint "keyword_use_overlap_excl" Exclude USING gist (singleton("tag") WITH =, singleton("container") WITH =, "segment" WITH &&) DEFERRABLE;
ALTER TABLE "slot_record" DROP Constraint "slot_record_singleton_singleton1_segment_excl",
	ADD Constraint "slot_record_overlap_excl" Exclude USING gist (singleton("record") WITH =, singleton("container") WITH =, "segment" WITH &&) DEFERRABLE;

CREATE OR REPLACE FUNCTION "excerpt_shift" () RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
	-- if we ever support "stretching" timeseries this will be wrong
	shift interval := COALESCE(lower(NEW.segment), '0') - COALESCE(lower(OLD.segment), '0');
BEGIN
	IF NEW.segment <> OLD.segment THEN
		SET CONSTRAINTS excerpt_overlap_excl DEFERRED;
		UPDATE excerpt SET segment = segment_shift(segment, shift) WHERE asset = NEW.asset;
	END IF;
	RETURN null;
END; $$;
