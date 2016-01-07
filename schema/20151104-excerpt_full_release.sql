UPDATE asset SET release = GREATEST(asset.release, excerpt.release) FROM slot_asset JOIN excerpt USING (asset) WHERE id = asset AND excerpt.segment @> slot_asset.segment;
UPDATE excerpt SET release = NULL FROM asset JOIN slot_asset ON asset.id = slot_asset.asset WHERE excerpt.asset = asset.id AND excerpt.release <= asset.release;
