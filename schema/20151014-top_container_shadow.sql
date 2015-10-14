CREATE TEMP TABLE top_container (
	real integer NOT NULL, -- References "container"
	volume integer NOT NULL, -- References "volume"
	shadow integer NOT NULL Default nextval('container_id_seq') -- References "container" 
);

WITH top AS (INSERT INTO top_container (real, volume) SELECT DISTINCT top.* FROM (SELECT min(id) AS id, volume FROM container WHERE top GROUP BY volume) AS top JOIN slot ON id = container WHERE segment <> '(,)' OR slot.tableoid IN ('slot_asset'::regclass, 'slot_release'::regclass) RETURNING *)
	INSERT INTO container (id, volume, top) SELECT shadow, volume, true FROM top;

UPDATE container SET name = COALESCE(top.name, 'Top-level materials'), date = top.date FROM top_container JOIN container top ON real = top.id WHERE shadow = container.id;
UPDATE container SET name = NULL, date = NULL FROM top_container WHERE real = container.id;
UPDATE slot SET container = shadow FROM top_container WHERE container = real AND (segment <> '(,)' OR slot.tableoid IN ('slot_asset'::regclass, 'slot_release'::regclass));
