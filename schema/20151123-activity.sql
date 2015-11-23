DROP VIEW "asset_revisions";

CREATE INDEX "party_activity_idx" ON audit."party" ("id") WHERE "audit_action" >= 'add';
CREATE INDEX "account_id_idx" ON audit."account" ("id");
CREATE INDEX "authorize_parent_activity_idx" ON audit."authorize" ("parent") WHERE "audit_action" >= 'add';
CREATE INDEX "volume_activity_idx" ON audit."volume" ("id") WHERE "audit_action" >= 'add';
CREATE INDEX "volume_access_activity_idx" ON audit."volume_access" ("volume") WHERE "audit_action" >= 'add';
CREATE INDEX "container_activity_idx" ON audit."container" ("id") WHERE "audit_action" >= 'add';
CREATE INDEX "slot_release_activity_idx" ON audit."slot_release" ("container") WHERE "audit_action" >= 'add';
CREATE INDEX "asset_activity_idx" ON audit."asset" ("id") WHERE "audit_action" >= 'add';
CREATE INDEX "slot_asset_activity_idx" ON audit."slot_asset" ("container") WHERE "audit_action" >= 'add';
CREATE INDEX "excerpt_activity_idx" ON audit."excerpt" ("asset") WHERE "audit_action" >= 'add';
