-- 001-initial-schema.sql
-- Initial database schema for Guile Deploy Ledger

-- Migration metadata table
CREATE TABLE IF NOT EXISTS schema_migrations (
    version INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    applied_at INTEGER NOT NULL DEFAULT (strftime('%s', 'now')),
    checksum TEXT
);

-- Insert migration record
INSERT OR IGNORE INTO schema_migrations (version, name) VALUES (1, 'initial-schema');

-- Main deployments table
CREATE TABLE IF NOT EXISTS deployments (
    id TEXT PRIMARY KEY,
    service_name TEXT NOT NULL,
    version TEXT NOT NULL,
    environment TEXT NOT NULL,
    deployment_type TEXT NOT NULL CHECK(deployment_type IN ('blue-green', 'canary', 'rolling', 'big-bang', 'feature-flag', 'a-b-test')),
    started INTEGER NOT NULL,
    completed INTEGER,
    status TEXT NOT NULL CHECK(status IN ('pending', 'in-progress', 'success', 'failure', 'rolled-back', 'partial', 'cancelled')),
    initiator TEXT NOT NULL,
    metadata TEXT,
    parent_id TEXT,
    created_at INTEGER DEFAULT (strftime('%s', 'now')),
    updated_at INTEGER DEFAULT (strftime('%s', 'now')),
    FOREIGN KEY (parent_id) REFERENCES deployments(id) ON DELETE SET NULL
);

-- Indexes for deployments
CREATE INDEX IF NOT EXISTS idx_deployments_service ON deployments(service_name);
CREATE INDEX IF NOT EXISTS idx_deployments_environment ON deployments(environment);
CREATE INDEX IF NOT EXISTS idx_deployments_started ON deployments(started);
CREATE INDEX IF NOT EXISTS idx_deployments_status ON deployments(status);
CREATE INDEX IF NOT EXISTS idx_deployments_type ON deployments(deployment_type);
CREATE INDEX IF NOT EXISTS idx_deployments_created ON deployments(created_at);

-- Rollbacks table
CREATE TABLE IF NOT EXISTS rollbacks (
    id TEXT PRIMARY KEY,
    service_name TEXT NOT NULL,
    from_version TEXT NOT NULL,
    to_version TEXT NOT NULL,
    reason TEXT NOT NULL,
    timestamp INTEGER NOT NULL,
    initiator TEXT NOT NULL,
    impact TEXT,
    deployment_id TEXT,
    created_at INTEGER DEFAULT (strftime('%s', 'now')),
    FOREIGN KEY (deployment_id) REFERENCES deployments(id) ON DELETE SET NULL
);

-- Indexes for rollbacks
CREATE INDEX IF NOT EXISTS idx_rollbacks_service ON rollbacks(service_name);
CREATE INDEX IF NOT EXISTS idx_rollbacks_timestamp ON rollbacks(timestamp);
CREATE INDEX IF NOT EXISTS idx_rollbacks_deployment ON rollbacks(deployment_id);

-- Service metadata table
CREATE TABLE IF NOT EXISTS service_metadata (
    name TEXT PRIMARY KEY,
    type TEXT NOT NULL CHECK(type IN ('monolith', 'microservice', 'serverless', 'batch-job', 'edge-function')),
    dependencies TEXT,
    owners TEXT,
    repository TEXT,
    health_check TEXT,
    created INTEGER NOT NULL,
    updated INTEGER NOT NULL,
    active BOOLEAN DEFAULT 1,
    tags TEXT
);

-- Deployment events table for audit trail
CREATE TABLE IF NOT EXISTS deployment_events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    deployment_id TEXT NOT NULL,
    event_type TEXT NOT NULL CHECK(event_type IN ('created', 'started', 'progress', 'completed', 'failed', 'rolled_back', 'cancelled')),
    event_data TEXT,
    timestamp INTEGER DEFAULT (strftime('%s', 'now')),
    user TEXT,
    FOREIGN KEY (deployment_id) REFERENCES deployments(id) ON DELETE CASCADE
);

-- Indexes for deployment events
CREATE INDEX IF NOT EXISTS idx_events_deployment ON deployment_events(deployment_id);
CREATE INDEX IF NOT EXISTS idx_events_timestamp ON deployment_events(timestamp);
CREATE INDEX IF NOT EXISTS idx_events_type ON deployment_events(event_type);

-- Metrics cache table
CREATE TABLE IF NOT EXISTS metrics_cache (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL,
    calculated_at INTEGER NOT NULL,
    expires_at INTEGER,
    service_name TEXT,
    period_start INTEGER,
    period_end INTEGER
);

CREATE INDEX IF NOT EXISTS idx_metrics_cache_expires ON metrics_cache(expires_at);
CREATE INDEX IF NOT EXISTS idx_metrics_cache_service ON metrics_cache(service_name);

-- Configuration table
CREATE TABLE IF NOT EXISTS configuration (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL,
    updated_at INTEGER DEFAULT (strftime('%s', 'now'))
);

-- Insert default configuration
INSERT OR IGNORE INTO configuration (key, value) VALUES
    ('retention_days', '365'),
    ('cache_ttl_seconds', '300'),
    ('max_deployment_duration_seconds', '3600'),
    ('alert_threshold_failure_rate', '0.1'),
    ('alert_threshold_mttr_seconds', '1800');

-- Views for common queries
CREATE VIEW IF NOT EXISTS v_recent_deployments AS
SELECT
    d.*,
    sm.type as service_type,
    sm.owners,
    sm.repository
FROM deployments d
LEFT JOIN service_metadata sm ON d.service_name = sm.name
WHERE d.created_at > strftime('%s', 'now', '-30 days')
ORDER BY d.started DESC;

CREATE VIEW IF NOT EXISTS v_deployment_success_rate AS
SELECT
    service_name,
    environment,
    COUNT(*) as total_deployments,
    SUM(CASE WHEN status = 'success' THEN 1 ELSE 0 END) as successful_deployments,
    CAST(SUM(CASE WHEN status = 'success' THEN 1 ELSE 0 END) AS REAL) / COUNT(*) * 100 as success_rate
FROM deployments
WHERE started > strftime('%s', 'now', '-30 days')
GROUP BY service_name, environment;

-- Triggers for update timestamps
CREATE TRIGGER IF NOT EXISTS update_deployment_timestamp
AFTER UPDATE ON deployments
BEGIN
    UPDATE deployments SET updated_at = strftime('%s', 'now') WHERE id = NEW.id;
END;

CREATE TRIGGER IF NOT EXISTS update_service_timestamp
AFTER UPDATE ON service_metadata
BEGIN
    UPDATE service_metadata SET updated = strftime('%s', 'now') WHERE name = NEW.name;
END;