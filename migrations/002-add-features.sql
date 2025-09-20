-- 002-add-features.sql
-- Add feature tracking and enhanced metrics

-- Check if migration already applied
INSERT OR IGNORE INTO schema_migrations (version, name) VALUES (2, 'add-features');

-- Feature flags table
CREATE TABLE IF NOT EXISTS feature_flags (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE,
    enabled BOOLEAN DEFAULT 0,
    description TEXT,
    created_at INTEGER DEFAULT (strftime('%s', 'now')),
    updated_at INTEGER DEFAULT (strftime('%s', 'now'))
);

-- Deployment feature associations
CREATE TABLE IF NOT EXISTS deployment_features (
    deployment_id TEXT NOT NULL,
    feature_id INTEGER NOT NULL,
    enabled BOOLEAN DEFAULT 1,
    PRIMARY KEY (deployment_id, feature_id),
    FOREIGN KEY (deployment_id) REFERENCES deployments(id) ON DELETE CASCADE,
    FOREIGN KEY (feature_id) REFERENCES feature_flags(id) ON DELETE CASCADE
);

-- Service dependencies tracking
CREATE TABLE IF NOT EXISTS service_dependencies (
    service_name TEXT NOT NULL,
    depends_on TEXT NOT NULL,
    dependency_type TEXT CHECK(dependency_type IN ('hard', 'soft', 'optional')),
    created_at INTEGER DEFAULT (strftime('%s', 'now')),
    PRIMARY KEY (service_name, depends_on),
    FOREIGN KEY (service_name) REFERENCES service_metadata(name) ON DELETE CASCADE
);

-- Deployment approvals
CREATE TABLE IF NOT EXISTS deployment_approvals (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    deployment_id TEXT NOT NULL,
    approver TEXT NOT NULL,
    approved_at INTEGER DEFAULT (strftime('%s', 'now')),
    comment TEXT,
    FOREIGN KEY (deployment_id) REFERENCES deployments(id) ON DELETE CASCADE
);

-- Add new indexes
CREATE INDEX IF NOT EXISTS idx_deployment_features_deployment ON deployment_features(deployment_id);
CREATE INDEX IF NOT EXISTS idx_deployment_features_feature ON deployment_features(feature_id);
CREATE INDEX IF NOT EXISTS idx_service_dependencies_service ON service_dependencies(service_name);
CREATE INDEX IF NOT EXISTS idx_deployment_approvals_deployment ON deployment_approvals(deployment_id);

-- Enhanced metrics view
CREATE VIEW IF NOT EXISTS v_deployment_metrics AS
SELECT
    d.service_name,
    d.environment,
    d.deployment_type,
    COUNT(*) as total_deployments,
    AVG(CASE WHEN d.completed IS NOT NULL
        THEN (d.completed - d.started)
        ELSE NULL END) as avg_duration_seconds,
    SUM(CASE WHEN d.status = 'success' THEN 1 ELSE 0 END) as successful,
    SUM(CASE WHEN d.status = 'failure' THEN 1 ELSE 0 END) as failed,
    SUM(CASE WHEN r.id IS NOT NULL THEN 1 ELSE 0 END) as rollbacks,
    MAX(d.started) as last_deployment
FROM deployments d
LEFT JOIN rollbacks r ON d.id = r.deployment_id
WHERE d.started > strftime('%s', 'now', '-30 days')
GROUP BY d.service_name, d.environment, d.deployment_type;