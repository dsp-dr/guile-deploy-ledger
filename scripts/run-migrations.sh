#!/usr/bin/env bash
# run-migrations.sh - Run database migrations

# Don't exit on command failures - we handle them
set +e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m'

DB_FILE="${1:-$HOME/.deploy-ledger/deployments.db}"
MIGRATION_DIR="migrations"

if [ ! -f "$DB_FILE" ]; then
    echo -e "${RED}Database file not found: $DB_FILE${NC}"
    exit 1
fi

if [ ! -d "$MIGRATION_DIR" ]; then
    echo -e "${RED}Migration directory not found: $MIGRATION_DIR${NC}"
    exit 1
fi

echo -e "${CYAN}Running database migrations...${NC}"
echo "Database: $DB_FILE"
echo ""

# Get current schema version
CURRENT_VERSION=$(sqlite3 "$DB_FILE" "SELECT COALESCE(MAX(version), 0) FROM schema_migrations;" 2>/dev/null || echo "0")
echo "Current schema version: $CURRENT_VERSION"

# Find migration files
MIGRATIONS=$(find "$MIGRATION_DIR" -name "*.sql" -type f | sort)
APPLIED=0
SKIPPED=0
FAILED=0

for migration in $MIGRATIONS; do
    # Extract version number from filename (e.g., 001-initial-schema.sql -> 1)
    FILENAME=$(basename "$migration")
    VERSION=$(echo "$FILENAME" | sed 's/^\([0-9]*\).*/\1/' | sed 's/^0*//')

    if [ -z "$VERSION" ] || ! [[ "$VERSION" =~ ^[0-9]+$ ]]; then
        echo -e "${YELLOW}⚠ Skipping invalid migration file: $FILENAME${NC}"
        ((SKIPPED++))
        continue
    fi

    if [ "$VERSION" -le "$CURRENT_VERSION" ]; then
        echo -e "${CYAN}○${NC} Migration $VERSION already applied: $FILENAME"
        ((SKIPPED++))
        continue
    fi

    echo -e "${YELLOW}→${NC} Applying migration $VERSION: $FILENAME"

    # Calculate checksum
    CHECKSUM=$(sha256sum "$migration" | awk '{print $1}')

    # Apply migration
    if sqlite3 "$DB_FILE" < "$migration" 2>/dev/null; then
        # Update checksum in migrations table
        sqlite3 "$DB_FILE" "UPDATE schema_migrations SET checksum = '$CHECKSUM' WHERE version = $VERSION;"
        echo -e "${GREEN}✓${NC} Migration $VERSION applied successfully"
        ((APPLIED++))
    else
        echo -e "${RED}✗${NC} Failed to apply migration $VERSION"
        ((FAILED++))
        break
    fi
done

echo ""
echo -e "${CYAN}Migration Summary:${NC}"
echo "  Applied: $APPLIED"
echo "  Skipped: $SKIPPED"
echo "  Failed: $FAILED"

# Get new schema version
NEW_VERSION=$(sqlite3 "$DB_FILE" "SELECT COALESCE(MAX(version), 0) FROM schema_migrations;")
echo ""
echo "New schema version: $NEW_VERSION"

# Verify database integrity
echo ""
echo -e "${CYAN}Verifying database integrity...${NC}"
INTEGRITY=$(sqlite3 "$DB_FILE" "PRAGMA integrity_check;")
if [ "$INTEGRITY" = "ok" ]; then
    echo -e "${GREEN}✓ Database integrity check passed${NC}"
else
    echo -e "${RED}✗ Database integrity check failed: $INTEGRITY${NC}"
    exit 1
fi

# Show table statistics
echo ""
echo -e "${CYAN}Database Statistics:${NC}"
sqlite3 "$DB_FILE" << EOF
.mode column
.headers on
SELECT
    name as 'Table',
    (SELECT COUNT(*) FROM pragma_table_info(name)) as 'Columns',
    (SELECT COUNT(*) FROM name) as 'Rows'
FROM sqlite_master
WHERE type = 'table'
    AND name NOT LIKE 'sqlite_%'
    AND name NOT LIKE 'v_%'
ORDER BY name;
EOF

if [ "$FAILED" -gt 0 ]; then
    echo ""
    echo -e "${RED}Migration failed! Please check the errors above.${NC}"
    exit 1
else
    echo ""
    echo -e "${GREEN}✓ All migrations completed successfully!${NC}"
    exit 0
fi