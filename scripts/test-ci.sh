#!/usr/bin/env bash
# Test CI/CD pipeline locally

set -e

echo "=== CI/CD Pipeline Test ==="
echo

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

# Test function
test_step() {
    local name=$1
    local cmd=$2
    echo -e "${YELLOW}Testing: $name${NC}"
    if eval "$cmd" > /dev/null 2>&1; then
        echo -e "${GREEN}✓ $name passed${NC}"
        return 0
    else
        echo -e "${RED}✗ $name failed${NC}"
        return 1
    fi
}

# Track failures
failures=0

# Test Makefile targets
test_step "Makefile help" "gmake help" || ((failures++))
test_step "Clean build artifacts" "gmake clean" || ((failures++))

# Test Guile module loading
test_step "Load core types" "guile -c '(add-to-load-path \"src\") (use-modules (deploy-ledger core types))'" || ((failures++))

# Storage and metrics modules require sqlite3 - skip if not available
if guile -c '(use-modules (sqlite3))' 2>/dev/null; then
    test_step "Load storage module" "guile -c '(add-to-load-path \"src\") (use-modules (deploy-ledger storage sqlite))'" || ((failures++))
    test_step "Load metrics module" "guile -c '(add-to-load-path \"src\") (use-modules (deploy-ledger query metrics))'" || ((failures++))
else
    echo -e "${YELLOW}⚠ Skipping storage/metrics tests (sqlite3 not installed)${NC}"
fi

# Test CLI script
test_step "CLI script exists" "test -f scripts/guile-deploy-ledger" || ((failures++))
test_step "CLI script executable" "test -x scripts/guile-deploy-ledger" || ((failures++))

# Test directory structure
test_step "Source directory exists" "test -d src" || ((failures++))
test_step "Test directory exists" "test -d tests" || ((failures++))
test_step "Examples directory exists" "test -d examples" || ((failures++))
test_step "Presentations directory exists" "test -d presentations" || ((failures++))

# Test configuration files
test_step "Dockerfile exists" "test -f Dockerfile" || ((failures++))
test_step "docker-compose.yml exists" "test -f docker-compose.yml" || ((failures++))
test_step "GitHub workflow exists" "test -f .github/workflows/ci.yml" || ((failures++))
test_step "Guix package exists" "test -f .guix.scm" || ((failures++))

# Test monitoring configuration
test_step "Prometheus config exists" "test -f monitoring/prometheus.yml" || ((failures++))
test_step "Alert rules exist" "test -f monitoring/alerts.yml" || ((failures++))

# Test presentations
test_step "Intro presentation exists" "test -f presentations/intro-to-deploy-ledger/intro-to-deploy-ledger.org" || ((failures++))
test_step "Technical presentation exists" "test -f presentations/technical-deep-dive/technical-deep-dive.org" || ((failures++))
test_step "Executive presentation exists" "test -f presentations/executive-overview/executive-overview.org" || ((failures++))

# Summary
echo
echo "=== Test Summary ==="
# Adjust total based on whether sqlite3 tests were run
if guile -c '(use-modules (sqlite3))' 2>/dev/null; then
    total_tests=20
else
    total_tests=18
fi
passed=$((total_tests - failures))
echo -e "Tests run: $total_tests"
echo -e "${GREEN}Passed: $passed${NC}"
if [ $failures -gt 0 ]; then
    echo -e "${RED}Failed: $failures${NC}"
    exit 1
else
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
fi