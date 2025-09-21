#!/usr/bin/env bash

# Comprehensive test suite for Guile Deploy Ledger
# Tests CLI, MCP server, and integrations

set -euo pipefail

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${BLUE}═══════════════════════════════════════${NC}"
echo -e "${BLUE} Guile Deploy Ledger - System Tests${NC}"
echo -e "${BLUE}═══════════════════════════════════════${NC}\n"

FAILED=0
PASSED=0

# Function to run test
run_test() {
    local name="$1"
    local cmd="$2"
    echo -ne "${YELLOW}Testing:${NC} $name ... "
    if eval "$cmd" > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC}"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}✗${NC}"
        FAILED=$((FAILED + 1))
    fi
}

# Test 1: CLI Version
echo -e "${CYAN}=== CLI Tests ===${NC}"
run_test "CLI version" "./gdl --version"
run_test "CLI help" "./gdl --help"
run_test "Version consistency" "./tests/test-cli-simple.sh"

# Test 2: Scripts
echo -e "\n${CYAN}=== Script Tests ===${NC}"
run_test "Deployment runner exists" "test -f scripts/run-deploy-ledger.sh"
run_test "List deployments" "./scripts/run-deploy-ledger.sh list-deployments"
run_test "Show metrics" "./scripts/run-deploy-ledger.sh show-metrics"
run_test "Health check" "./scripts/run-deploy-ledger.sh health"

# Test 3: MCP Server
echo -e "\n${CYAN}=== MCP Server Tests ===${NC}"
run_test "MCP server module" "test -f mcp-server/server.js"
run_test "MCP specification" "test -f mcp-server/deploy-ledger-mcp.json"
run_test "Webhook handler" "test -f mcp-server/webhook-handler.js"
run_test "MCP server tests" "cd mcp-server && node test-server.js"

# Test 4: Documentation
echo -e "\n${CYAN}=== Documentation Tests ===${NC}"
run_test "Man page exists" "test -f man/gdl.1"
run_test "Man page valid" "man -l man/gdl.1 2>/dev/null | head -1 | grep -q GDL"
run_test "README exists" "test -f README.org"
run_test "Claude config exists" "test -f claude_desktop_config.json"

# Test 5: Git/Version
echo -e "\n${CYAN}=== Version Management ===${NC}"
run_test "VERSION file exists" "test -f VERSION"
VERSION=$(cat VERSION)
run_test "Version format" "echo $VERSION | grep -E '^[0-9]+\.[0-9]+\.[0-9]+$'"
run_test "Git tags exist" "git tag | grep -q v"

# Test 6: CI/CD
echo -e "\n${CYAN}=== CI/CD Tests ===${NC}"
run_test "CI Simple workflow" "test -f .github/workflows/ci-simple.yml"
run_test "Version bump workflow" "test -f .github/workflows/version-bump.yml"
run_test "Makefile exists" "test -f Makefile"
run_test "Make targets" "make help > /dev/null 2>&1"

# Test 7: Integration Test
echo -e "\n${CYAN}=== Integration Tests ===${NC}"
echo "Recording test deployment..."
DEPLOY_ID=$(./scripts/run-deploy-ledger.sh record-deployment | grep -o '"id":"[^"]*"' | cut -d'"' -f4)
run_test "Deployment recorded" "test -n '$DEPLOY_ID'"

# Summary
echo -e "\n${BLUE}═══════════════════════════════════════${NC}"
echo -e "${BLUE} Test Summary${NC}"
echo -e "${BLUE}═══════════════════════════════════════${NC}"
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"

if [ $FAILED -eq 0 ]; then
    echo -e "\n${GREEN}✅ All systems operational!${NC}"
    exit 0
else
    echo -e "\n${RED}❌ Some tests failed${NC}"
    exit 1
fi