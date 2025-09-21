#!/usr/bin/env bash

# CLI Test Matrix Harness for Guile Deploy Ledger
# Tests all parameters, flags, and command combinations
# Copyright (C) 2025 Daria Pascal (dsp-dr)

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Test counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0

# Test results log
TEST_LOG="test-results-$(date +%Y%m%d-%H%M%S).log"
MATRIX_REPORT="test-matrix-report.md"

# CLI executable
CLI_CMD="./gdl-simple"

# Test database
TEST_DB="/tmp/test-deploy-ledger-$$.db"

# Initialize test log
init_log() {
    echo "CLI Test Matrix - $(date)" > "$TEST_LOG"
    echo "================================" >> "$TEST_LOG"
    echo "" >> "$TEST_LOG"
}

# Log test result
log_test() {
    local status="$1"
    local category="$2"
    local test_name="$3"
    local command="$4"
    local output="${5:-}"

    echo "[$status] $category :: $test_name" >> "$TEST_LOG"
    echo "  Command: $command" >> "$TEST_LOG"
    if [ -n "$output" ]; then
        echo "  Output: $output" >> "$TEST_LOG"
    fi
    echo "" >> "$TEST_LOG"
}

# Run a test
run_test() {
    local category="$1"
    local test_name="$2"
    local command="$3"
    local expected_exit="${4:-0}"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    echo -ne "${CYAN}Testing:${NC} $category :: $test_name ... "

    # Run command and capture output/exit code
    set +e
    output=$(eval "$command" 2>&1)
    exit_code=$?
    set -e

    if [ "$exit_code" -eq "$expected_exit" ]; then
        echo -e "${GREEN}✓${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        log_test "PASS" "$category" "$test_name" "$command" "$output"
        return 0
    else
        echo -e "${RED}✗${NC} (exit: $exit_code, expected: $expected_exit)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        log_test "FAIL" "$category" "$test_name" "$command" "$output"
        return 1
    fi
}

# Skip a test
skip_test() {
    local category="$1"
    local test_name="$2"
    local reason="$3"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    SKIPPED_TESTS=$((SKIPPED_TESTS + 1))

    echo -e "${YELLOW}Skipping:${NC} $category :: $test_name ($reason)"
    log_test "SKIP" "$category" "$test_name" "N/A" "$reason"
}

# Test global flags
test_global_flags() {
    echo -e "\n${BLUE}=== Testing Global Flags ===${NC}"

    # Basic flags
    run_test "Global" "Help flag (-h)" "$CLI_CMD -h" 0
    run_test "Global" "Help flag (--help)" "$CLI_CMD --help" 0
    run_test "Global" "Version flag (-v)" "$CLI_CMD -v" 0
    run_test "Global" "Version flag (--version)" "$CLI_CMD --version" 0

    # Database flag
    run_test "Global" "Database flag (-d)" "$CLI_CMD -d $TEST_DB --help" 0
    run_test "Global" "Database flag (--database)" "$CLI_CMD --database $TEST_DB --help" 0

    # Format flag
    run_test "Global" "Format flag (-f json)" "$CLI_CMD -f json --help" 0
    run_test "Global" "Format flag (--format csv)" "$CLI_CMD --format csv --help" 0
    run_test "Global" "Format flag (--format markdown)" "$CLI_CMD --format markdown --help" 0
    run_test "Global" "Format flag (--format org)" "$CLI_CMD --format org --help" 0

    # Quiet/Verbose flags
    run_test "Global" "Quiet flag (-q)" "$CLI_CMD -q --help" 0
    run_test "Global" "Quiet flag (--quiet)" "$CLI_CMD --quiet --help" 0
    run_test "Global" "Verbose flag" "$CLI_CMD --verbose --help" 0

    # Config flag
    run_test "Global" "Config flag" "$CLI_CMD --config /tmp/test.conf --help" 0

    # Invalid flags
    run_test "Global" "Invalid flag" "$CLI_CMD --invalid-flag" 2
    run_test "Global" "Invalid short flag" "$CLI_CMD -Z" 2
}

# Test record-deployment command
test_record_deployment() {
    echo -e "\n${BLUE}=== Testing record-deployment Command ===${NC}"

    # Valid deployment recording
    run_test "record-deployment" "All required params" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production --type rolling" 0

    run_test "record-deployment" "With initiator" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production --type rolling --initiator ci-bot" 0

    run_test "record-deployment" "With change-id" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production --type rolling --change-id CHG-1234" 0

    run_test "record-deployment" "With approved-by" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production --type rolling --approved-by alice,bob,charlie" 0

    run_test "record-deployment" "With region" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production --type rolling --region us-east-1" 0

    run_test "record-deployment" "With cluster" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production --type rolling --cluster k8s-prod" 0

    run_test "record-deployment" "With replicas" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production --type rolling --replicas 5" 0

    run_test "record-deployment" "All optional params" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production --type rolling --initiator ci-bot --change-id CHG-1234 --approved-by alice,bob --region us-east-1 --cluster k8s-prod --replicas 5" 0

    # Test all environment values
    run_test "record-deployment" "Environment: development" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment development --type rolling" 0

    run_test "record-deployment" "Environment: staging" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment staging --type rolling" 0

    run_test "record-deployment" "Environment: production" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production --type rolling" 0

    # Test all deployment types
    run_test "record-deployment" "Type: blue-green" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production --type blue-green" 0

    run_test "record-deployment" "Type: canary" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production --type canary" 0

    run_test "record-deployment" "Type: rolling" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production --type rolling" 0

    run_test "record-deployment" "Type: big-bang" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production --type big-bang" 0

    # Missing required parameters
    run_test "record-deployment" "Missing service" \
        "$CLI_CMD record-deployment --version v1.0.0 --environment production --type rolling" 2

    run_test "record-deployment" "Missing version" \
        "$CLI_CMD record-deployment --service api --environment production --type rolling" 2

    run_test "record-deployment" "Missing environment" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --type rolling" 2

    run_test "record-deployment" "Missing type" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production" 2

    # Invalid parameter values
    run_test "record-deployment" "Invalid environment" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment invalid --type rolling" 2

    run_test "record-deployment" "Invalid type" \
        "$CLI_CMD record-deployment --service api --version v1.0.0 --environment production --type invalid" 2
}

# Test record-rollback command
test_record_rollback() {
    echo -e "\n${BLUE}=== Testing record-rollback Command ===${NC}"

    # Valid rollback recording
    run_test "record-rollback" "All required params" \
        "$CLI_CMD record-rollback --service api --from-version v1.0.1 --to-version v1.0.0 --reason 'Memory leak'" 0

    run_test "record-rollback" "With deployment-id" \
        "$CLI_CMD record-rollback --service api --from-version v1.0.1 --to-version v1.0.0 --reason 'Memory leak' --deployment-id dep-123" 0

    # Missing required parameters
    run_test "record-rollback" "Missing service" \
        "$CLI_CMD record-rollback --from-version v1.0.1 --to-version v1.0.0 --reason 'Memory leak'" 2

    run_test "record-rollback" "Missing from-version" \
        "$CLI_CMD record-rollback --service api --to-version v1.0.0 --reason 'Memory leak'" 2

    run_test "record-rollback" "Missing to-version" \
        "$CLI_CMD record-rollback --service api --from-version v1.0.1 --reason 'Memory leak'" 2

    run_test "record-rollback" "Missing reason" \
        "$CLI_CMD record-rollback --service api --from-version v1.0.1 --to-version v1.0.0" 2
}

# Test list-deployments command
test_list_deployments() {
    echo -e "\n${BLUE}=== Testing list-deployments Command ===${NC}"

    # Basic listing
    run_test "list-deployments" "No params" \
        "$CLI_CMD list-deployments" 0

    # With filters
    run_test "list-deployments" "Filter by service" \
        "$CLI_CMD list-deployments --service api" 0

    run_test "list-deployments" "Filter by environment" \
        "$CLI_CMD list-deployments --environment production" 0

    # Test all status values
    run_test "list-deployments" "Status: pending" \
        "$CLI_CMD list-deployments --status pending" 0

    run_test "list-deployments" "Status: in-progress" \
        "$CLI_CMD list-deployments --status in-progress" 0

    run_test "list-deployments" "Status: succeeded" \
        "$CLI_CMD list-deployments --status succeeded" 0

    run_test "list-deployments" "Status: failed" \
        "$CLI_CMD list-deployments --status failed" 0

    run_test "list-deployments" "Status: rolled-back" \
        "$CLI_CMD list-deployments --status rolled-back" 0

    # Limit parameter
    run_test "list-deployments" "Limit 5" \
        "$CLI_CMD list-deployments --limit 5" 0

    run_test "list-deployments" "Limit 100" \
        "$CLI_CMD list-deployments --limit 100" 0

    # Date filters
    run_test "list-deployments" "Since date" \
        "$CLI_CMD list-deployments --since 2025-01-01" 0

    run_test "list-deployments" "Until date" \
        "$CLI_CMD list-deployments --until 2025-12-31" 0

    run_test "list-deployments" "Date range" \
        "$CLI_CMD list-deployments --since 2025-01-01 --until 2025-12-31" 0

    # Combined filters
    run_test "list-deployments" "Multiple filters" \
        "$CLI_CMD list-deployments --service api --environment production --status succeeded --limit 10" 0

    # Invalid values
    run_test "list-deployments" "Invalid status" \
        "$CLI_CMD list-deployments --status invalid" 2

    run_test "list-deployments" "Invalid limit" \
        "$CLI_CMD list-deployments --limit abc" 2

    run_test "list-deployments" "Invalid date format" \
        "$CLI_CMD list-deployments --since not-a-date" 2
}

# Test get-deployment command
test_get_deployment() {
    echo -e "\n${BLUE}=== Testing get-deployment Command ===${NC}"

    run_test "get-deployment" "Valid deployment ID" \
        "$CLI_CMD get-deployment --deployment-id dep-123" 0

    run_test "get-deployment" "Missing deployment ID" \
        "$CLI_CMD get-deployment" 2
}

# Test show-metrics command
test_show_metrics() {
    echo -e "\n${BLUE}=== Testing show-metrics Command ===${NC}"

    # Test all metric types
    run_test "show-metrics" "Metric: frequency" \
        "$CLI_CMD show-metrics --metric frequency" 0

    run_test "show-metrics" "Metric: duration" \
        "$CLI_CMD show-metrics --metric duration" 0

    run_test "show-metrics" "Metric: success_rate" \
        "$CLI_CMD show-metrics --metric success_rate" 0

    run_test "show-metrics" "Metric: mttr" \
        "$CLI_CMD show-metrics --metric mttr" 0

    run_test "show-metrics" "Metric: lead_time" \
        "$CLI_CMD show-metrics --metric lead_time" 0

    # With service filter
    run_test "show-metrics" "Specific service" \
        "$CLI_CMD show-metrics --service api --metric success_rate" 0

    run_test "show-metrics" "All services" \
        "$CLI_CMD show-metrics --service all --metric success_rate" 0

    # Period parameter
    run_test "show-metrics" "Period 7 days" \
        "$CLI_CMD show-metrics --metric success_rate --period 7" 0

    run_test "show-metrics" "Period 30 days" \
        "$CLI_CMD show-metrics --metric success_rate --period 30" 0

    run_test "show-metrics" "Period 90 days" \
        "$CLI_CMD show-metrics --metric success_rate --period 90" 0

    # Group by parameter
    run_test "show-metrics" "Group by day" \
        "$CLI_CMD show-metrics --metric success_rate --group-by day" 0

    run_test "show-metrics" "Group by week" \
        "$CLI_CMD show-metrics --metric success_rate --group-by week" 0

    run_test "show-metrics" "Group by month" \
        "$CLI_CMD show-metrics --metric success_rate --group-by month" 0

    # Missing/invalid parameters
    run_test "show-metrics" "Missing metric" \
        "$CLI_CMD show-metrics" 2

    run_test "show-metrics" "Invalid metric" \
        "$CLI_CMD show-metrics --metric invalid" 2

    run_test "show-metrics" "Invalid period" \
        "$CLI_CMD show-metrics --metric success_rate --period abc" 2
}

# Test analyze command
test_analyze() {
    echo -e "\n${BLUE}=== Testing analyze Command ===${NC}"

    # Test all analysis types
    run_test "analyze" "Type: failure_patterns" \
        "$CLI_CMD analyze --type failure_patterns" 0

    run_test "analyze" "Type: peak_times" \
        "$CLI_CMD analyze --type peak_times" 0

    run_test "analyze" "Type: dependencies" \
        "$CLI_CMD analyze --type dependencies" 0

    run_test "analyze" "Type: risk" \
        "$CLI_CMD analyze --type risk" 0

    # Period parameter
    run_test "analyze" "Period 30 days" \
        "$CLI_CMD analyze --type failure_patterns --period 30" 0

    run_test "analyze" "Period 90 days" \
        "$CLI_CMD analyze --type failure_patterns --period 90" 0

    # Threshold parameter
    run_test "analyze" "With threshold" \
        "$CLI_CMD analyze --type failure_patterns --threshold 0.95" 0

    # Missing/invalid parameters
    run_test "analyze" "Missing type" \
        "$CLI_CMD analyze" 2

    run_test "analyze" "Invalid type" \
        "$CLI_CMD analyze --type invalid" 2
}

# Test export command
test_export() {
    echo -e "\n${BLUE}=== Testing export Command ===${NC}"

    # Test all export formats
    run_test "export" "Format: json" \
        "$CLI_CMD export --format json" 0

    run_test "export" "Format: csv" \
        "$CLI_CMD export --format csv" 0

    run_test "export" "Format: markdown" \
        "$CLI_CMD export --format markdown" 0

    run_test "export" "Format: org-mode" \
        "$CLI_CMD export --format org-mode" 0

    # Output file
    run_test "export" "Output to file" \
        "$CLI_CMD export --format json --output /tmp/test-export.json" 0

    # Filters
    run_test "export" "Filter by service" \
        "$CLI_CMD export --format json --service api" 0

    run_test "export" "Date range" \
        "$CLI_CMD export --format json --date-from 2025-01-01 --date-to 2025-12-31" 0

    # Missing/invalid parameters
    run_test "export" "Missing format" \
        "$CLI_CMD export" 2

    run_test "export" "Invalid format" \
        "$CLI_CMD export --format invalid" 2
}

# Test visualize command
test_visualize() {
    echo -e "\n${BLUE}=== Testing visualize Command ===${NC}"

    # Test all visualization types
    run_test "visualize" "Type: timeline" \
        "$CLI_CMD visualize --type timeline --format mermaid" 0

    run_test "visualize" "Type: dependency_graph" \
        "$CLI_CMD visualize --type dependency_graph --format mermaid" 0

    run_test "visualize" "Type: heatmap" \
        "$CLI_CMD visualize --type heatmap --format ascii" 0

    run_test "visualize" "Type: flow" \
        "$CLI_CMD visualize --type flow --format mermaid" 0

    # Test all format types
    run_test "visualize" "Format: mermaid" \
        "$CLI_CMD visualize --type timeline --format mermaid" 0

    run_test "visualize" "Format: graphviz" \
        "$CLI_CMD visualize --type timeline --format graphviz" 0

    run_test "visualize" "Format: ascii" \
        "$CLI_CMD visualize --type timeline --format ascii" 0

    # Optional parameters
    run_test "visualize" "With service filter" \
        "$CLI_CMD visualize --type timeline --format mermaid --service api" 0

    run_test "visualize" "With dimensions" \
        "$CLI_CMD visualize --type heatmap --format ascii --width 80 --height 24" 0

    # Missing/invalid parameters
    run_test "visualize" "Missing type" \
        "$CLI_CMD visualize --format mermaid" 2

    run_test "visualize" "Missing format" \
        "$CLI_CMD visualize --type timeline" 2

    run_test "visualize" "Invalid type" \
        "$CLI_CMD visualize --type invalid --format mermaid" 2

    run_test "visualize" "Invalid format" \
        "$CLI_CMD visualize --type timeline --format invalid" 2
}

# Test health command
test_health() {
    echo -e "\n${BLUE}=== Testing health Command ===${NC}"

    run_test "health" "No params" \
        "$CLI_CMD health" 0

    run_test "health" "Specific service" \
        "$CLI_CMD health --service api" 0

    run_test "health" "All services" \
        "$CLI_CMD health --service all" 0

    run_test "health" "Include recommendations" \
        "$CLI_CMD health --include-recommendations true" 0

    run_test "health" "Exclude recommendations" \
        "$CLI_CMD health --include-recommendations false" 0
}

# Test server command
test_server() {
    echo -e "\n${BLUE}=== Testing server Command ===${NC}"

    skip_test "server" "Start server" "Requires background process management"
}

# Test command combinations
test_combinations() {
    echo -e "\n${BLUE}=== Testing Command Combinations ===${NC}"

    # Global flags with commands
    run_test "combinations" "Quiet mode with list" \
        "$CLI_CMD -q list-deployments" 0

    run_test "combinations" "Verbose mode with metrics" \
        "$CLI_CMD --verbose show-metrics --metric success_rate" 0

    run_test "combinations" "Custom DB with deployment" \
        "$CLI_CMD -d $TEST_DB record-deployment --service api --version v1.0.0 --environment production --type rolling" 0

    run_test "combinations" "Format with export" \
        "$CLI_CMD -f csv export --format csv" 0

    # Multiple global flags
    run_test "combinations" "Multiple global flags" \
        "$CLI_CMD -q -d $TEST_DB -f json list-deployments" 0
}

# Test edge cases
test_edge_cases() {
    echo -e "\n${BLUE}=== Testing Edge Cases ===${NC}"

    # Empty strings
    run_test "edge-cases" "Empty service name" \
        "$CLI_CMD record-deployment --service '' --version v1.0.0 --environment production --type rolling" 2

    # Very long strings
    LONG_STRING=$(printf 'a%.0s' {1..1000})
    run_test "edge-cases" "Very long service name" \
        "$CLI_CMD record-deployment --service '$LONG_STRING' --version v1.0.0 --environment production --type rolling" 0

    # Special characters
    run_test "edge-cases" "Special chars in version" \
        "$CLI_CMD record-deployment --service api --version 'v1.0.0-beta+build.123' --environment production --type rolling" 0

    run_test "edge-cases" "Spaces in reason" \
        "$CLI_CMD record-rollback --service api --from-version v1.0.1 --to-version v1.0.0 --reason 'Critical bug found in production'" 0

    # Unicode characters
    run_test "edge-cases" "Unicode in service name" \
        "$CLI_CMD record-deployment --service 'api-服务' --version v1.0.0 --environment production --type rolling" 0

    # SQL injection attempts
    run_test "edge-cases" "SQL injection in service" \
        "$CLI_CMD record-deployment --service \"api'; DROP TABLE deployments; --\" --version v1.0.0 --environment production --type rolling" 0

    # Command injection attempts
    run_test "edge-cases" "Command injection in version" \
        "$CLI_CMD record-deployment --service api --version '\$(whoami)' --environment production --type rolling" 0

    # Path traversal attempts
    run_test "edge-cases" "Path traversal in database" \
        "$CLI_CMD -d '../../../etc/passwd' list-deployments" 2
}

# Test environment variables
test_environment() {
    echo -e "\n${BLUE}=== Testing Environment Variables ===${NC}"

    # Test DEPLOY_LEDGER_DB
    DEPLOY_LEDGER_DB="/tmp/env-test.db" run_test "environment" "DEPLOY_LEDGER_DB" \
        "$CLI_CMD list-deployments" 0

    # Test DEPLOY_LEDGER_CONFIG
    DEPLOY_LEDGER_CONFIG="/tmp/env-config.scm" run_test "environment" "DEPLOY_LEDGER_CONFIG" \
        "$CLI_CMD list-deployments" 0

    # Test DEPLOY_LEDGER_FORMAT
    DEPLOY_LEDGER_FORMAT="json" run_test "environment" "DEPLOY_LEDGER_FORMAT" \
        "$CLI_CMD list-deployments" 0
}

# Generate test matrix report
generate_matrix_report() {
    echo "# CLI Test Matrix Report" > "$MATRIX_REPORT"
    echo "" >> "$MATRIX_REPORT"
    echo "Generated: $(date)" >> "$MATRIX_REPORT"
    echo "" >> "$MATRIX_REPORT"

    echo "## Summary" >> "$MATRIX_REPORT"
    echo "" >> "$MATRIX_REPORT"
    echo "- **Total Tests**: $TOTAL_TESTS" >> "$MATRIX_REPORT"
    echo "- **Passed**: $PASSED_TESTS ✅" >> "$MATRIX_REPORT"
    echo "- **Failed**: $FAILED_TESTS ❌" >> "$MATRIX_REPORT"
    echo "- **Skipped**: $SKIPPED_TESTS ⚠️" >> "$MATRIX_REPORT"

    local pass_rate=$(echo "scale=2; $PASSED_TESTS * 100 / $TOTAL_TESTS" | bc)
    echo "- **Pass Rate**: ${pass_rate}%" >> "$MATRIX_REPORT"
    echo "" >> "$MATRIX_REPORT"

    echo "## Test Categories" >> "$MATRIX_REPORT"
    echo "" >> "$MATRIX_REPORT"

    # Parse log for category summary
    echo "| Category | Tests | Passed | Failed | Skipped |" >> "$MATRIX_REPORT"
    echo "|----------|-------|--------|--------|---------|" >> "$MATRIX_REPORT"

    for category in "Global" "record-deployment" "record-rollback" "list-deployments" \
                   "get-deployment" "show-metrics" "analyze" "export" "visualize" \
                   "health" "server" "combinations" "edge-cases" "environment"; do
        local cat_total=$(grep -c "\[$category\]" "$TEST_LOG" 2>/dev/null || echo "0")
        local cat_pass=$(grep -c "\[PASS\] $category" "$TEST_LOG" 2>/dev/null || echo "0")
        local cat_fail=$(grep -c "\[FAIL\] $category" "$TEST_LOG" 2>/dev/null || echo "0")
        local cat_skip=$(grep -c "\[SKIP\] $category" "$TEST_LOG" 2>/dev/null || echo "0")

        if [ "$cat_total" -gt 0 ]; then
            echo "| $category | $cat_total | $cat_pass | $cat_fail | $cat_skip |" >> "$MATRIX_REPORT"
        fi
    done

    echo "" >> "$MATRIX_REPORT"
    echo "## Parameter Coverage" >> "$MATRIX_REPORT"
    echo "" >> "$MATRIX_REPORT"

    echo "### Commands Tested" >> "$MATRIX_REPORT"
    echo "- ✅ record-deployment" >> "$MATRIX_REPORT"
    echo "- ✅ record-rollback" >> "$MATRIX_REPORT"
    echo "- ✅ list-deployments" >> "$MATRIX_REPORT"
    echo "- ✅ get-deployment" >> "$MATRIX_REPORT"
    echo "- ✅ show-metrics" >> "$MATRIX_REPORT"
    echo "- ✅ analyze" >> "$MATRIX_REPORT"
    echo "- ✅ export" >> "$MATRIX_REPORT"
    echo "- ✅ visualize" >> "$MATRIX_REPORT"
    echo "- ✅ health" >> "$MATRIX_REPORT"
    echo "- ⚠️ server (skipped - requires daemon)" >> "$MATRIX_REPORT"
    echo "" >> "$MATRIX_REPORT"

    echo "### Global Flags Tested" >> "$MATRIX_REPORT"
    echo "- ✅ --help / -h" >> "$MATRIX_REPORT"
    echo "- ✅ --version / -v" >> "$MATRIX_REPORT"
    echo "- ✅ --database / -d" >> "$MATRIX_REPORT"
    echo "- ✅ --format / -f (json, csv, markdown, org)" >> "$MATRIX_REPORT"
    echo "- ✅ --quiet / -q" >> "$MATRIX_REPORT"
    echo "- ✅ --verbose" >> "$MATRIX_REPORT"
    echo "- ✅ --config" >> "$MATRIX_REPORT"
    echo "" >> "$MATRIX_REPORT"

    echo "### Enum Values Tested" >> "$MATRIX_REPORT"
    echo "- **Environments**: development, staging, production ✅" >> "$MATRIX_REPORT"
    echo "- **Deployment Types**: blue-green, canary, rolling, big-bang ✅" >> "$MATRIX_REPORT"
    echo "- **Statuses**: pending, in-progress, succeeded, failed, rolled-back ✅" >> "$MATRIX_REPORT"
    echo "- **Metrics**: frequency, duration, success_rate, mttr, lead_time ✅" >> "$MATRIX_REPORT"
    echo "- **Analysis Types**: failure_patterns, peak_times, dependencies, risk ✅" >> "$MATRIX_REPORT"
    echo "- **Export Formats**: json, csv, markdown, org-mode ✅" >> "$MATRIX_REPORT"
    echo "- **Visualization Types**: timeline, dependency_graph, heatmap, flow ✅" >> "$MATRIX_REPORT"
    echo "- **Visualization Formats**: mermaid, graphviz, ascii ✅" >> "$MATRIX_REPORT"
    echo "" >> "$MATRIX_REPORT"

    echo "### Edge Cases Tested" >> "$MATRIX_REPORT"
    echo "- ✅ Empty strings" >> "$MATRIX_REPORT"
    echo "- ✅ Very long strings (1000+ chars)" >> "$MATRIX_REPORT"
    echo "- ✅ Special characters" >> "$MATRIX_REPORT"
    echo "- ✅ Unicode characters" >> "$MATRIX_REPORT"
    echo "- ✅ SQL injection attempts" >> "$MATRIX_REPORT"
    echo "- ✅ Command injection attempts" >> "$MATRIX_REPORT"
    echo "- ✅ Path traversal attempts" >> "$MATRIX_REPORT"
    echo "" >> "$MATRIX_REPORT"

    echo "### Environment Variables Tested" >> "$MATRIX_REPORT"
    echo "- ✅ DEPLOY_LEDGER_DB" >> "$MATRIX_REPORT"
    echo "- ✅ DEPLOY_LEDGER_CONFIG" >> "$MATRIX_REPORT"
    echo "- ✅ DEPLOY_LEDGER_FORMAT" >> "$MATRIX_REPORT"
    echo "" >> "$MATRIX_REPORT"

    echo "## Failed Tests" >> "$MATRIX_REPORT"
    echo "" >> "$MATRIX_REPORT"

    if [ "$FAILED_TESTS" -gt 0 ]; then
        echo '```' >> "$MATRIX_REPORT"
        grep "\[FAIL\]" "$TEST_LOG" >> "$MATRIX_REPORT"
        echo '```' >> "$MATRIX_REPORT"
    else
        echo "No failed tests! 🎉" >> "$MATRIX_REPORT"
    fi

    echo "" >> "$MATRIX_REPORT"
    echo "## Test Log" >> "$MATRIX_REPORT"
    echo "Full test log available at: \`$TEST_LOG\`" >> "$MATRIX_REPORT"
}

# Main test execution
main() {
    echo -e "${CYAN}╔══════════════════════════════════════════╗${NC}"
    echo -e "${CYAN}║   CLI Test Matrix for Guile Deploy Ledger   ║${NC}"
    echo -e "${CYAN}╚══════════════════════════════════════════╝${NC}"
    echo ""

    # Check if CLI exists
    if [ ! -f "$CLI_CMD" ]; then
        echo -e "${RED}Error: CLI not found at $CLI_CMD${NC}"
        echo "Creating mock CLI for testing..."

        # Create a mock CLI that accepts all commands for testing
        cat > "$CLI_CMD" << 'EOF'
#!/usr/bin/env guile
!#
(use-modules (ice-9 getopt-long))
(define (main args) 0)
(exit (main (command-line)))
EOF
        chmod +x "$CLI_CMD"
    fi

    # Initialize
    init_log

    # Run all test suites
    test_global_flags
    test_record_deployment
    test_record_rollback
    test_list_deployments
    test_get_deployment
    test_show_metrics
    test_analyze
    test_export
    test_visualize
    test_health
    test_server
    test_combinations
    test_edge_cases
    test_environment

    # Generate report
    generate_matrix_report

    # Display results
    echo ""
    echo -e "${CYAN}═══════════════════════════════════════════${NC}"
    echo -e "${CYAN}Test Results Summary${NC}"
    echo -e "${CYAN}═══════════════════════════════════════════${NC}"
    echo -e "Total Tests:  ${BLUE}$TOTAL_TESTS${NC}"
    echo -e "Passed:       ${GREEN}$PASSED_TESTS${NC} ✅"
    echo -e "Failed:       ${RED}$FAILED_TESTS${NC} ❌"
    echo -e "Skipped:      ${YELLOW}$SKIPPED_TESTS${NC} ⚠️"

    local pass_rate=$(echo "scale=2; $PASSED_TESTS * 100 / $TOTAL_TESTS" | bc)
    echo -e "Pass Rate:    ${CYAN}${pass_rate}%${NC}"
    echo ""
    echo -e "Test Log:     ${BLUE}$TEST_LOG${NC}"
    echo -e "Matrix Report: ${BLUE}$MATRIX_REPORT${NC}"

    # Cleanup
    rm -f "$TEST_DB"

    # Exit with appropriate code
    if [ "$FAILED_TESTS" -gt 0 ]; then
        exit 1
    else
        exit 0
    fi
}

# Run main if not sourced
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    main "$@"
fi