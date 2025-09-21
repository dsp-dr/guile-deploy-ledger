#!/usr/bin/env bash

# Test script to run demo recordings multiple times
# and verify consistency

set +e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}     DEMO RECORDING QUALITY CHECK - 10 ITERATIONS${NC}"
echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"
echo

cd "$PROJECT_ROOT"

# Function to run and check demo
run_demo_check() {
    local iteration=$1
    local output_file="demos/test-run-${iteration}.log"

    echo -e "${YELLOW}Iteration $iteration/10${NC}"

    # Run the quick demo and capture output
    ./demos/quick-demo.sh > "$output_file" 2>&1
    local exit_code=$?

    if [ $exit_code -eq 0 ]; then
        echo -e "  ${GREEN}✓${NC} Demo completed successfully"
    else
        echo -e "  ${RED}✗${NC} Demo failed with exit code: $exit_code"
    fi

    # Check for expected content
    if grep -q "Created deployment:" "$output_file"; then
        echo -e "  ${GREEN}✓${NC} Deployment creation verified"
    else
        echo -e "  ${RED}✗${NC} Deployment creation missing"
    fi

    if grep -q "Service metadata:" "$output_file"; then
        echo -e "  ${GREEN}✓${NC} Service metadata verified"
    else
        echo -e "  ${RED}✗${NC} Service metadata missing"
    fi

    if grep -q "Created rollback:" "$output_file"; then
        echo -e "  ${GREEN}✓${NC} Rollback creation verified"
    else
        echo -e "  ${RED}✗${NC} Rollback creation missing"
    fi

    # Check file size (should be consistent)
    local file_size=$(wc -c < "$output_file")
    echo -e "  File size: ${file_size} bytes"

    echo
    return $exit_code
}

# Track results
SUCCESS_COUNT=0
FAILURE_COUNT=0
SIZES=()

# Run 10 iterations
for i in {1..10}; do
    if run_demo_check $i; then
        ((SUCCESS_COUNT++))
    else
        ((FAILURE_COUNT++))
    fi

    # Store file size for consistency check
    SIZES+=("$(wc -c < "demos/test-run-${i}.log")")

    # Small delay between runs
    sleep 0.5
done

# Analyze results
echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}                      SUMMARY${NC}"
echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"
echo

echo -e "Success Rate: ${GREEN}$SUCCESS_COUNT/10${NC}"
if [ $FAILURE_COUNT -gt 0 ]; then
    echo -e "Failures: ${RED}$FAILURE_COUNT/10${NC}"
fi

# Check size consistency
MIN_SIZE=$(printf '%s\n' "${SIZES[@]}" | sort -n | head -1)
MAX_SIZE=$(printf '%s\n' "${SIZES[@]}" | sort -n | tail -1)
SIZE_DIFF=$((MAX_SIZE - MIN_SIZE))

echo -e "Output Size Range: ${MIN_SIZE} - ${MAX_SIZE} bytes"
if [ $SIZE_DIFF -lt 100 ]; then
    echo -e "Size Consistency: ${GREEN}✓ Excellent${NC} (diff: ${SIZE_DIFF} bytes)"
elif [ $SIZE_DIFF -lt 500 ]; then
    echo -e "Size Consistency: ${YELLOW}⚠ Good${NC} (diff: ${SIZE_DIFF} bytes)"
else
    echo -e "Size Consistency: ${RED}✗ Poor${NC} (diff: ${SIZE_DIFF} bytes)"
fi

echo

# Clean up test files
echo -e "${CYAN}Cleaning up test files...${NC}"
rm -f demos/test-run-*.log

echo -e "${GREEN}Quality check complete!${NC}"