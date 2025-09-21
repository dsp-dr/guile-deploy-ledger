#!/usr/bin/env bash

# Test tmux recording consistency

set +e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m'

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}     TMUX RECORDING TEST - 10 ITERATIONS${NC}"
echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"
echo

cd "$PROJECT_ROOT"

for i in {1..10}; do
    echo -e "${YELLOW}Test $i/10:${NC}"

    # Create a new tmux session
    tmux new-session -d -s test-demo-$i -x 100 -y 30

    # Navigate to project
    tmux send-keys -t test-demo-$i "cd $PROJECT_ROOT" C-m

    # Run quick demo
    tmux send-keys -t test-demo-$i "./demos/quick-demo.sh" C-m

    # Wait for demo to complete
    sleep 3

    # Capture output
    tmux capture-pane -t test-demo-$i -p > "demos/tmux-test-$i.log"

    # Kill session
    tmux kill-session -t test-demo-$i 2>/dev/null

    # Check output
    if grep -q "Created deployment:" "demos/tmux-test-$i.log"; then
        echo -e "  ${GREEN}✓${NC} Deployment visible"
    else
        echo -e "  ${RED}✗${NC} Deployment not visible"
    fi

    if grep -q "Demo complete" "demos/tmux-test-$i.log"; then
        echo -e "  ${GREEN}✓${NC} Demo completed"
    else
        echo -e "  ${RED}✗${NC} Demo incomplete"
    fi

    # Show line count
    LINE_COUNT=$(wc -l < "demos/tmux-test-$i.log")
    echo -e "  Lines: $LINE_COUNT"

    echo
done

# Analyze consistency
echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}                 CONSISTENCY CHECK${NC}"
echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"
echo

# Compare line counts
MIN_LINES=$(ls demos/tmux-test-*.log 2>/dev/null | xargs -I {} wc -l {} | awk '{print $1}' | sort -n | head -1)
MAX_LINES=$(ls demos/tmux-test-*.log 2>/dev/null | xargs -I {} wc -l {} | awk '{print $1}' | sort -n | tail -1)

if [ -n "$MIN_LINES" ] && [ -n "$MAX_LINES" ]; then
    LINE_DIFF=$((MAX_LINES - MIN_LINES))
    echo "Line count range: ${MIN_LINES} - ${MAX_LINES}"

    if [ $LINE_DIFF -le 2 ]; then
        echo -e "Consistency: ${GREEN}✓ Excellent${NC} (diff: ${LINE_DIFF} lines)"
    elif [ $LINE_DIFF -le 5 ]; then
        echo -e "Consistency: ${YELLOW}⚠ Good${NC} (diff: ${LINE_DIFF} lines)"
    else
        echo -e "Consistency: ${RED}✗ Poor${NC} (diff: ${LINE_DIFF} lines)"
    fi
fi

# Clean up
echo
echo -e "${CYAN}Cleaning up test files...${NC}"
rm -f demos/tmux-test-*.log

echo -e "${GREEN}Tmux recording test complete!${NC}"