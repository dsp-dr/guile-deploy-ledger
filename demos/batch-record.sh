#!/usr/bin/env bash

# Batch recording script - Create 10 recordings and verify consistency

set +e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m'

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}     BATCH RECORDING - 10 ITERATIONS${NC}"
echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"
echo

# Create recordings directory
mkdir -p demos/recordings

echo -e "${YELLOW}Creating 10 asciicinema recordings...${NC}"
echo

for i in {1..10}; do
    echo -e "Recording $i/10..."

    # Create tmux session with unique name
    tmux new-session -d -s rec-$i -x 80 -y 24

    # Start recording in tmux
    tmux send-keys -t rec-$i "cd $PROJECT_ROOT" C-m
    tmux send-keys -t rec-$i "asciinema rec -q -c './demos/quick-demo.sh' demos/recordings/demo-$i.cast --overwrite" C-m

    # Wait for recording to complete
    sleep 5

    # Kill tmux session
    tmux kill-session -t rec-$i 2>/dev/null

    # Check if recording was created
    if [ -f "demos/recordings/demo-$i.cast" ]; then
        echo -e "  ${GREEN}✓${NC} Recording $i created"
        FILE_SIZE=$(wc -c < "demos/recordings/demo-$i.cast")
        echo -e "  Size: ${FILE_SIZE} bytes"
    else
        echo -e "  ${RED}✗${NC} Recording $i failed"
    fi
    echo
done

echo -e "${YELLOW}Converting recordings to GIFs...${NC}"
echo

# Convert each recording to GIF
for i in {1..10}; do
    if [ -f "demos/recordings/demo-$i.cast" ]; then
        echo -e "Converting recording $i..."
        agg "demos/recordings/demo-$i.cast" "demos/recordings/demo-$i.gif" \
            --theme monokai --font-size 14 --speed 1.0 >/dev/null 2>&1

        if [ -f "demos/recordings/demo-$i.gif" ]; then
            GIF_SIZE=$(du -h "demos/recordings/demo-$i.gif" | cut -f1)
            echo -e "  ${GREEN}✓${NC} GIF $i created (${GIF_SIZE})"
        else
            echo -e "  ${RED}✗${NC} GIF $i conversion failed"
        fi
    fi
done

echo
echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}                    ANALYSIS${NC}"
echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"
echo

# Analyze .cast file sizes
if ls demos/recordings/*.cast 1> /dev/null 2>&1; then
    echo -e "${MAGENTA}asciicinema File Sizes:${NC}"
    MIN_CAST=$(ls -la demos/recordings/*.cast | awk '{print $5}' | sort -n | head -1)
    MAX_CAST=$(ls -la demos/recordings/*.cast | awk '{print $5}' | sort -n | tail -1)
    echo "  Range: ${MIN_CAST} - ${MAX_CAST} bytes"
    CAST_DIFF=$((MAX_CAST - MIN_CAST))

    if [ $CAST_DIFF -lt 100 ]; then
        echo -e "  Consistency: ${GREEN}✓ Excellent${NC}"
    elif [ $CAST_DIFF -lt 500 ]; then
        echo -e "  Consistency: ${YELLOW}⚠ Good${NC}"
    else
        echo -e "  Consistency: ${RED}✗ Needs attention${NC}"
    fi
    echo
fi

# Analyze GIF file sizes
if ls demos/recordings/*.gif 1> /dev/null 2>&1; then
    echo -e "${MAGENTA}GIF File Sizes:${NC}"
    ls -lh demos/recordings/*.gif | awk '{print $9 ": " $5}'
    echo
fi

# Visual comparison prompt
echo -e "${CYAN}Visual Comparison:${NC}"
echo "To visually compare GIFs, you can:"
echo "  1. Open them in an image viewer"
echo "  2. Use imagemagick: montage demos/recordings/*.gif -tile 2x5 -geometry +2+2 comparison.png"
echo "  3. Create animated comparison: convert -delay 100 demos/recordings/*.gif comparison.gif"
echo

# Keep or clean?
echo -e "${YELLOW}Test files created in demos/recordings/${NC}"
echo "Run 'rm -rf demos/recordings' to clean up test files"
echo
echo -e "${GREEN}Batch recording test complete!${NC}"