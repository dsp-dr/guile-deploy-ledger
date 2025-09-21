#!/usr/bin/env bash

# Quick demo - Shows core functionality in 20 seconds

set +e

# Colors
GREEN='\033[0;32m'
CYAN='\033[0;36m'
YELLOW='\033[0;33m'
NC='\033[0m'

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

clear

echo -e "${CYAN}╔════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║  GUILE DEPLOY LEDGER - QUICK DEMO     ║${NC}"
echo -e "${CYAN}╚════════════════════════════════════════╝${NC}"
echo

sleep 1

echo "Running deployment example..."
echo
sleep 1

cd "$PROJECT_ROOT"
guile -L src --no-auto-compile examples/simple-deployment.scm 2>/dev/null

sleep 2

echo
echo -e "${GREEN}✓ Demo complete!${NC}"
echo
echo "Explore more with: ./demos/run-demos.sh"
echo

sleep 1