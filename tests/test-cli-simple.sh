#!/usr/bin/env bash

# Simple CLI Test for Guile Deploy Ledger
# Copyright (C) 2025 Daria Pascal (dsp-dr)

set -euo pipefail

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m'

echo "Testing gdl CLI..."

# Test 1: Version flag
echo -n "Testing --version flag... "
if ./gdl --version | grep -q "guile-deploy-ledger"; then
    echo -e "${GREEN}✓${NC}"
else
    echo -e "${RED}✗${NC}"
    exit 1
fi

# Test 2: Help flag
echo -n "Testing --help flag... "
if ./gdl --help | grep -q "Usage:"; then
    echo -e "${GREEN}✓${NC}"
else
    echo -e "${RED}✗${NC}"
    exit 1
fi

# Test 3: Version from file
echo -n "Testing VERSION file reading... "
FILE_VERSION=$(cat VERSION)
CLI_VERSION=$(./gdl --version | head -1 | awk '{print $2}')
if [ "$FILE_VERSION" = "$CLI_VERSION" ]; then
    echo -e "${GREEN}✓${NC} (v$FILE_VERSION)"
else
    echo -e "${RED}✗${NC} (file: $FILE_VERSION, cli: $CLI_VERSION)"
    exit 1
fi

echo -e "${GREEN}All tests passed!${NC}"