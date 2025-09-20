#!/usr/bin/env bash
# preflight-check.sh - Pre-flight checks before build

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}═══════════════════════════════════════${NC}"
echo -e "${CYAN}     Pre-flight Checks                 ${NC}"
echo -e "${CYAN}═══════════════════════════════════════${NC}"
echo ""

WARNINGS=0
ERRORS=0

# Function to check condition
check() {
    local test_name=$1
    local command=$2
    local severity=$3  # info, warning, error

    echo -n "Checking $test_name... "

    if eval "$command" > /dev/null 2>&1; then
        echo -e "${GREEN}OK${NC}"
        return 0
    else
        case $severity in
            error)
                echo -e "${RED}FAILED${NC}"
                ((ERRORS++))
                ;;
            warning)
                echo -e "${YELLOW}WARNING${NC}"
                ((WARNINGS++))
                ;;
            info)
                echo -e "${CYAN}INFO${NC}"
                ;;
        esac
        return 1
    fi
}

# System checks
echo -e "${CYAN}System Checks:${NC}"
check "Operating System" "uname -s" "info"
check "Architecture" "uname -m" "info"
check "User permissions" "[ -w . ]" "error"
check "Network connectivity" "ping -c 1 github.com 2>/dev/null || ping -c 1 8.8.8.8" "warning"

echo ""

# Tool availability
echo -e "${CYAN}Tool Availability:${NC}"
check "Guile interpreter" "command -v guile" "error"
check "Guild compiler" "command -v guild" "error"
check "SQLite3" "command -v sqlite3" "error"
check "Git" "command -v git" "error"
check "Make" "command -v make || command -v gmake" "error"
check "Bash" "command -v bash" "error"

echo ""

# Environment checks
echo -e "${CYAN}Environment Checks:${NC}"
check "Home directory writable" "[ -w \"$HOME\" ]" "error"
check "Temp directory writable" "[ -w /tmp ]" "warning"
check "Load path configured" "[ -n \"$GUILE_LOAD_PATH\" ]" "info"

echo ""

# Project structure
echo -e "${CYAN}Project Structure:${NC}"
check "Source directory" "[ -d src ]" "error"
check "Test directory" "[ -d tests ]" "error"
check "Scripts directory" "[ -d scripts ]" "error"
check "Migrations directory" "[ -d migrations ] || mkdir -p migrations" "warning"

echo ""

# Git repository
echo -e "${CYAN}Git Repository:${NC}"
if [ -d .git ]; then
    check "Git repository" "[ -d .git ]" "info"
    check "Git status clean" "[ -z \"$(git status --porcelain)\" ]" "info"
    BRANCH=$(git branch --show-current 2>/dev/null || echo "unknown")
    echo "  Current branch: $BRANCH"
else
    echo -e "${YELLOW}Not a git repository${NC}"
fi

echo ""

# Database checks
echo -e "${CYAN}Database Checks:${NC}"
DB_DIR="$HOME/.deploy-ledger"
if [ -d "$DB_DIR" ]; then
    check "Database directory exists" "[ -d \"$DB_DIR\" ]" "info"
    if [ -f "$DB_DIR/deployments.db" ]; then
        check "Database file exists" "[ -f \"$DB_DIR/deployments.db\" ]" "info"
        SIZE=$(du -h "$DB_DIR/deployments.db" 2>/dev/null | cut -f1)
        echo "  Database size: $SIZE"
    else
        echo -e "${CYAN}Database not initialized${NC}"
    fi
else
    echo -e "${CYAN}Database directory not created yet${NC}"
fi

echo ""

# Guile module paths
echo -e "${CYAN}Guile Configuration:${NC}"
GUILE_VERSION=$(guile --version 2>/dev/null | head -1 | awk '{print $NF}' || echo "unknown")
echo "  Guile version: $GUILE_VERSION"
echo "  Load paths:"
guile -c '(for-each (lambda (p) (display "    ") (display p) (newline)) %load-path)' 2>/dev/null || echo "    Unable to determine"

echo ""

# Summary
echo -e "${CYAN}═══════════════════════════════════════${NC}"
echo -e "${CYAN}Summary:${NC}"

if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✓ All checks passed!${NC}"
    echo "System is ready for build."
    exit 0
elif [ $ERRORS -eq 0 ]; then
    echo -e "${YELLOW}⚠ $WARNINGS warning(s) found${NC}"
    echo "Build can proceed but some features may be limited."
    exit 0
else
    echo -e "${RED}✗ $ERRORS error(s) found${NC}"
    if [ $WARNINGS -gt 0 ]; then
        echo -e "${YELLOW}⚠ $WARNINGS warning(s) found${NC}"
    fi
    echo ""
    echo "Please fix errors before proceeding."
    echo "Run 'make deps-install' to install missing dependencies."
    exit 1
fi