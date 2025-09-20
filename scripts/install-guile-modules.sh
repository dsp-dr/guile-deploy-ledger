#!/usr/bin/env bash
# install-guile-modules.sh - Install required Guile modules

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}Checking and installing Guile modules...${NC}"

# Function to check module availability
check_module() {
    local module=$1
    local package=$2
    local description=$3

    if guile -c "(use-modules ($module)) (exit 0)" 2>/dev/null; then
        echo -e "${GREEN}✓${NC} $description found"
        return 0
    else
        echo -e "${YELLOW}⚠${NC} $description not found"
        return 1
    fi
}

# Check required modules
MISSING_MODULES=0

echo "Checking core modules..."
check_module "srfi srfi-9" "guile" "SRFI-9 (records)" || ((MISSING_MODULES++))
check_module "srfi srfi-19" "guile" "SRFI-19 (time)" || ((MISSING_MODULES++))
check_module "srfi srfi-64" "guile" "SRFI-64 (testing)" || ((MISSING_MODULES++))
check_module "ice-9 match" "guile" "Pattern matching" || ((MISSING_MODULES++))
check_module "ice-9 format" "guile" "Formatting" || ((MISSING_MODULES++))
check_module "ice-9 getopt-long" "guile" "Command-line parsing" || ((MISSING_MODULES++))
check_module "ice-9 readline" "guile" "Readline support" || echo -e "${CYAN}  (optional for REPL)${NC}"

echo ""
echo "Checking external modules..."
if ! check_module "sqlite3" "guile-sqlite3" "SQLite3 bindings"; then
    echo -e "${YELLOW}Attempting to install guile-sqlite3...${NC}"

    # Detect OS and attempt installation
    OS_TYPE=$(uname -s | tr '[:upper:]' '[:lower:]')

    case "$OS_TYPE" in
        linux)
            if command -v apt-get >/dev/null 2>&1; then
                sudo apt-get install -y guile-sqlite3 2>/dev/null || true
            elif command -v dnf >/dev/null 2>&1; then
                sudo dnf install -y guile-sqlite3 2>/dev/null || true
            elif command -v pacman >/dev/null 2>&1; then
                sudo pacman -S --noconfirm guile-sqlite3 2>/dev/null || true
            fi
            ;;
        darwin)
            if command -v brew >/dev/null 2>&1; then
                brew install guile-sqlite3 2>/dev/null || true
            fi
            ;;
        freebsd)
            if command -v pkg >/dev/null 2>&1; then
                sudo pkg install -y guile-sqlite3 2>/dev/null || true
            fi
            ;;
    esac

    # Check again
    if check_module "sqlite3" "guile-sqlite3" "SQLite3 bindings (after install)"; then
        echo -e "${GREEN}✓ Successfully installed guile-sqlite3${NC}"
    else
        echo -e "${YELLOW}Manual installation of guile-sqlite3 required${NC}"
        echo "Please run: make deps-install"
    fi
fi

# Check for optional but useful modules
echo ""
echo "Checking optional modules..."
check_module "web server" "guile" "Web server" || echo -e "${CYAN}  (needed for webhook server)${NC}"
check_module "json" "guile-json" "JSON support" || echo -e "${CYAN}  (needed for JSON export)${NC}"

# Summary
echo ""
echo -e "${CYAN}Module Check Summary:${NC}"

if [ $MISSING_MODULES -eq 0 ]; then
    echo -e "${GREEN}✓ All required modules are available${NC}"
    exit 0
else
    echo -e "${YELLOW}⚠ Some modules are missing${NC}"
    echo "Your Guile installation may be incomplete."
    echo "Consider reinstalling Guile with development packages."
    exit 1
fi