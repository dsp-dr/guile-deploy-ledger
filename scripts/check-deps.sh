#!/usr/bin/env bash
# check-deps.sh - Check for required dependencies

# Don't exit on individual command failures - we handle them ourselves
set +e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

echo "Checking system dependencies..."

# Track missing dependencies
MISSING_DEPS=""
MISSING_OPTIONAL=""

# Function to check command
check_command() {
    local cmd=$1
    local required=$2
    local description=$3

    if command -v "$cmd" > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} $description found: $(command -v $cmd)"
        return 0
    else
        if [ "$required" = "required" ]; then
            echo -e "${RED}✗${NC} $description not found (required)"
            MISSING_DEPS="$MISSING_DEPS $cmd"
        else
            echo -e "${YELLOW}⚠${NC} $description not found (optional)"
            MISSING_OPTIONAL="$MISSING_OPTIONAL $cmd"
        fi
        return 1
    fi
}

# Function to check Guile module
check_guile_module() {
    local module=$1
    local required=$2
    local description=$3

    if guile -c "(use-modules ($module)) (exit 0)" 2>/dev/null; then
        echo -e "${GREEN}✓${NC} Guile module '$module' found"
        return 0
    else
        if [ "$required" = "required" ]; then
            echo -e "${RED}✗${NC} Guile module '$module' not found (required)"
            MISSING_DEPS="$MISSING_DEPS guile-$module"
        else
            echo -e "${YELLOW}⚠${NC} Guile module '$module' not found (optional)"
            MISSING_OPTIONAL="$MISSING_OPTIONAL guile-$module"
        fi
        return 1
    fi
}

# Check required tools
echo ""
echo "Checking required tools:"
check_command "guile" "required" "Guile"
check_command "guild" "required" "Guild (Guile compiler)"
check_command "sqlite3" "required" "SQLite3"
check_command "git" "required" "Git"
check_command "bash" "required" "Bash"
check_command "make" "required" "Make"

# Check optional tools
echo ""
echo "Checking optional tools:"
check_command "docker" "optional" "Docker"
check_command "emacs" "optional" "Emacs"
check_command "pandoc" "optional" "Pandoc"
check_command "inotifywait" "optional" "inotify-tools"
check_command "rg" "optional" "Ripgrep"

# Check Guile version
echo ""
echo "Checking Guile version:"
if command -v guile > /dev/null 2>&1; then
    GUILE_VERSION=$(guile --version | head -1 | awk '{print $NF}')
    MAJOR_VERSION=$(echo "$GUILE_VERSION" | cut -d. -f1)
    MINOR_VERSION=$(echo "$GUILE_VERSION" | cut -d. -f2)

    if [ "$MAJOR_VERSION" -ge 2 ]; then
        echo -e "${GREEN}✓${NC} Guile version $GUILE_VERSION is supported"
    else
        echo -e "${RED}✗${NC} Guile version $GUILE_VERSION is too old (need 2.0+)"
        MISSING_DEPS="$MISSING_DEPS guile-upgrade"
    fi
fi

# Check Guile modules
echo ""
echo "Checking Guile modules:"
check_guile_module "srfi srfi-9" "required" "SRFI-9 records"
check_guile_module "srfi srfi-19" "required" "SRFI-19 time"
check_guile_module "srfi srfi-64" "required" "SRFI-64 testing"
check_guile_module "ice-9 match" "required" "Pattern matching"
check_guile_module "ice-9 format" "required" "Formatting"
# SQLite3 is optional for basic functionality
if ! check_guile_module "sqlite3" "optional" "SQLite3 bindings"; then
    echo -e "${CYAN}  Note: Some features will be limited without SQLite3 module${NC}"
fi

# Check filesystem permissions
echo ""
echo "Checking filesystem permissions:"
if [ -w "$HOME" ]; then
    echo -e "${GREEN}✓${NC} Home directory is writable"
else
    echo -e "${RED}✗${NC} Home directory is not writable"
    MISSING_DEPS="$MISSING_DEPS filesystem-perms"
fi

# Check available disk space
echo ""
echo "Checking disk space:"
AVAILABLE_SPACE=$(df -m . | awk 'NR==2 {print $4}')
if [ "$AVAILABLE_SPACE" -ge 100 ]; then
    echo -e "${GREEN}✓${NC} Sufficient disk space (${AVAILABLE_SPACE}MB available)"
else
    echo -e "${YELLOW}⚠${NC} Low disk space (${AVAILABLE_SPACE}MB available)"
fi

# Summary
echo ""
echo "================================"
if [ -z "$MISSING_DEPS" ]; then
    echo -e "${GREEN}All required dependencies are installed!${NC}"
    exit 0
else
    echo -e "${RED}Missing required dependencies:${NC}"
    echo "$MISSING_DEPS"
    echo ""
    echo "To install missing dependencies, run:"
    echo "  make deps-install"
    exit 1
fi