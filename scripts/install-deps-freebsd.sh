#!/usr/bin/env bash
# install-deps-freebsd.sh - Install dependencies on FreeBSD

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

echo -e "${YELLOW}Installing dependencies for FreeBSD...${NC}"

# Check if running as root or with sudo
check_sudo() {
    if [ "$EUID" -ne 0 ]; then
        if ! command -v sudo >/dev/null 2>&1; then
            echo -e "${RED}This script requires root privileges${NC}"
            echo "Please run as root or install sudo"
            exit 1
        fi
        echo -e "${YELLOW}This script requires sudo privileges${NC}"
        sudo true
    fi
}

check_sudo

# Update package repository
echo "Updating package repository..."
sudo pkg update

# Install from packages
echo "Installing packages..."
sudo pkg install -y \
    guile3 \
    sqlite3 \
    git \
    gmake \
    gcc \
    bash \
    curl \
    inotify-tools \
    pandoc \
    ripgrep \
    hs-pandoc || true

# Install guile-sqlite3 binding
if ! pkg info -e guile-sqlite3 >/dev/null 2>&1; then
    echo -e "${YELLOW}Installing guile-sqlite3...${NC}"

    # Try from ports
    if [ -d /usr/ports ]; then
        cd /usr/ports/databases/guile-sqlite3
        sudo make install clean
    else
        # Build from source
        echo -e "${YELLOW}Building guile-sqlite3 from source...${NC}"
        TEMP_DIR=$(mktemp -d)
        cd "$TEMP_DIR"

        # Download and extract
        fetch https://github.com/opencog/guile-dbi/archive/refs/heads/master.tar.gz
        tar xzf master.tar.gz
        cd guile-dbi-master

        # Build guile-dbi
        cd guile-dbi
        ./autogen.sh
        ./configure --prefix=/usr/local
        gmake
        sudo gmake install

        # Build SQLite driver
        cd ../guile-dbd-sqlite3
        ./autogen.sh
        ./configure --prefix=/usr/local
        gmake
        sudo gmake install

        cd "$OLDPWD"
        rm -rf "$TEMP_DIR"
    fi
fi

# Set up environment
echo ""
echo -e "${CYAN}Setting up environment...${NC}"

# Add to shell profile
SHELL_PROFILE="$HOME/.profile"
if [ "$SHELL" = "/bin/csh" ] || [ "$SHELL" = "/bin/tcsh" ]; then
    SHELL_PROFILE="$HOME/.cshrc"
fi

if [ -f "$SHELL_PROFILE" ]; then
    if ! grep -q "GUILE_LOAD_PATH" "$SHELL_PROFILE"; then
        if [ "$SHELL" = "/bin/csh" ] || [ "$SHELL" = "/bin/tcsh" ]; then
            echo "" >> "$SHELL_PROFILE"
            echo "# Guile Deploy Ledger" >> "$SHELL_PROFILE"
            echo "setenv GUILE_LOAD_PATH /usr/local/share/guile/site/3.0:\$GUILE_LOAD_PATH" >> "$SHELL_PROFILE"
            echo "setenv GUILE_LOAD_COMPILED_PATH /usr/local/lib/guile/3.0/site-ccache:\$GUILE_LOAD_COMPILED_PATH" >> "$SHELL_PROFILE"
        else
            echo "" >> "$SHELL_PROFILE"
            echo "# Guile Deploy Ledger" >> "$SHELL_PROFILE"
            echo "export GUILE_LOAD_PATH=/usr/local/share/guile/site/3.0:\$GUILE_LOAD_PATH" >> "$SHELL_PROFILE"
            echo "export GUILE_LOAD_COMPILED_PATH=/usr/local/lib/guile/3.0/site-ccache:\$GUILE_LOAD_COMPILED_PATH" >> "$SHELL_PROFILE"
        fi
        echo -e "${GREEN}✓ Added Guile paths to $SHELL_PROFILE${NC}"
    fi
fi

# Create symlinks for compatibility
if [ ! -f /usr/local/bin/make ]; then
    sudo ln -sf /usr/local/bin/gmake /usr/local/bin/make
fi

# Verify installation
echo ""
echo -e "${CYAN}Verifying installation...${NC}"

check_command() {
    if command -v "$1" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} $1 installed: $(command -v $1)"
    else
        echo -e "${RED}✗${NC} $1 not found"
    fi
}

check_command guile
check_command guild
check_command sqlite3
check_command git
check_command gmake
check_command gcc
check_command bash
check_command rg

# Check Guile modules
echo ""
echo "Checking Guile modules..."
if guile -c "(use-modules (srfi srfi-64))" 2>/dev/null; then
    echo -e "${GREEN}✓${NC} SRFI-64 available"
else
    echo -e "${YELLOW}⚠${NC} SRFI-64 not available"
fi

if guile -c "(use-modules (sqlite3))" 2>/dev/null; then
    echo -e "${GREEN}✓${NC} SQLite3 module available"
else
    echo -e "${YELLOW}⚠${NC} SQLite3 module not available"
fi

echo ""
echo -e "${GREEN}✓ FreeBSD dependency installation complete${NC}"
echo ""
echo "Note: Please reload your shell configuration:"
echo "  source $SHELL_PROFILE"