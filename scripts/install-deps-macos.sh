#!/usr/bin/env bash
# install-deps-macos.sh - Install dependencies on macOS

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

echo -e "${YELLOW}Installing dependencies for macOS...${NC}"

# Check for Homebrew
if ! command -v brew >/dev/null 2>&1; then
    echo -e "${YELLOW}Homebrew not found. Installing Homebrew...${NC}"
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    # Add Homebrew to PATH for Apple Silicon Macs
    if [[ $(uname -m) == 'arm64' ]]; then
        echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zprofile
        eval "$(/opt/homebrew/bin/brew shellenv)"
    fi
fi

echo "Using Homebrew at: $(which brew)"

# Update Homebrew
echo "Updating Homebrew..."
brew update

# Install required packages
echo "Installing required packages..."
brew install \
    guile \
    sqlite3 \
    git \
    make \
    gcc \
    curl \
    pandoc \
    ripgrep \
    fswatch || true

# Install guile-sqlite3
if ! guile -c "(use-modules (sqlite3))" 2>/dev/null; then
    echo -e "${YELLOW}Installing guile-sqlite3...${NC}"

    # Try installing from Homebrew formula if available
    brew install guile-sqlite3 2>/dev/null || {
        echo -e "${YELLOW}guile-sqlite3 not in Homebrew, building from source...${NC}"

        # Build from source
        TEMP_DIR=$(mktemp -d)
        cd "$TEMP_DIR"

        # Download and build
        git clone https://github.com/opencog/guile-dbi.git
        cd guile-dbi/guile-dbi
        ./autogen.sh
        ./configure --prefix=/usr/local
        make
        sudo make install

        cd ../guile-dbd-sqlite3
        ./autogen.sh
        ./configure --prefix=/usr/local
        make
        sudo make install

        cd "$OLDPWD"
        rm -rf "$TEMP_DIR"
    }
fi

# Install GNU coreutils for better compatibility
if ! command -v greadlink >/dev/null 2>&1; then
    echo -e "${YELLOW}Installing GNU coreutils for better compatibility...${NC}"
    brew install coreutils
fi

# Set up environment variables
echo ""
echo -e "${CYAN}Setting up environment...${NC}"

# Add to shell profile if not already present
SHELL_PROFILE=""
if [ -n "$ZSH_VERSION" ]; then
    SHELL_PROFILE="$HOME/.zshrc"
elif [ -n "$BASH_VERSION" ]; then
    SHELL_PROFILE="$HOME/.bash_profile"
fi

if [ -n "$SHELL_PROFILE" ] && [ -f "$SHELL_PROFILE" ]; then
    # Add Guile paths
    if ! grep -q "GUILE_LOAD_PATH" "$SHELL_PROFILE"; then
        echo "" >> "$SHELL_PROFILE"
        echo "# Guile Deploy Ledger paths" >> "$SHELL_PROFILE"
        echo "export GUILE_LOAD_PATH=/usr/local/share/guile/site/3.0:\$GUILE_LOAD_PATH" >> "$SHELL_PROFILE"
        echo "export GUILE_LOAD_COMPILED_PATH=/usr/local/lib/guile/3.0/site-ccache:\$GUILE_LOAD_COMPILED_PATH" >> "$SHELL_PROFILE"
        echo -e "${GREEN}✓ Added Guile paths to $SHELL_PROFILE${NC}"
        echo -e "${YELLOW}Please run: source $SHELL_PROFILE${NC}"
    fi
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
check_command make
check_command gcc
check_command rg

# Check Guile version
GUILE_VERSION=$(guile --version | head -1 | awk '{print $NF}')
echo "Guile version: $GUILE_VERSION"

echo ""
echo -e "${GREEN}✓ macOS dependency installation complete${NC}"
echo ""
echo "Note: If you're using Apple Silicon (M1/M2), some packages may need Rosetta 2."
echo "Install it with: softwareupdate --install-rosetta"