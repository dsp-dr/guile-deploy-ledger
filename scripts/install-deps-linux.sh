#!/usr/bin/env bash
# install-deps-linux.sh - Install dependencies on Linux

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

echo -e "${YELLOW}Installing dependencies for Linux...${NC}"

# Detect distribution
if [ -f /etc/os-release ]; then
    . /etc/os-release
    DISTRO=$ID
    VERSION=$VERSION_ID
else
    echo -e "${RED}Cannot detect Linux distribution${NC}"
    exit 1
fi

echo "Detected: $DISTRO $VERSION"

# Function to check if running with sudo
check_sudo() {
    if [ "$EUID" -ne 0 ] && ! sudo -n true 2>/dev/null; then
        echo -e "${YELLOW}This script requires sudo privileges${NC}"
        echo "Please enter your password:"
        sudo true
    fi
}

# Install based on distribution
case "$DISTRO" in
    ubuntu|debian)
        check_sudo
        echo "Installing packages for Debian/Ubuntu..."
        sudo apt-get update
        sudo apt-get install -y \
            guile-3.0 \
            guile-3.0-dev \
            guile-3.0-libs \
            sqlite3 \
            libsqlite3-dev \
            git \
            make \
            gcc \
            curl \
            inotify-tools \
            pandoc || true

        # Try to install guile-sqlite3
        sudo apt-get install -y guile-sqlite3 2>/dev/null || {
            echo -e "${YELLOW}guile-sqlite3 not available in repos, building from source...${NC}"
            bash scripts/build-guile-sqlite3.sh
        }
        ;;

    fedora|rhel|centos)
        check_sudo
        echo "Installing packages for Fedora/RHEL/CentOS..."
        sudo dnf install -y \
            guile30 \
            guile30-devel \
            sqlite \
            sqlite-devel \
            git \
            make \
            gcc \
            curl \
            inotify-tools \
            pandoc || {
            # Fallback to yum for older versions
            sudo yum install -y \
                guile \
                guile-devel \
                sqlite \
                sqlite-devel \
                git \
                make \
                gcc \
                curl
        }
        ;;

    arch|manjaro)
        check_sudo
        echo "Installing packages for Arch Linux..."
        sudo pacman -Syu --noconfirm
        sudo pacman -S --noconfirm \
            guile \
            sqlite \
            git \
            make \
            gcc \
            curl \
            inotify-tools \
            pandoc

        # Install from AUR if needed
        if ! pacman -Q guile-sqlite3 >/dev/null 2>&1; then
            echo -e "${YELLOW}Installing guile-sqlite3 from AUR...${NC}"
            if command -v yay >/dev/null 2>&1; then
                yay -S --noconfirm guile-sqlite3
            elif command -v paru >/dev/null 2>&1; then
                paru -S --noconfirm guile-sqlite3
            else
                echo -e "${YELLOW}No AUR helper found, please install guile-sqlite3 manually${NC}"
            fi
        fi
        ;;

    opensuse*)
        check_sudo
        echo "Installing packages for openSUSE..."
        sudo zypper install -y \
            guile \
            guile-devel \
            sqlite3 \
            sqlite3-devel \
            git \
            make \
            gcc \
            curl \
            inotify-tools \
            pandoc
        ;;

    alpine)
        check_sudo
        echo "Installing packages for Alpine Linux..."
        sudo apk add --no-cache \
            guile \
            guile-dev \
            sqlite \
            sqlite-dev \
            git \
            make \
            gcc \
            libc-dev \
            curl \
            inotify-tools \
            pandoc
        ;;

    *)
        echo -e "${RED}Unsupported distribution: $DISTRO${NC}"
        echo "Please install the following packages manually:"
        echo "  - guile (3.0 preferred, 2.2 minimum)"
        echo "  - guile-dev/guile-devel"
        echo "  - sqlite3"
        echo "  - git"
        echo "  - make"
        echo "  - gcc"
        exit 1
        ;;
esac

# Install ripgrep if not present
if ! command -v rg >/dev/null 2>&1; then
    echo -e "${YELLOW}Installing ripgrep...${NC}"
    if command -v cargo >/dev/null 2>&1; then
        cargo install ripgrep
    else
        curl -LO https://github.com/BurntSushi/ripgrep/releases/download/13.0.0/ripgrep_13.0.0_amd64.deb
        sudo dpkg -i ripgrep_13.0.0_amd64.deb 2>/dev/null || \
        sudo rpm -i https://github.com/BurntSushi/ripgrep/releases/download/13.0.0/ripgrep-13.0.0-x86_64.rpm 2>/dev/null || \
        echo -e "${YELLOW}Could not install ripgrep automatically${NC}"
        rm -f ripgrep_13.0.0_amd64.deb
    fi
fi

echo -e "${GREEN}✓ Linux dependency installation complete${NC}"