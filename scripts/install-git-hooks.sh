#!/usr/bin/env bash
# install-git-hooks.sh - Install Git hooks for development

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}Installing Git hooks...${NC}"

# Check if in a git repository
if [ ! -d .git ]; then
    echo -e "${RED}Not in a Git repository${NC}"
    exit 1
fi

HOOKS_DIR=".git/hooks"
mkdir -p "$HOOKS_DIR"

# Pre-commit hook
echo -e "${YELLOW}Creating pre-commit hook...${NC}"
cat > "$HOOKS_DIR/pre-commit" << 'EOF'
#!/usr/bin/env bash
# Pre-commit hook for Guile Deploy Ledger

set -e

echo "Running pre-commit checks..."

# Check for syntax errors in Scheme files
echo "Checking Scheme syntax..."
for file in $(git diff --cached --name-only --diff-filter=ACM | grep '\.scm$'); do
    if [ -f "$file" ]; then
        guile -c "(load \"$file\")" 2>/dev/null || {
            echo "Syntax error in $file"
            exit 1
        }
    fi
done

# Check for large files
echo "Checking file sizes..."
for file in $(git diff --cached --name-only); do
    if [ -f "$file" ]; then
        size=$(wc -c < "$file")
        if [ "$size" -gt 1048576 ]; then  # 1MB
            echo "Warning: $file is larger than 1MB ($size bytes)"
            echo "Consider using Git LFS for large files"
        fi
    fi
done

# Check for sensitive data
echo "Checking for sensitive data..."
for file in $(git diff --cached --name-only --diff-filter=ACM); do
    if [ -f "$file" ]; then
        # Check for common sensitive patterns
        if grep -qE "(password|secret|key|token|api_key)\s*=\s*['\"]" "$file"; then
            echo "Potential sensitive data found in $file"
            echo "Please review before committing"
            exit 1
        fi
    fi
done

# Run tests if available
if [ -f "Makefile" ] && grep -q "^test:" Makefile; then
    echo "Running tests..."
    make test > /dev/null 2>&1 || {
        echo "Tests failed. Please fix before committing."
        exit 1
    }
fi

echo "Pre-commit checks passed!"
EOF

chmod +x "$HOOKS_DIR/pre-commit"
echo -e "${GREEN}✓${NC} Pre-commit hook installed"

# Pre-push hook
echo -e "${YELLOW}Creating pre-push hook...${NC}"
cat > "$HOOKS_DIR/pre-push" << 'EOF'
#!/usr/bin/env bash
# Pre-push hook for Guile Deploy Ledger

set -e

echo "Running pre-push checks..."

# Run full test suite
if [ -f "Makefile" ]; then
    echo "Running full test suite..."
    make test || {
        echo "Tests failed. Please fix before pushing."
        exit 1
    }
fi

# Check for uncommitted changes
if [ -n "$(git status --porcelain)" ]; then
    echo "Warning: You have uncommitted changes"
    echo "Consider committing or stashing them"
fi

echo "Pre-push checks passed!"
EOF

chmod +x "$HOOKS_DIR/pre-push"
echo -e "${GREEN}✓${NC} Pre-push hook installed"

# Commit message hook
echo -e "${YELLOW}Creating commit-msg hook...${NC}"
cat > "$HOOKS_DIR/commit-msg" << 'EOF'
#!/usr/bin/env bash
# Commit message hook for conventional commits

COMMIT_MSG_FILE=$1
COMMIT_MSG=$(cat "$COMMIT_MSG_FILE")

# Check for conventional commit format
if ! echo "$COMMIT_MSG" | grep -qE "^(feat|fix|docs|style|refactor|test|chore|perf|ci|build|revert)(\(.+\))?: .+"; then
    echo "Commit message does not follow conventional format:"
    echo "  type(scope): description"
    echo ""
    echo "Types: feat, fix, docs, style, refactor, test, chore, perf, ci, build, revert"
    echo ""
    echo "Example: feat(storage): add support for PostgreSQL backend"
    exit 1
fi

# Check message length
FIRST_LINE=$(echo "$COMMIT_MSG" | head -1)
if [ ${#FIRST_LINE} -gt 72 ]; then
    echo "First line of commit message is too long (${#FIRST_LINE} > 72 characters)"
    exit 1
fi

echo "Commit message format OK"
EOF

chmod +x "$HOOKS_DIR/commit-msg"
echo -e "${GREEN}✓${NC} Commit-msg hook installed"

# Post-merge hook
echo -e "${YELLOW}Creating post-merge hook...${NC}"
cat > "$HOOKS_DIR/post-merge" << 'EOF'
#!/usr/bin/env bash
# Post-merge hook to update dependencies

echo "Running post-merge tasks..."

# Check if Makefile changed
if git diff HEAD@{1} --name-only | grep -q "Makefile"; then
    echo "Makefile changed, running make setup..."
    make setup
fi

# Check if migrations changed
if git diff HEAD@{1} --name-only | grep -q "migrations/"; then
    echo "Migrations changed, running database migrations..."
    make db-migrate
fi

# Check if dependencies changed
if git diff HEAD@{1} --name-only | grep -q "scripts/install-deps"; then
    echo "Dependencies may have changed, running dependency check..."
    make deps-check
fi

echo "Post-merge tasks complete"
EOF

chmod +x "$HOOKS_DIR/post-merge"
echo -e "${GREEN}✓${NC} Post-merge hook installed"

# Create hooks README
cat > "$HOOKS_DIR/README.md" << 'EOF'
# Git Hooks for Guile Deploy Ledger

This directory contains Git hooks that help maintain code quality.

## Installed Hooks

- **pre-commit**: Runs before each commit
  - Checks Scheme syntax
  - Scans for large files
  - Detects sensitive data
  - Runs tests

- **pre-push**: Runs before pushing
  - Runs full test suite
  - Checks for uncommitted changes

- **commit-msg**: Validates commit messages
  - Enforces conventional commit format
  - Checks message length

- **post-merge**: Runs after merge/pull
  - Updates dependencies if needed
  - Runs migrations if changed
  - Checks for Makefile changes

## Bypassing Hooks

If you need to bypass hooks (not recommended):
```bash
git commit --no-verify
git push --no-verify
```

## Customization

You can customize these hooks by editing the files directly.
Changes will be local to your repository.
EOF

echo ""
echo -e "${GREEN}✓ All Git hooks installed successfully!${NC}"
echo ""
echo "Hooks installed:"
echo "  • pre-commit: Syntax and security checks"
echo "  • pre-push: Full test suite"
echo "  • commit-msg: Conventional commit format"
echo "  • post-merge: Auto-update dependencies"
echo ""
echo "To bypass hooks (not recommended): git commit --no-verify"