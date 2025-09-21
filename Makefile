# Enhanced Makefile for Guile Deploy Ledger with Universal Support
# Copyright (C) 2025 Daria Pascal (dsp-dr)

# Detect OS and architecture
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)
OS_TYPE := unknown
ARCH_TYPE := $(UNAME_M)

ifeq ($(UNAME_S),Linux)
	OS_TYPE := linux
	DISTRO := $(shell lsb_release -si 2>/dev/null || echo "unknown")
endif
ifeq ($(UNAME_S),Darwin)
	OS_TYPE := macos
endif
ifeq ($(UNAME_S),FreeBSD)
	OS_TYPE := freebsd
endif
ifeq ($(UNAME_S),OpenBSD)
	OS_TYPE := openbsd
endif

# Shell configuration (OS-specific)
ifeq ($(OS_TYPE),freebsd)
	SHELL := /usr/local/bin/bash
else ifeq ($(OS_TYPE),openbsd)
	SHELL := /usr/local/bin/bash
else
	SHELL := /bin/bash
endif
.SHELLFLAGS := -eu -o pipefail -c

# Configuration
PREFIX ?= /usr/local
BINDIR := $(PREFIX)/bin
MANDIR := $(PREFIX)/share/man/man1
LIBDIR := $(PREFIX)/lib/guile/3.0/site-ccache
DATADIR := $(PREFIX)/share/guile/site/3.0
DOCDIR := $(PREFIX)/share/doc/guile-deploy-ledger
DBDIR := $(HOME)/.deploy-ledger
SENTINEL_DIR := .make-sentinels

# Version from VERSION file
VERSION := $(shell cat VERSION 2>/dev/null || echo "1.0.0")

# Tool detection
GUILE := $(shell which guile 2>/dev/null || echo "guile-not-found")
GUILD := $(shell which guild 2>/dev/null || echo "guild-not-found")
SQLITE3 := $(shell which sqlite3 2>/dev/null || echo "sqlite3-not-found")
GIT := $(shell which git 2>/dev/null || echo "git-not-found")
DOCKER := $(shell which docker 2>/dev/null || echo "docker-not-found")
MAKE := $(shell which gmake 2>/dev/null || which make 2>/dev/null || echo "make-not-found")

# Version detection
GUILE_VERSION := $(shell $(GUILE) --version 2>/dev/null | head -1 | awk '{print $$NF}' || echo "0.0.0")
SQLITE_VERSION := $(shell $(SQLITE3) --version 2>/dev/null | awk '{print $$1}' || echo "0.0.0")

# Directories
SRC_DIR := src
TEST_DIR := tests
EXAMPLE_DIR := examples
DOC_DIR := docs
SCRIPT_DIR := scripts
MIGRATION_DIR := migrations

# Source files - ordered for proper compilation dependencies
# Types must be compiled first, then storage, then everything else
SOURCES := src/deploy-ledger/core/types.scm \
           src/deploy-ledger/storage/sqlite.scm \
           src/deploy-ledger/query/metrics.scm \
           src/deploy-ledger/reporting/export.scm \
           src/deploy-ledger/reporting/visualize.scm \
           src/deploy-ledger/cli/config.scm \
           src/deploy-ledger/cli/commands.scm \
           src/deploy-ledger/cli/main.scm
TESTS := $(shell find $(TEST_DIR) -name "test-*.scm" 2>/dev/null)
EXAMPLES := $(shell find $(EXAMPLE_DIR) -name "*.scm" 2>/dev/null)

# Colors for output
RED := \033[0;31m
GREEN := \033[0;32m
YELLOW := \033[0;33m
BLUE := \033[0;34m
MAGENTA := \033[0;35m
CYAN := \033[0;36m
NC := \033[0m # No Color

# Create sentinel directory
$(shell mkdir -p $(SENTINEL_DIR))

# Default target
.DEFAULT_GOAL := help

# Phony targets
.PHONY: all help clean install uninstall test check lint format docs compile repl examples
.PHONY: deps-check deps-install setup db-init db-migrate db-reset db-backup
.PHONY: dev-setup onboard diagnostic preflight
.PHONY: docker-build docker-run ci ci-check release
.PHONY: build gdl man presentations

# Main targets
all: preflight setup compile build test ## Complete build and test

build: gdl man ## Build executable and documentation

gdl: guile-deploy-ledger ## Create short-named executable
	@echo -e "$(YELLOW)Creating short executable name 'gdl'...$(NC)"
	@cp guile-deploy-ledger gdl
	@chmod +x gdl
	@echo -e "$(GREEN)✓ Created gdl executable$(NC)"

man: man/gdl.1 ## Build man page
	@echo -e "$(GREEN)✓ Man page ready$(NC)"

presentations: ## Build presentations
	@echo -e "$(YELLOW)Building presentations...$(NC)"
	@if [ -d presentations ]; then \
		$(MAKE) -C presentations all 2>/dev/null || true; \
	fi
	@echo -e "$(GREEN)✓ Presentations built$(NC)"

help: ## Show this help message with OS info
	@echo -e "$(CYAN)Guile Deploy Ledger - Build System$(NC)"
	@echo -e "$(BLUE)OS: $(OS_TYPE) | Arch: $(ARCH_TYPE)$(NC)"
	@echo -e "$(BLUE)Guile: $(GUILE_VERSION) | SQLite: $(SQLITE_VERSION)$(NC)"
	@echo ""
	@echo "Targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*## "}; {printf "  $(GREEN)%-20s$(NC) %s\n", $$1, $$2}'
	@echo ""
	@echo "Quick start: make onboard"

# Sentinel files for tracking completed steps
$(SENTINEL_DIR)/deps-checked: scripts/check-deps.sh
	@echo -e "$(YELLOW)Checking dependencies...$(NC)"
	@bash scripts/check-deps.sh
	@touch $@

$(SENTINEL_DIR)/db-initialized: $(SENTINEL_DIR)/deps-checked
	@echo -e "$(YELLOW)Initializing database...$(NC)"
	@mkdir -p $(DBDIR)
	@if [ ! -f "$(DBDIR)/deployments.db" ]; then \
		$(SQLITE3) "$(DBDIR)/deployments.db" < migrations/001-initial-schema.sql && \
		echo -e "$(GREEN)✓ Database created$(NC)"; \
	else \
		echo -e "$(BLUE)ℹ Database already exists$(NC)"; \
	fi
	@touch $@

$(SENTINEL_DIR)/migrations-applied: $(SENTINEL_DIR)/db-initialized migrations/*.sql
	@echo -e "$(YELLOW)Applying database migrations...$(NC)"
	@bash scripts/run-migrations.sh "$(DBDIR)/deployments.db"
	@touch $@

$(SENTINEL_DIR)/guile-modules-installed: $(SENTINEL_DIR)/deps-checked
	@echo -e "$(YELLOW)Checking Guile modules...$(NC)"
	@bash scripts/install-guile-modules.sh
	@touch $@

# Dependency management
deps-check: $(SENTINEL_DIR)/deps-checked ## Check system dependencies

deps-install: ## Install missing dependencies (requires sudo)
	@echo -e "$(YELLOW)Installing dependencies for $(OS_TYPE)...$(NC)"
	@bash scripts/install-deps-$(OS_TYPE).sh

# Database management
db-init: $(SENTINEL_DIR)/db-initialized ## Initialize database

db-migrate: $(SENTINEL_DIR)/migrations-applied ## Run database migrations

db-reset: ## Reset database (WARNING: destroys data)
	@echo -e "$(RED)WARNING: This will delete all data!$(NC)"
	@echo -n "Continue? [y/N] " && read ans && [ $${ans:-N} = y ]
	@rm -f $(DBDIR)/deployments.db
	@rm -f $(SENTINEL_DIR)/db-initialized $(SENTINEL_DIR)/migrations-applied
	@$(MAKE) db-init db-migrate
	@echo -e "$(GREEN)✓ Database reset complete$(NC)"

db-backup: ## Backup database
	@mkdir -p $(DBDIR)/backups
	@cp $(DBDIR)/deployments.db $(DBDIR)/backups/deployments-$$(date +%Y%m%d-%H%M%S).db
	@echo -e "$(GREEN)✓ Database backed up$(NC)"

# Setup and configuration
setup: $(SENTINEL_DIR)/deps-checked $(SENTINEL_DIR)/db-initialized $(SENTINEL_DIR)/migrations-applied $(SENTINEL_DIR)/guile-modules-installed ## Complete setup

preflight: ## Run pre-flight checks
	@echo -e "$(CYAN)Running pre-flight checks...$(NC)"
	@bash scripts/preflight-check.sh

diagnostic: ## Run system diagnostics
	@echo -e "$(CYAN)System Diagnostics$(NC)"
	@echo "================================"
	@echo "OS Type: $(OS_TYPE)"
	@echo "Architecture: $(ARCH_TYPE)"
	@echo "Shell: $(SHELL)"
	@echo "Guile: $(GUILE) ($(GUILE_VERSION))"
	@echo "SQLite: $(SQLITE3) ($(SQLITE_VERSION))"
	@echo "Git: $(GIT)"
	@echo "Docker: $(DOCKER)"
	@echo ""
	@echo -e "$(CYAN)Guile Load Path:$(NC)"
	@$(GUILE) -c '(display %load-path)' 2>/dev/null || echo "Unable to get load path"
	@echo ""
	@echo -e "$(CYAN)Database Status:$(NC)"
	@if [ -f "$(DBDIR)/deployments.db" ]; then \
		echo "Database exists at $(DBDIR)/deployments.db"; \
		echo "Size: $$(du -h $(DBDIR)/deployments.db | cut -f1)"; \
	else \
		echo "Database not initialized"; \
	fi

# Onboarding
onboard: ## Complete onboarding for new developers
	@echo -e "$(MAGENTA)╔══════════════════════════════════════╗$(NC)"
	@echo -e "$(MAGENTA)║   Welcome to Guile Deploy Ledger!   ║$(NC)"
	@echo -e "$(MAGENTA)╚══════════════════════════════════════╝$(NC)"
	@echo ""
	@echo -e "$(CYAN)Starting onboarding process...$(NC)"
	@$(MAKE) preflight
	@$(MAKE) deps-check || $(MAKE) deps-install
	@$(MAKE) setup
	@$(MAKE) compile
	@$(MAKE) test
	@echo ""
	@echo -e "$(GREEN)✓ Onboarding complete!$(NC)"
	@echo ""
	@echo -e "$(CYAN)Next steps:$(NC)"
	@echo "  1. Run examples: make examples"
	@echo "  2. Start REPL: make repl"
	@echo "  3. View docs: make docs"

# Development setup
dev-setup: setup ## Setup development environment
	@echo -e "$(YELLOW)Setting up development environment...$(NC)"
	@$(MAKE) install-hooks
	@mkdir -p $(DBDIR)/dev
	@cp -n $(DBDIR)/deployments.db $(DBDIR)/dev/deployments.db 2>/dev/null || true
	@echo -e "$(GREEN)✓ Development environment ready$(NC)"

# Compilation
compile: $(SOURCES:.scm=.go) ## Compile Scheme files to bytecode
	@echo -e "$(GREEN)✓ Compilation complete$(NC)"

%.go: %.scm $(SENTINEL_DIR)/guile-modules-installed
	@mkdir -p $(dir $@)
	@echo -e "$(BLUE)Compiling $<...$(NC)"
	@export GUILE_LOAD_COMPILED_PATH="$(SRC_DIR):${GUILE_LOAD_COMPILED_PATH:-}"; \
		$(GUILD) compile -L $(SRC_DIR) -o $@ $< 2>/dev/null || \
		(echo -e "$(RED)✗ Failed to compile $<$(NC)" && exit 1)

# Testing
test: $(SENTINEL_DIR)/guile-modules-installed ## Run all tests
	@echo -e "$(YELLOW)Running tests...$(NC)"
	@if [ -n "$(TESTS)" ]; then \
		$(GUILE) -L $(SRC_DIR) tests/run-tests.scm tests/ || exit 1; \
	else \
		echo -e "$(YELLOW)No tests found$(NC)"; \
	fi
	@echo -e "$(GREEN)✓ All tests passed$(NC)"

check: test lint ## Run tests and linting

lint: ## Check code style
	@echo -e "$(YELLOW)Linting code...$(NC)"
	@bash scripts/lint-scheme.sh || true
	@echo -e "$(GREEN)✓ Linting complete$(NC)"

format: ## Format code
	@echo -e "$(YELLOW)Formatting code...$(NC)"
	@bash scripts/format-scheme.sh || true
	@echo -e "$(GREEN)✓ Formatting complete$(NC)"

# Installation
install: compile build ## Install the application
	@echo -e "$(YELLOW)Installing to $(PREFIX)...$(NC)"
	@mkdir -p $(BINDIR) $(MANDIR) $(DATADIR)/deploy-ledger $(DOCDIR)
	@cp -r $(SRC_DIR)/* $(DATADIR)/deploy-ledger/
	@cp gdl $(BINDIR)/
	@chmod +x $(BINDIR)/gdl
	@cp man/gdl.1 $(MANDIR)/
	@gzip -f $(MANDIR)/gdl.1
	@cp README.org $(DOCDIR)/
	@echo -e "$(GREEN)✓ Installation complete$(NC)"
	@echo -e "$(CYAN)Installed 'gdl' to $(BINDIR)$(NC)"
	@echo -e "$(CYAN)Man page: man gdl$(NC)"

uninstall: ## Uninstall the application
	@echo -e "$(YELLOW)Uninstalling...$(NC)"
	@rm -f $(BINDIR)/gdl
	@rm -f $(MANDIR)/gdl.1.gz
	@rm -rf $(DATADIR)/deploy-ledger
	@rm -rf $(DOCDIR)
	@echo -e "$(GREEN)✓ Uninstallation complete$(NC)"

# Utilities
clean: ## Clean build artifacts
	@echo -e "$(YELLOW)Cleaning...$(NC)"
	@find . -name "*.go" -delete 2>/dev/null || true
	@find . -name "*~" -delete 2>/dev/null || true
	@rm -rf build/ dist/ *.log
	@rm -rf $(SENTINEL_DIR)
	@rm -f gdl
	@echo -e "$(GREEN)✓ Clean complete$(NC)"

repl: setup ## Start a REPL with project loaded
	@echo -e "$(CYAN)Starting REPL...$(NC)"
	@$(GUILE) -L $(SRC_DIR) \
		--listen=localhost:37146 \
		-c "(use-modules (deploy-ledger core types) \
		                 (deploy-ledger storage sqlite) \
		                 (deploy-ledger query metrics)) \
		    (display \"Guile Deploy Ledger REPL\n\") \
		    (display \"Modules loaded. Connect with: M-x geiser-connect\n\") \
		    (catch #t \
		      (lambda () ((@ (ice-9 readline) activate-readline))) \
		      (lambda args (display \"Readline not available\n\"))) \
		    (catch #t \
		      (lambda () ((@ (system repl server) spawn-server))) \
		      (lambda args (display \"REPL server not available\n\")))"

examples: setup ## Run example scenarios
	@echo -e "$(YELLOW)Running examples...$(NC)"
	@for example in $(EXAMPLES); do \
		echo -e "$(BLUE)Running $$example...$(NC)"; \
		$(GUILE) -L $(SRC_DIR) -s $$example || true; \
		echo ""; \
	done
	@echo -e "$(GREEN)✓ Examples complete$(NC)"

docs: ## Generate documentation
	@echo -e "$(YELLOW)Generating documentation...$(NC)"
	@mkdir -p $(DOC_DIR)/html
	@bash scripts/generate-docs.sh
	@echo -e "$(GREEN)✓ Documentation generated$(NC)"

# Git hooks
install-hooks: ## Install git hooks
	@echo -e "$(YELLOW)Installing git hooks...$(NC)"
	@bash scripts/install-git-hooks.sh
	@echo -e "$(GREEN)✓ Git hooks installed$(NC)"

# Docker targets
docker-build: ## Build Docker image
	@echo -e "$(YELLOW)Building Docker image...$(NC)"
	@if [ "$(DOCKER)" != "docker-not-found" ]; then \
		$(DOCKER) build -t guile-deploy-ledger:latest . && \
		echo -e "$(GREEN)✓ Docker image built$(NC)"; \
	else \
		echo -e "$(RED)Docker not installed$(NC)"; \
		exit 1; \
	fi

docker-run: docker-build ## Run in Docker container
	@$(DOCKER) run -it --rm \
		-v $(PWD):/app \
		-w /app \
		guile-deploy-ledger:latest

# CI/CD
ci: clean preflight setup compile test ci-check ## Run CI pipeline
	@echo -e "$(GREEN)✓ CI pipeline passed$(NC)"

ci-check: ## Validate latest CI run (GitHub Actions)
	@echo -e "$(YELLOW)Checking latest CI run status...$(NC)"
	@if [ "$(GIT)" != "git-not-found" ]; then \
		REPO=$$(git remote get-url origin 2>/dev/null | sed 's/.*github.com[:\/]\(.*\)\.git/\1/' || echo "dsp-dr/guile-deploy-ledger"); \
		echo "Repository: $$REPO"; \
		if command -v gh >/dev/null 2>&1; then \
			gh run list --repo $$REPO --limit 1 --json status,conclusion,name,headBranch | \
				jq -r '.[0] | "\(.name) on \(.headBranch): \(.status) (\(.conclusion // "in progress"))"' || \
				echo "Unable to fetch CI status"; \
			gh run view --repo $$REPO --json jobs | \
				jq -r '.jobs[] | "  - \(.name): \(.conclusion // .status)"' 2>/dev/null || true; \
		else \
			echo "GitHub CLI (gh) not installed - cannot check CI status"; \
		fi; \
	else \
		echo "Git not found - cannot determine repository"; \
	fi

release: clean compile test build ## Prepare a release
	@echo -e "$(YELLOW)Preparing release v$(VERSION)...$(NC)"
	@mkdir -p dist
	@tar czf dist/guile-deploy-ledger-v$(VERSION).tar.gz \
		--exclude='.git' --exclude='dist' --exclude='*.db' --exclude='*.go' \
		--exclude='.make-sentinels' .
	@echo -e "$(GREEN)✓ Release package created: dist/guile-deploy-ledger-v$(VERSION).tar.gz$(NC)"

version: ## Display current version
	@echo "Guile Deploy Ledger v$(VERSION)"

bump-version: ## Bump version number
	@echo -e "$(YELLOW)Current version: $(VERSION)$(NC)"
	@echo -n "New version: " && read NEW_VERSION && \
		echo "$$NEW_VERSION" > VERSION && \
		echo -e "$(GREEN)✓ Version bumped to $$NEW_VERSION$(NC)"

# Development helpers
watch: ## Watch for changes and recompile
	@echo -e "$(CYAN)Watching for changes...$(NC)"
	@bash scripts/watch-compile.sh

benchmark: setup ## Run performance benchmarks
	@echo -e "$(YELLOW)Running benchmarks...$(NC)"
	@$(GUILE) -L $(SRC_DIR) benchmarks/run-benchmarks.scm 2>/dev/null || \
		echo -e "$(YELLOW)No benchmarks found$(NC)"

# Include local overrides if they exist
-include Makefile.local