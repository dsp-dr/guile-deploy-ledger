# Makefile for Guile Deploy Ledger
# Copyright (C) 2024 DSP-DR

SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c

# Configuration
PREFIX ?= /usr/local
BINDIR := $(PREFIX)/bin
LIBDIR := $(PREFIX)/lib/guile/3.0/site-ccache
DATADIR := $(PREFIX)/share/guile/site/3.0
DOCDIR := $(PREFIX)/share/doc/guile-deploy-ledger

# Tools
GUILE := guile
GUILD := guild
GMAKE := gmake
EMACS := emacs

# Directories
SRC_DIR := src
TEST_DIR := tests
EXAMPLE_DIR := examples
DOC_DIR := docs
SCRIPT_DIR := scripts

# Source files
SOURCES := $(shell find $(SRC_DIR) -name "*.scm")
TESTS := $(shell find $(TEST_DIR) -name "*.scm")
EXAMPLES := $(shell find $(EXAMPLE_DIR) -name "*.scm")

# Colors for output
RED := \033[0;31m
GREEN := \033[0;32m
YELLOW := \033[0;33m
BLUE := \033[0;34m
NC := \033[0m # No Color

.PHONY: all help clean install uninstall test check lint format docs compile repl examples

all: compile ## Build everything

help: ## Show this help message
	@echo "Guile Deploy Ledger - Make Targets"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  $(BLUE)%-15s$(NC) %s\n", $$1, $$2}'

compile: $(SOURCES:.scm=.go) ## Compile Scheme files to bytecode
	@echo -e "$(GREEN)✓ Compilation complete$(NC)"

%.go: %.scm
	@echo -e "$(BLUE)Compiling $<...$(NC)"
	@$(GUILD) compile -L $(SRC_DIR) -o $@ $<

test: ## Run all tests
	@echo -e "$(YELLOW)Running tests...$(NC)"
	@for test in $(TESTS); do \
		echo -e "$(BLUE)Testing $$test...$(NC)"; \
		$(GUILE) -L $(SRC_DIR) -s $$test || exit 1; \
	done
	@echo -e "$(GREEN)✓ All tests passed$(NC)"

check: test lint ## Run tests and linting

lint: ## Check code style and common issues
	@echo -e "$(YELLOW)Linting code...$(NC)"
	@echo "TODO: Add Guile linting tools"
	@echo -e "$(GREEN)✓ Linting complete$(NC)"

format: ## Format code (if tools available)
	@echo -e "$(YELLOW)Formatting code...$(NC)"
	@echo "TODO: Add Guile formatting tools"
	@echo -e "$(GREEN)✓ Formatting complete$(NC)"

install: compile ## Install the application
	@echo -e "$(YELLOW)Installing...$(NC)"
	@mkdir -p $(BINDIR)
	@mkdir -p $(DATADIR)/deploy-ledger
	@mkdir -p $(DOCDIR)
	@cp -r $(SRC_DIR)/* $(DATADIR)/deploy-ledger/
	@cp $(SCRIPT_DIR)/guile-deploy-ledger $(BINDIR)/
	@chmod +x $(BINDIR)/guile-deploy-ledger
	@cp README.org $(DOCDIR)/
	@echo -e "$(GREEN)✓ Installation complete$(NC)"
	@echo -e "$(BLUE)Installed to: $(PREFIX)$(NC)"

uninstall: ## Uninstall the application
	@echo -e "$(YELLOW)Uninstalling...$(NC)"
	@rm -f $(BINDIR)/guile-deploy-ledger
	@rm -rf $(DATADIR)/deploy-ledger
	@rm -rf $(DOCDIR)
	@echo -e "$(GREEN)✓ Uninstallation complete$(NC)"

clean: ## Clean build artifacts
	@echo -e "$(YELLOW)Cleaning...$(NC)"
	@find . -name "*.go" -delete
	@find . -name "*~" -delete
	@find . -name "*.db" -delete
	@rm -rf build/
	@rm -rf dist/
	@echo -e "$(GREEN)✓ Clean complete$(NC)"

repl: ## Start a REPL with the project loaded
	@echo -e "$(BLUE)Starting REPL...$(NC)"
	@$(GUILE) -L $(SRC_DIR) \
		--listen=localhost:37146 \
		-c "(use-modules (deploy-ledger core types) \
		                 (deploy-ledger storage sqlite) \
		                 (deploy-ledger query metrics)) \
		    (display \"Guile Deploy Ledger REPL\n\") \
		    (display \"Modules loaded. Connect with: M-x geiser-connect\n\") \
		    ((@ (ice-9 readline) activate-readline)) \
		    ((@ (system repl server) spawn-server))"

examples: ## Run example scenarios
	@echo -e "$(YELLOW)Running examples...$(NC)"
	@for example in $(EXAMPLES); do \
		echo -e "$(BLUE)Running $$example...$(NC)"; \
		$(GUILE) -L $(SRC_DIR) -s $$example; \
		echo ""; \
	done
	@echo -e "$(GREEN)✓ Examples complete$(NC)"

docs: ## Generate documentation
	@echo -e "$(YELLOW)Generating documentation...$(NC)"
	@mkdir -p $(DOC_DIR)/html
	@echo "TODO: Add documentation generation"
	@echo -e "$(GREEN)✓ Documentation generated$(NC)"

install-hooks: ## Install git hooks
	@echo -e "$(YELLOW)Installing git hooks...$(NC)"
	@echo '#!/bin/sh' > .git/hooks/pre-commit
	@echo '$(GMAKE) check' >> .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit
	@echo -e "$(GREEN)✓ Git hooks installed$(NC)"

docker-build: ## Build Docker image
	@echo -e "$(YELLOW)Building Docker image...$(NC)"
	@docker build -t guile-deploy-ledger:latest .
	@echo -e "$(GREEN)✓ Docker image built$(NC)"

docker-run: ## Run in Docker container
	@docker run -it --rm \
		-v $(PWD):/app \
		-w /app \
		guile-deploy-ledger:latest

coverage: ## Run tests with coverage
	@echo -e "$(YELLOW)Running coverage analysis...$(NC)"
	@echo "TODO: Add coverage tools for Guile"
	@echo -e "$(GREEN)✓ Coverage complete$(NC)"

release: clean compile test ## Prepare a release
	@echo -e "$(YELLOW)Preparing release...$(NC)"
	@mkdir -p dist
	@tar czf dist/guile-deploy-ledger-$(shell date +%Y%m%d).tar.gz \
		--exclude='.git' \
		--exclude='dist' \
		--exclude='*.db' \
		--exclude='*.go' \
		.
	@echo -e "$(GREEN)✓ Release package created$(NC)"

# Development targets
watch: ## Watch for changes and recompile
	@echo -e "$(BLUE)Watching for changes...$(NC)"
	@while true; do \
		inotifywait -qre modify $(SRC_DIR); \
		$(GMAKE) compile; \
	done

benchmark: ## Run performance benchmarks
	@echo -e "$(YELLOW)Running benchmarks...$(NC)"
	@$(GUILE) -L $(SRC_DIR) benchmarks/run-benchmarks.scm
	@echo -e "$(GREEN)✓ Benchmarks complete$(NC)"

.PHONY: ci
ci: clean compile test lint ## Run CI pipeline
	@echo -e "$(GREEN)✓ CI pipeline passed$(NC)"

# Include local overrides if they exist
-include Makefile.local