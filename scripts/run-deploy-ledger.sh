#!/usr/bin/env bash
# run-deploy-ledger.sh - Run the deploy ledger CLI without compilation

# Set the project root
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Run with proper load paths
exec guile -L "$PROJECT_ROOT/src" \
           --no-auto-compile \
           "$PROJECT_ROOT/src/deploy-ledger/cli/main.scm" "$@"