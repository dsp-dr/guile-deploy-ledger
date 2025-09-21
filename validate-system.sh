#!/usr/bin/env bash

# Simple validation script for Guile Deploy Ledger

echo "=== Guile Deploy Ledger System Validation ==="
echo ""

# Check CLI
echo "1. CLI Check:"
./gdl --version | head -1
echo ""

# Check MCP Server
echo "2. MCP Server Check:"
cd mcp-server && node test-server.js 2>/dev/null && cd ..
echo ""

# Check Scripts
echo "3. Runner Script Check:"
./scripts/run-deploy-ledger.sh health | python3 -m json.tool | head -5
echo ""

# Check Documentation
echo "4. Documentation:"
echo "   - Man page: $(test -f man/gdl.1 && echo '✓' || echo '✗')"
echo "   - README: $(test -f README.org && echo '✓' || echo '✗')"
echo "   - Claude setup: $(test -f CLAUDE_DESKTOP_SETUP.md && echo '✓' || echo '✗')"
echo ""

# Check Version
echo "5. Version Check:"
echo "   - VERSION file: $(cat VERSION)"
echo "   - CLI version: $(./gdl --version | head -1 | awk '{print $2}')"
echo "   - Man page: $(grep "Version" man/gdl.1 | head -1 | awk '{print $3}')"
echo ""

# Check Claude Config
echo "6. Claude Desktop Config:"
if [ -f ~/.config/claude/claude_desktop_config.json ]; then
    echo "   ✓ Config exists at ~/.config/claude/claude_desktop_config.json"
    echo "   MCP Servers configured:"
    cat ~/.config/claude/claude_desktop_config.json | grep '"deploy-ledger"' && echo "   ✓ deploy-ledger server configured"
else
    echo "   ✗ Config not found"
fi
echo ""

echo "=== System Ready for Claude Desktop Integration ==="