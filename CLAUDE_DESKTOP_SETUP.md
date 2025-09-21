# Claude Desktop MCP Integration Setup

## Installation Steps

### 1. Install Claude Desktop

Download and install Claude Desktop from: https://claude.ai/download

### 2. Configure MCP Server

Add the following to your Claude Desktop configuration file:

**macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
**Linux**: `~/.config/Claude/claude_desktop_config.json`
**Windows**: `%APPDATA%\Claude\claude_desktop_config.json`

```json
{
  "mcpServers": {
    "deploy-ledger": {
      "command": "node",
      "args": [
        "/home/dsp-dr/ghq/github.com/dsp-dr/guile-deploy-ledger/mcp-server/server.js"
      ],
      "env": {
        "DEPLOY_LEDGER_ROOT": "/home/dsp-dr/ghq/github.com/dsp-dr/guile-deploy-ledger",
        "DEPLOY_LEDGER_DB": "/home/dsp-dr/.deploy-ledger/deployments.db",
        "GUILE_PATH": "guile"
      }
    }
  }
}
```

**Note**: Adjust the paths to match your system.

### 3. Install Node.js Dependencies

```bash
cd mcp-server
npm install
```

### 4. Verify Installation

Restart Claude Desktop and look for the MCP indicator in the interface.

## Available MCP Tools

Once configured, Claude will have access to these deployment tools:

### Deployment Management
- `record_deployment` - Track new deployments
- `record_rollback` - Record rollback events
- `list_deployments` - Query deployment history
- `get_deployment` - Get specific deployment details
- `update_deployment_status` - Update deployment progress

### Metrics & Analysis
- `get_metrics` - Analyze deployment metrics
- `analyze_patterns` - Detect failure patterns
- `check_health` - Monitor service health

### Data Export
- `export_data` - Export in multiple formats
- `generate_visualization` - Create visual representations

## Usage Examples

In Claude Desktop, you can ask:

- "Show me the recent deployments"
- "What's the success rate for api-gateway?"
- "Record a deployment of version 2.0.0 to production"
- "Generate a deployment timeline visualization"
- "Check the health of all services"

## Testing the Integration

### 1. Test MCP Server Directly

```bash
cd mcp-server
node test-server.js
```

### 2. Test Runner Script

```bash
./scripts/run-deploy-ledger.sh list-deployments
./scripts/run-deploy-ledger.sh show-metrics
./scripts/run-deploy-ledger.sh health
```

### 3. Run Full Test Suite

```bash
./test-all-systems.sh
```

## GitHub Webhook Integration

To enable automatic deployment tracking from GitHub:

### 1. Set Webhook Secret

```bash
export GITHUB_WEBHOOK_SECRET="your-secret-here"
```

### 2. Add Webhook to Repository

```bash
gh api repos/OWNER/REPO/hooks \
  --method POST \
  --field name="web" \
  --field active=true \
  --field events[]="deployment" \
  --field events[]="deployment_status" \
  --field config[url]="https://your-server.com/webhook/github" \
  --field config[content_type]="json" \
  --field config[secret]="$GITHUB_WEBHOOK_SECRET"
```

## Troubleshooting

### MCP Server Not Connecting

1. Check Node.js is installed: `node --version`
2. Verify paths in config are absolute
3. Check server logs: `tail -f ~/.claude/logs/mcp.log`

### Commands Not Working

1. Test runner script manually
2. Check Guile installation: `guile --version`
3. Verify database exists: `ls -la ~/.deploy-ledger/`

### Version Mismatch

Ensure VERSION file matches:
```bash
cat VERSION
./gdl --version
```

## Development Mode

For development, run the MCP server manually:

```bash
cd mcp-server
DEPLOY_LEDGER_ROOT=$(pwd)/.. \
DEPLOY_LEDGER_DB=~/.deploy-ledger/deployments.db \
node server.js
```

## Support

- GitHub Issues: https://github.com/dsp-dr/guile-deploy-ledger/issues
- Documentation: https://github.com/dsp-dr/guile-deploy-ledger