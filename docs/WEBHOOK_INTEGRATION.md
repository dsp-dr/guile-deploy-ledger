# GitHub Webhook Integration Guide

## Overview

The Guile Deploy Ledger MCP Server includes comprehensive GitHub webhook support for automatically tracking deployments from your GitHub repositories.

## Supported GitHub Events

The webhook handler processes the following GitHub events to track deployments:

### Primary Deployment Events

1. **`deployment`** - Direct deployment creation via GitHub API
2. **`deployment_status`** - Updates to deployment status (success/failure)
3. **`release`** - Published releases and pre-releases
4. **`workflow_run`** - GitHub Actions workflow completions

### Secondary Events (Auto-Detection)

5. **`push`** - Pushes to deployment branches (main, staging, production)
6. **`pull_request`** - PR preview environment deployments
7. **`check_run`** - Deployment-related check completions
8. **`workflow_job`** - Individual job completions in workflows

## Setup Instructions

### 1. Configure Webhook Secret

Generate a secure webhook secret:

```bash
# Generate random secret
openssl rand -hex 32

# Or using uuidgen
uuidgen | sha256sum | cut -d' ' -f1
```

Set the secret in your environment:

```bash
export GITHUB_WEBHOOK_SECRET="your-generated-secret"

# Or add to .env file
echo "GITHUB_WEBHOOK_SECRET=your-generated-secret" >> .env
```

### 2. Add Webhook to GitHub Repository

#### Via GitHub UI:

1. Go to your repository Settings → Webhooks
2. Click "Add webhook"
3. Configure:
   - **Payload URL**: `https://your-server.com/webhook/github`
   - **Content type**: `application/json`
   - **Secret**: Your generated secret
   - **SSL verification**: Enable (recommended)
4. Select events:
   - Deployments
   - Deployment statuses
   - Workflow runs
   - Releases
   - Push events
   - Pull requests
   - Check runs
   - Workflow jobs

#### Via GitHub CLI:

```bash
# Create webhook with all deployment-related events
gh api repos/OWNER/REPO/hooks \
  --method POST \
  --field name="web" \
  --field active=true \
  --field events[]="deployment" \
  --field events[]="deployment_status" \
  --field events[]="workflow_run" \
  --field events[]="release" \
  --field events[]="push" \
  --field events[]="pull_request" \
  --field events[]="check_run" \
  --field events[]="workflow_job" \
  --field config[url]="https://your-server.com/webhook/github" \
  --field config[content_type]="json" \
  --field config[secret]="$GITHUB_WEBHOOK_SECRET"

# List configured webhooks
gh api repos/OWNER/REPO/hooks
```

### 3. Configure MCP Server

Update your MCP server configuration:

```javascript
// mcp-server/config.js
export default {
  webhookSecret: process.env.GITHUB_WEBHOOK_SECRET,
  autoRollback: process.env.AUTO_ROLLBACK === 'true',
  deploymentBranches: ['main', 'master', 'production', 'staging'],
  deploymentWorkflows: ['deploy', 'release', 'ship', 'production']
};
```

### 4. Start Server with Webhook Support

```bash
# Using Node.js directly
GITHUB_WEBHOOK_SECRET=your-secret node mcp-server/server.js

# Using PM2
pm2 start mcp-server/server.js \
  --name deploy-ledger-mcp \
  --env GITHUB_WEBHOOK_SECRET=your-secret

# Using systemd
# Create /etc/systemd/system/deploy-ledger-mcp.service
```

## Webhook Payloads

### Deployment Event

When GitHub sends a `deployment` event:

```json
{
  "action": "created",
  "deployment": {
    "id": 1234567,
    "ref": "v2.3.0",
    "sha": "abc123def456",
    "environment": "production",
    "task": "deploy",
    "description": "Deploy v2.3.0 to production",
    "creator": {
      "login": "github-user"
    }
  },
  "repository": {
    "name": "api-service",
    "full_name": "org/api-service"
  }
}
```

This creates a deployment record:

```javascript
{
  service: "api-service",
  version: "v2.3.0",
  environment: "production",
  deployment_type: "rolling",
  initiator: "github-user",
  metadata: {
    github_deployment_id: 1234567,
    repository: "org/api-service",
    sha: "abc123def456"
  }
}
```

### Workflow Run Event

For GitHub Actions deployments:

```json
{
  "action": "completed",
  "workflow_run": {
    "id": 987654,
    "name": "Deploy to Production",
    "head_sha": "def456abc123",
    "head_branch": "main",
    "conclusion": "success",
    "actor": {
      "login": "github-user"
    }
  }
}
```

## Auto-Detection Logic

The webhook handler includes smart detection for deployment events:

### Branch-Based Detection

Deployments are automatically tracked for pushes to:
- `main` / `master` → Production environment
- `staging` / `stage` → Staging environment
- `develop` / `development` → Development environment

### Workflow Name Detection

Workflows with these keywords trigger deployment tracking:
- "deploy"
- "release"
- "ship"
- "production"

### Deployment Type Inference

The handler automatically determines deployment type:
- `canary` - If task contains "canary" or transient environment
- `blue-green` - If task contains "blue-green"
- `rolling` - Default for most deployments

## Auto-Rollback Configuration

Enable automatic rollback on deployment failures:

```bash
# Set environment variable
export AUTO_ROLLBACK=true

# Configure conditions
export ROLLBACK_ENVIRONMENTS="production,staging"
export ROLLBACK_THRESHOLD=3  # Max failures before rollback
```

When a deployment fails:
1. Status updated to "failed"
2. Auto-rollback triggered for configured environments
3. Rollback event recorded with reason
4. Previous stable version deployed

## Security Considerations

### Webhook Signature Verification

All webhooks are verified using HMAC-SHA256:

```javascript
const crypto = require('crypto');

function verifySignature(payload, signature, secret) {
  const hmac = crypto.createHmac('sha256', secret);
  const digest = 'sha256=' + hmac.update(payload).digest('hex');
  return crypto.timingSafeEqual(
    Buffer.from(signature),
    Buffer.from(digest)
  );
}
```

### IP Allowlisting

Optionally restrict to GitHub's webhook IPs:

```bash
# Get GitHub webhook IPs
curl https://api.github.com/meta | jq .hooks
```

Configure firewall or reverse proxy:

```nginx
# nginx.conf
location /webhook/github {
  # GitHub webhook IPs
  allow 192.30.252.0/22;
  allow 185.199.108.0/22;
  allow 140.82.112.0/20;
  deny all;

  proxy_pass http://localhost:8080;
}
```

## Testing Webhooks

### Using GitHub's Redelivery

1. Go to Settings → Webhooks → Recent Deliveries
2. Select a delivery
3. Click "Redeliver"

### Using curl

```bash
# Test deployment event
PAYLOAD='{"action":"created","deployment":{...}}'
SIGNATURE=$(echo -n "$PAYLOAD" | openssl dgst -sha256 -hmac "$GITHUB_WEBHOOK_SECRET" | sed 's/^.* //')

curl -X POST https://your-server.com/webhook/github \
  -H "Content-Type: application/json" \
  -H "X-Hub-Signature-256: sha256=$SIGNATURE" \
  -H "X-GitHub-Event: deployment" \
  -H "X-GitHub-Delivery: test-123" \
  -d "$PAYLOAD"
```

### Using ngrok for Local Testing

```bash
# Expose local server
ngrok http 8080

# Use ngrok URL in GitHub webhook settings
# https://abc123.ngrok.io/webhook/github
```

## Monitoring & Debugging

### Enable Debug Logging

```bash
# Set debug mode
export DEBUG=true
export LOG_LEVEL=debug

# Start server with verbose logging
node mcp-server/server.js --verbose
```

### View Webhook Logs

```bash
# Tail server logs
tail -f /var/log/deploy-ledger/webhook.log

# Filter by event type
grep "X-GitHub-Event: deployment" webhook.log

# Check failed verifications
grep "Invalid signature" webhook.log
```

### Metrics

Track webhook processing metrics:

```javascript
// Prometheus metrics
webhook_requests_total{event="deployment", status="success"} 42
webhook_requests_total{event="deployment", status="failed"} 3
webhook_processing_duration_seconds{event="deployment"} 0.125
webhook_signature_verification_failures_total 2
```

## Troubleshooting

### Common Issues

1. **"Invalid signature" errors**
   - Verify webhook secret matches
   - Check for trailing newlines in secret
   - Ensure proper encoding (no base64)

2. **Events not triggering**
   - Verify webhook is active in GitHub
   - Check event selection
   - Review server logs for errors

3. **Deployments not recorded**
   - Check branch/workflow name patterns
   - Verify database connectivity
   - Review metadata extraction

### Debug Checklist

```bash
# 1. Verify webhook configuration
gh api repos/OWNER/REPO/hooks

# 2. Check recent deliveries
gh api repos/OWNER/REPO/hooks/HOOK_ID/deliveries

# 3. Test webhook endpoint
curl -I https://your-server.com/webhook/github

# 4. Verify secret is set
echo $GITHUB_WEBHOOK_SECRET

# 5. Check server logs
journalctl -u deploy-ledger-mcp -f

# 6. Test database connection
gdl list-deployments --limit 1
```

## Integration Examples

### GitHub Actions Workflow

```yaml
name: Deploy to Production

on:
  push:
    branches: [main]
  workflow_dispatch:

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Create GitHub Deployment
        id: deployment
        uses: octokit/request-action@v2.x
        with:
          route: POST /repos/{owner}/{repo}/deployments
          owner: ${{ github.repository_owner }}
          repo: ${{ github.event.repository.name }}
          ref: ${{ github.sha }}
          environment: production
          auto_merge: false
          required_contexts: []

      - name: Deploy Application
        run: |
          # Your deployment script
          ./scripts/deploy.sh

      - name: Update Deployment Status
        if: always()
        uses: octokit/request-action@v2.x
        with:
          route: POST /repos/{owner}/{repo}/deployments/{deployment_id}/statuses
          owner: ${{ github.repository_owner }}
          repo: ${{ github.event.repository.name }}
          deployment_id: ${{ fromJson(steps.deployment.outputs.data).id }}
          state: ${{ job.status }}
          environment_url: https://app.example.com
          log_url: ${{ github.server_url }}/${{ github.repository }}/actions/runs/${{ github.run_id }}
```

### Direct API Integration

```bash
# Record deployment via webhook
gh api repos/OWNER/REPO/deployments \
  --method POST \
  --field ref="v2.3.0" \
  --field environment="production" \
  --field description="Deploy version 2.3.0"

# Update deployment status
gh api repos/OWNER/REPO/deployments/DEPLOYMENT_ID/statuses \
  --method POST \
  --field state="success" \
  --field description="Deployment completed"
```

## Best Practices

1. **Always use webhook secrets** - Never accept unsigned webhooks
2. **Log all webhook events** - Maintain audit trail
3. **Implement retry logic** - Handle transient failures
4. **Monitor webhook health** - Alert on repeated failures
5. **Version your deployments** - Use semantic versioning
6. **Tag deployments** - Include change IDs and approvers
7. **Test webhook changes** - Use staging environment first
8. **Document deployment types** - Standardize across teams

## Additional Resources

- [GitHub Webhooks Documentation](https://docs.github.com/en/webhooks)
- [GitHub Deployments API](https://docs.github.com/en/rest/deployments)
- [GitHub Actions Events](https://docs.github.com/en/actions/using-workflows/events-that-trigger-workflows)
- [Webhook Security Best Practices](https://docs.github.com/en/webhooks/using-webhooks/best-practices-for-using-webhooks)