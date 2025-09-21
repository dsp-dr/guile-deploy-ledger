# Cloudflare Deployment Guide

## Overview

Deploy the Guile Deploy Ledger MCP Server to Cloudflare Workers with D1 database for serverless operation.

## Architecture

```
┌─────────────────────┐
│  Claude Desktop     │
│  (MCP Client)       │
└──────┬──────────────┘
       │ HTTPS
       ▼
┌─────────────────────┐
│  Cloudflare Worker  │
│  (MCP Server)       │
├─────────────────────┤
│  • Request routing  │
│  • Tool execution   │
│  • Resource access  │
└──────┬──────────────┘
       │
       ▼
┌─────────────────────┐
│  Cloudflare D1      │
│  (SQLite Edge DB)   │
└─────────────────────┘
```

## Prerequisites

- Cloudflare account with Workers enabled
- Wrangler CLI installed: `npm install -g wrangler`
- Node.js 18+ for local development

## Setup Instructions

### 1. Initialize Cloudflare Worker

```bash
# Create new Worker project
wrangler init guile-deploy-ledger-mcp
cd guile-deploy-ledger-mcp

# Install dependencies
npm install @modelcontextprotocol/sdk
npm install @cloudflare/workers-types --save-dev
```

### 2. Create D1 Database

```bash
# Create database
wrangler d1 create deploy-ledger-db

# Create schema
wrangler d1 execute deploy-ledger-db --file=./schema.sql
```

### 3. Worker Implementation

Create `src/index.js`:

```javascript
import { Server } from '@modelcontextprotocol/sdk/server/index.js';

export default {
  async fetch(request, env, ctx) {
    const url = new URL(request.url);

    // Handle MCP protocol endpoints
    if (url.pathname === '/mcp') {
      return handleMCPRequest(request, env);
    }

    // Health check
    if (url.pathname === '/health') {
      return new Response(JSON.stringify({
        status: 'healthy',
        service: 'deploy-ledger-mcp',
        timestamp: new Date().toISOString()
      }), {
        headers: { 'Content-Type': 'application/json' }
      });
    }

    return new Response('Deploy Ledger MCP Server', { status: 200 });
  }
};

async function handleMCPRequest(request, env) {
  const body = await request.json();

  // Route to appropriate handler based on method
  switch (body.method) {
    case 'tools/list':
      return listTools();
    case 'tools/call':
      return callTool(body.params, env);
    case 'resources/list':
      return listResources();
    case 'resources/read':
      return readResource(body.params, env);
    default:
      return new Response(JSON.stringify({
        error: 'Method not found'
      }), { status: 404 });
  }
}
```

### 4. Database Schema

Create `schema.sql`:

```sql
CREATE TABLE deployments (
  id TEXT PRIMARY KEY,
  service TEXT NOT NULL,
  version TEXT NOT NULL,
  environment TEXT NOT NULL,
  deployment_type TEXT NOT NULL,
  status TEXT DEFAULT 'pending',
  initiator TEXT,
  metadata TEXT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE rollbacks (
  id TEXT PRIMARY KEY,
  deployment_id TEXT,
  service TEXT NOT NULL,
  from_version TEXT NOT NULL,
  to_version TEXT NOT NULL,
  reason TEXT NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (deployment_id) REFERENCES deployments(id)
);

CREATE TABLE metrics (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  service TEXT NOT NULL,
  metric_type TEXT NOT NULL,
  value REAL NOT NULL,
  timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for performance
CREATE INDEX idx_deployments_service ON deployments(service);
CREATE INDEX idx_deployments_environment ON deployments(environment);
CREATE INDEX idx_deployments_created ON deployments(created_at);
CREATE INDEX idx_metrics_service ON metrics(service);
CREATE INDEX idx_metrics_timestamp ON metrics(timestamp);
```

### 5. Wrangler Configuration

Update `wrangler.toml`:

```toml
name = "deploy-ledger-mcp"
main = "src/index.js"
compatibility_date = "2024-01-01"

[[d1_databases]]
binding = "DB"
database_name = "deploy-ledger-db"
database_id = "YOUR_DATABASE_ID"

[vars]
MCP_VERSION = "1.0.0"

# Enable CORS for Claude Desktop
[env.production.cors]
allowed_origins = ["https://claude.ai", "claude://"]
allowed_methods = ["GET", "POST", "OPTIONS"]
allowed_headers = ["Content-Type", "Authorization"]

# Rate limiting
[[env.production.ratelimit]]
zone = "deploy-ledger"
threshold = 100
period = 60
```

### 6. Deploy to Cloudflare

```bash
# Deploy to production
wrangler deploy

# Get deployment URL
wrangler deployments list
```

### 7. Configure Claude Desktop

Add to Claude Desktop settings:

```json
{
  "mcp_servers": {
    "deploy-ledger": {
      "url": "https://deploy-ledger-mcp.YOUR-SUBDOMAIN.workers.dev/mcp",
      "token": "YOUR_AUTH_TOKEN",
      "capabilities": ["tools", "resources", "prompts"]
    }
  }
}
```

## API Endpoints

### Public Endpoints

- `GET /health` - Health check
- `GET /metrics` - Prometheus metrics
- `POST /webhook` - GitHub/GitLab webhooks

### MCP Protocol Endpoints

- `POST /mcp` - Main MCP protocol handler
- WebSocket `/ws` - Real-time updates (optional)

## Security Configuration

### Authentication

```javascript
// Add to Worker
async function authenticate(request) {
  const token = request.headers.get('Authorization');
  if (!token || !await verifyToken(token)) {
    return new Response('Unauthorized', { status: 401 });
  }
  return null; // authenticated
}
```

### Rate Limiting

Cloudflare automatically provides DDoS protection. Additional rate limiting:

```javascript
// Using Cloudflare's rate limiting
export async function onRequest(context) {
  const { request, env } = context;

  // Check rate limit
  const identifier = request.headers.get('CF-Connecting-IP');
  const { success } = await env.RATE_LIMITER.limit({ key: identifier });

  if (!success) {
    return new Response('Rate limit exceeded', { status: 429 });
  }

  return fetch(request);
}
```

## Monitoring & Observability

### Cloudflare Analytics

```javascript
// Log custom metrics
export default {
  async fetch(request, env, ctx) {
    const start = Date.now();

    try {
      const response = await handleRequest(request, env);

      // Log to Analytics Engine
      ctx.waitUntil(
        env.ANALYTICS.writeDataPoint({
          dataset: 'deploy_ledger',
          point: {
            timestamp: Date.now(),
            tags: {
              endpoint: new URL(request.url).pathname,
              status: response.status
            },
            values: {
              duration: Date.now() - start
            }
          }
        })
      );

      return response;
    } catch (error) {
      // Log errors
      ctx.waitUntil(logError(error, env));
      throw error;
    }
  }
};
```

### Alerting

Configure Cloudflare notifications:

1. Go to Cloudflare Dashboard > Notifications
2. Set up alerts for:
   - Worker errors > 1% error rate
   - D1 database query failures
   - Rate limiting triggers

## Cost Optimization

### Cloudflare Workers Pricing

- **Free tier**: 100,000 requests/day
- **Paid tier**: $5/month for 10M requests
- **D1 Database**:
  - Free: 5GB storage, 5M rows read/day
  - Paid: $0.75/GB storage

### Optimization Tips

1. **Cache responses**: Use Cloudflare Cache API
2. **Batch database queries**: Reduce D1 operations
3. **Use Durable Objects**: For stateful operations
4. **Enable compression**: Reduce bandwidth

## Deployment Pipeline

### GitHub Actions

Create `.github/workflows/deploy.yml`:

```yaml
name: Deploy to Cloudflare

on:
  push:
    branches: [main]
  workflow_dispatch:

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: '20'

      - name: Install dependencies
        run: npm ci

      - name: Run tests
        run: npm test

      - name: Deploy to Cloudflare
        uses: cloudflare/wrangler-action@v3
        with:
          apiToken: ${{ secrets.CLOUDFLARE_API_TOKEN }}
          environment: production
```

## Troubleshooting

### Common Issues

1. **CORS errors**: Ensure allowed origins include Claude Desktop
2. **D1 connection failures**: Check database bindings in wrangler.toml
3. **Rate limiting**: Adjust thresholds or upgrade plan
4. **Cold starts**: Use Durable Objects for persistent connections

### Debug Mode

```javascript
// Enable debug logging
const DEBUG = env.DEBUG === 'true';

function log(...args) {
  if (DEBUG) {
    console.log('[Deploy-Ledger]', ...args);
  }
}
```

## Migration from Self-Hosted

### Export existing data:

```bash
# From Guile system
guile -c "(export-to-json \"deployments.json\")"
```

### Import to D1:

```javascript
// Import script
async function importData(env) {
  const data = await fetch('deployments.json').then(r => r.json());

  for (const deployment of data.deployments) {
    await env.DB.prepare(
      'INSERT INTO deployments (id, service, version, environment, deployment_type, status, initiator, metadata, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)'
    ).bind(
      deployment.id,
      deployment.service,
      deployment.version,
      deployment.environment,
      deployment.deployment_type,
      deployment.status,
      deployment.initiator,
      JSON.stringify(deployment.metadata),
      deployment.created_at
    ).run();
  }
}
```

## Performance Benchmarks

Expected performance on Cloudflare Workers:

- **Cold start**: < 50ms
- **Warm requests**: < 10ms
- **Database queries**: < 20ms (D1 at edge)
- **Global latency**: < 100ms (via Cloudflare's 300+ PoPs)

## Support & Resources

- [Cloudflare Workers Docs](https://developers.cloudflare.com/workers/)
- [D1 Database Guide](https://developers.cloudflare.com/d1/)
- [MCP Protocol Spec](https://modelcontextprotocol.io/)
- [Wrangler CLI Reference](https://developers.cloudflare.com/workers/wrangler/)

## License

GPLv3+ (same as Guile Deploy Ledger)