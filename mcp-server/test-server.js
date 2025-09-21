#!/usr/bin/env node

/**
 * Test script for MCP Server
 */

console.log('Testing MCP Server...\n');

// Test 1: Check if server module loads
console.log('Test 1: Loading server module...');
try {
  const server = require('./server.js');
  console.log('✓ Server module loaded successfully');
} catch (error) {
  console.log('✗ Failed to load server:', error.message);
  process.exit(1);
}

// Test 2: Check MCP specification
console.log('\nTest 2: Checking MCP specification...');
try {
  const spec = require('./deploy-ledger-mcp.json');
  console.log(`✓ MCP spec loaded: ${spec.name} v${spec.version}`);
  console.log(`  Tools: ${spec.tools.length}`);
  console.log(`  Resources: ${spec.resources.length}`);
  console.log(`  Prompts: ${spec.prompts.length}`);
} catch (error) {
  console.log('✗ Failed to load MCP spec:', error.message);
  process.exit(1);
}

// Test 3: Check webhook handler
console.log('\nTest 3: Loading webhook handler...');
try {
  const { GitHubWebhookHandler } = require('./webhook-handler.js');
  const handler = new GitHubWebhookHandler({ webhookSecret: 'test-secret' });
  console.log('✓ Webhook handler loaded successfully');
} catch (error) {
  console.log('✗ Failed to load webhook handler:', error.message);
  process.exit(1);
}

console.log('\n✅ All MCP server tests passed!');
process.exit(0);