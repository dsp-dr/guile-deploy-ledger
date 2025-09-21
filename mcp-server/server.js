#!/usr/bin/env node

/**
 * MCP Server for Guile Deploy Ledger
 * Model Context Protocol server implementation for Claude Desktop
 */

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import {
  CallToolRequestSchema,
  ErrorCode,
  ListResourcesRequestSchema,
  ListToolsRequestSchema,
  ListPromptsRequestSchema,
  McpError,
  ReadResourceRequestSchema,
  GetPromptRequestSchema,
} from '@modelcontextprotocol/sdk/types.js';
import { exec } from 'child_process';
import { promisify } from 'util';
import path from 'path';
import fs from 'fs/promises';
import os from 'os';

const execAsync = promisify(exec);

class DeployLedgerMCPServer {
  constructor() {
    this.server = new Server(
      {
        name: 'deploy-ledger-mcp',
        version: '1.0.0',
      },
      {
        capabilities: {
          resources: {},
          tools: {},
          prompts: {},
        },
      }
    );

    // Configuration
    this.config = {
      guileExecutable: process.env.GUILE_PATH || 'guile',
      projectRoot: process.env.DEPLOY_LEDGER_ROOT || path.join(os.homedir(), 'ghq/github.com/dsp-dr/guile-deploy-ledger'),
      databasePath: process.env.DEPLOY_LEDGER_DB || path.join(os.homedir(), '.deploy-ledger/deployments.db'),
    };

    this.setupHandlers();
  }

  setupHandlers() {
    // List available tools
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        {
          name: 'record_deployment',
          description: 'Record a new deployment event',
          inputSchema: {
            type: 'object',
            properties: {
              service: { type: 'string', description: 'Service name' },
              version: { type: 'string', description: 'Version' },
              environment: { type: 'string', enum: ['development', 'staging', 'production'] },
              deployment_type: { type: 'string', enum: ['blue-green', 'canary', 'rolling', 'big-bang'] },
              initiator: { type: 'string', description: 'Who initiated' },
              metadata: { type: 'object', description: 'Additional metadata' },
            },
            required: ['service', 'version', 'environment', 'deployment_type'],
          },
        },
        {
          name: 'record_rollback',
          description: 'Record a rollback event',
          inputSchema: {
            type: 'object',
            properties: {
              service: { type: 'string' },
              from_version: { type: 'string' },
              to_version: { type: 'string' },
              reason: { type: 'string' },
              deployment_id: { type: 'string' },
            },
            required: ['service', 'from_version', 'to_version', 'reason'],
          },
        },
        {
          name: 'list_deployments',
          description: 'List deployment events',
          inputSchema: {
            type: 'object',
            properties: {
              service: { type: 'string' },
              environment: { type: 'string' },
              status: { type: 'string' },
              limit: { type: 'integer', default: 10 },
            },
          },
        },
        {
          name: 'get_metrics',
          description: 'Get deployment metrics',
          inputSchema: {
            type: 'object',
            properties: {
              service: { type: 'string' },
              metric: { type: 'string', enum: ['frequency', 'duration', 'success_rate', 'mttr'] },
              period_days: { type: 'integer', default: 30 },
            },
            required: ['metric'],
          },
        },
        {
          name: 'check_health',
          description: 'Check deployment health',
          inputSchema: {
            type: 'object',
            properties: {
              service: { type: 'string' },
            },
          },
        },
      ],
    }));

    // Handle tool calls
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;

      try {
        switch (name) {
          case 'record_deployment':
            return await this.recordDeployment(args);
          case 'record_rollback':
            return await this.recordRollback(args);
          case 'list_deployments':
            return await this.listDeployments(args);
          case 'get_metrics':
            return await this.getMetrics(args);
          case 'check_health':
            return await this.checkHealth(args);
          default:
            throw new McpError(ErrorCode.MethodNotFound, `Unknown tool: ${name}`);
        }
      } catch (error) {
        throw new McpError(ErrorCode.InternalError, error.message);
      }
    });

    // List available resources
    this.server.setRequestHandler(ListResourcesRequestSchema, async () => ({
      resources: [
        {
          uri: 'deploy-ledger://deployments',
          name: 'All Deployments',
          description: 'Access to all deployment records',
          mimeType: 'application/json',
        },
        {
          uri: 'deploy-ledger://metrics/current',
          name: 'Current Metrics',
          description: 'Real-time deployment metrics',
          mimeType: 'application/json',
        },
      ],
    }));

    // Read resource content
    this.server.setRequestHandler(ReadResourceRequestSchema, async (request) => {
      const { uri } = request.params;

      if (uri === 'deploy-ledger://deployments') {
        const result = await this.executeGuileCommand('list-deployments');
        return {
          contents: [
            {
              uri,
              mimeType: 'application/json',
              text: result,
            },
          ],
        };
      }

      if (uri === 'deploy-ledger://metrics/current') {
        const result = await this.executeGuileCommand('show-metrics');
        return {
          contents: [
            {
              uri,
              mimeType: 'application/json',
              text: result,
            },
          ],
        };
      }

      throw new McpError(ErrorCode.InvalidRequest, `Unknown resource: ${uri}`);
    });

    // List available prompts
    this.server.setRequestHandler(ListPromptsRequestSchema, async () => ({
      prompts: [
        {
          name: 'deployment_analysis',
          description: 'Analyze recent deployments',
          arguments: [
            {
              name: 'period_days',
              description: 'Days to analyze',
              required: false,
            },
          ],
        },
        {
          name: 'incident_report',
          description: 'Generate incident report',
          arguments: [
            {
              name: 'deployment_id',
              description: 'Failed deployment ID',
              required: true,
            },
          ],
        },
      ],
    }));

    // Get prompt content
    this.server.setRequestHandler(GetPromptRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;

      if (name === 'deployment_analysis') {
        const period = args?.period_days || 30;
        const result = await this.executeGuileCommand(`analyze --period ${period}`);
        return {
          messages: [
            {
              role: 'user',
              content: {
                type: 'text',
                text: `Analyze deployments from the last ${period} days and provide recommendations.`,
              },
            },
            {
              role: 'assistant',
              content: {
                type: 'text',
                text: result,
              },
            },
          ],
        };
      }

      throw new McpError(ErrorCode.InvalidRequest, `Unknown prompt: ${name}`);
    });
  }

  async executeGuileCommand(command, args = {}) {
    const scriptPath = path.join(this.config.projectRoot, 'scripts/run-deploy-ledger.sh');

    // Build command arguments
    const argString = Object.entries(args)
      .map(([key, value]) => `--${key} "${value}"`)
      .join(' ');

    const fullCommand = `${scriptPath} ${command} ${argString}`;

    try {
      const { stdout, stderr } = await execAsync(fullCommand, {
        env: {
          ...process.env,
          GUILE_LOAD_PATH: path.join(this.config.projectRoot, 'src'),
          DEPLOY_LEDGER_DB: this.config.databasePath,
        },
      });

      if (stderr && !stderr.includes('note:')) {
        console.error('Guile stderr:', stderr);
      }

      return stdout;
    } catch (error) {
      console.error('Command execution failed:', error);
      throw new Error(`Failed to execute command: ${error.message}`);
    }
  }

  async recordDeployment(args) {
    const result = await this.executeGuileCommand('record-deployment', {
      service: args.service,
      version: args.version,
      environment: args.environment,
      type: args.deployment_type,
      initiator: args.initiator || 'mcp-client',
    });

    return {
      content: [
        {
          type: 'text',
          text: `Deployment recorded: ${result}`,
        },
      ],
    };
  }

  async recordRollback(args) {
    const result = await this.executeGuileCommand('record-rollback', {
      service: args.service,
      'from-version': args.from_version,
      'to-version': args.to_version,
      reason: args.reason,
      'deployment-id': args.deployment_id || '',
    });

    return {
      content: [
        {
          type: 'text',
          text: `Rollback recorded: ${result}`,
        },
      ],
    };
  }

  async listDeployments(args) {
    const cmdArgs = {};
    if (args.service) cmdArgs.service = args.service;
    if (args.environment) cmdArgs.environment = args.environment;
    if (args.status) cmdArgs.status = args.status;
    if (args.limit) cmdArgs.limit = args.limit;

    const result = await this.executeGuileCommand('list-deployments', cmdArgs);

    return {
      content: [
        {
          type: 'text',
          text: result,
        },
      ],
    };
  }

  async getMetrics(args) {
    const result = await this.executeGuileCommand('show-metrics', {
      service: args.service || 'all',
      metric: args.metric,
      period: args.period_days || 30,
    });

    return {
      content: [
        {
          type: 'text',
          text: result,
        },
      ],
    };
  }

  async checkHealth(args) {
    const result = await this.executeGuileCommand('health', {
      service: args.service || 'all',
    });

    return {
      content: [
        {
          type: 'text',
          text: result,
        },
      ],
    };
  }

  async start() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error('Deploy Ledger MCP Server started');
  }
}

// Start the server
const server = new DeployLedgerMCPServer();
server.start().catch(console.error);