/**
 * GitHub Webhook Handler for Guile Deploy Ledger MCP Server
 * Automatically records deployments from GitHub events
 * Ref: https://docs.github.com/en/webhooks/webhook-events-and-payloads
 */

import crypto from 'crypto';

class GitHubWebhookHandler {
  constructor(config) {
    this.config = config;
    this.secret = process.env.GITHUB_WEBHOOK_SECRET || config.webhookSecret;
  }

  /**
   * Verify GitHub webhook signature
   */
  verifySignature(payload, signature) {
    if (!this.secret) {
      console.warn('No webhook secret configured - accepting all webhooks');
      return true;
    }

    const hmac = crypto.createHmac('sha256', this.secret);
    const digest = 'sha256=' + hmac.update(payload).digest('hex');

    return crypto.timingSafeEqual(
      Buffer.from(signature),
      Buffer.from(digest)
    );
  }

  /**
   * Handle incoming webhook from GitHub
   */
  async handleWebhook(request) {
    const signature = request.headers.get('x-hub-signature-256');
    const event = request.headers.get('x-github-event');
    const delivery = request.headers.get('x-github-delivery');

    const body = await request.text();

    // Verify signature
    if (!this.verifySignature(body, signature)) {
      return new Response('Invalid signature', { status: 401 });
    }

    const payload = JSON.parse(body);

    console.log(`Received GitHub webhook: ${event} (${delivery})`);

    try {
      switch (event) {
        case 'deployment':
          return await this.handleDeployment(payload);

        case 'deployment_status':
          return await this.handleDeploymentStatus(payload);

        case 'workflow_run':
          return await this.handleWorkflowRun(payload);

        case 'release':
          return await this.handleRelease(payload);

        case 'push':
          return await this.handlePush(payload);

        case 'pull_request':
          return await this.handlePullRequest(payload);

        case 'check_run':
          return await this.handleCheckRun(payload);

        case 'workflow_job':
          return await this.handleWorkflowJob(payload);

        default:
          console.log(`Unhandled event type: ${event}`);
          return new Response('Event not handled', { status: 200 });
      }
    } catch (error) {
      console.error('Error handling webhook:', error);
      return new Response('Internal error', { status: 500 });
    }
  }

  /**
   * Handle GitHub deployment event
   * https://docs.github.com/en/webhooks/webhook-events-and-payloads#deployment
   */
  async handleDeployment(payload) {
    const deployment = {
      service: payload.repository.name,
      version: payload.deployment.ref,
      environment: payload.deployment.environment || 'production',
      deployment_type: this.inferDeploymentType(payload.deployment),
      initiator: payload.sender.login,
      metadata: {
        github_deployment_id: payload.deployment.id,
        repository: payload.repository.full_name,
        sha: payload.deployment.sha,
        task: payload.deployment.task,
        description: payload.deployment.description,
        creator: payload.deployment.creator.login,
        created_at: payload.deployment.created_at,
        auto_merge: payload.deployment.auto_merge,
        production_environment: payload.deployment.production_environment,
        transient_environment: payload.deployment.transient_environment,
        original_environment: payload.deployment.original_environment
      }
    };

    await this.recordDeployment(deployment);

    return new Response(JSON.stringify({
      status: 'recorded',
      deployment_id: deployment.metadata.github_deployment_id
    }), {
      status: 200,
      headers: { 'Content-Type': 'application/json' }
    });
  }

  /**
   * Handle GitHub deployment_status event
   * https://docs.github.com/en/webhooks/webhook-events-and-payloads#deployment_status
   */
  async handleDeploymentStatus(payload) {
    const status = payload.deployment_status.state;
    const deploymentId = payload.deployment.id;

    const statusUpdate = {
      deployment_id: `github-${deploymentId}`,
      status: this.mapGitHubStatus(status),
      message: payload.deployment_status.description,
      metadata: {
        github_status_id: payload.deployment_status.id,
        environment: payload.deployment.environment,
        environment_url: payload.deployment_status.environment_url,
        log_url: payload.deployment_status.log_url,
        created_at: payload.deployment_status.created_at
      }
    };

    await this.updateDeploymentStatus(statusUpdate);

    // If deployment failed, potentially trigger rollback
    if (status === 'failure' || status === 'error') {
      await this.checkForAutoRollback(payload);
    }

    return new Response('Status updated', { status: 200 });
  }

  /**
   * Handle GitHub workflow_run event
   * https://docs.github.com/en/webhooks/webhook-events-and-payloads#workflow_run
   */
  async handleWorkflowRun(payload) {
    const workflow = payload.workflow_run;

    // Only track deployment workflows
    if (!this.isDeploymentWorkflow(workflow.name)) {
      return new Response('Not a deployment workflow', { status: 200 });
    }

    if (workflow.conclusion === 'success') {
      const deployment = {
        service: payload.repository.name,
        version: workflow.head_sha.substring(0, 7),
        environment: this.inferEnvironmentFromBranch(workflow.head_branch),
        deployment_type: 'rolling',
        initiator: workflow.actor.login,
        metadata: {
          workflow_id: workflow.id,
          workflow_name: workflow.name,
          workflow_run_number: workflow.run_number,
          branch: workflow.head_branch,
          sha: workflow.head_sha,
          event: workflow.event,
          status: workflow.status,
          conclusion: workflow.conclusion,
          started_at: workflow.run_started_at,
          completed_at: workflow.updated_at,
          url: workflow.html_url
        }
      };

      await this.recordDeployment(deployment);
    }

    return new Response('Workflow processed', { status: 200 });
  }

  /**
   * Handle GitHub release event
   * https://docs.github.com/en/webhooks/webhook-events-and-payloads#release
   */
  async handleRelease(payload) {
    if (payload.action !== 'released' && payload.action !== 'published') {
      return new Response('Not a release event', { status: 200 });
    }

    const deployment = {
      service: payload.repository.name,
      version: payload.release.tag_name,
      environment: payload.release.prerelease ? 'staging' : 'production',
      deployment_type: 'blue-green',
      initiator: payload.release.author.login,
      metadata: {
        release_id: payload.release.id,
        release_name: payload.release.name,
        release_body: payload.release.body,
        draft: payload.release.draft,
        prerelease: payload.release.prerelease,
        created_at: payload.release.created_at,
        published_at: payload.release.published_at,
        tarball_url: payload.release.tarball_url,
        zipball_url: payload.release.zipball_url,
        html_url: payload.release.html_url
      }
    };

    await this.recordDeployment(deployment);

    return new Response('Release recorded', { status: 200 });
  }

  /**
   * Handle GitHub push event (for automatic deployments)
   * https://docs.github.com/en/webhooks/webhook-events-and-payloads#push
   */
  async handlePush(payload) {
    // Only track pushes to deployment branches
    const deployBranches = ['main', 'master', 'production', 'staging'];
    const branch = payload.ref.replace('refs/heads/', '');

    if (!deployBranches.includes(branch)) {
      return new Response('Not a deployment branch', { status: 200 });
    }

    const deployment = {
      service: payload.repository.name,
      version: payload.after.substring(0, 7),
      environment: this.inferEnvironmentFromBranch(branch),
      deployment_type: 'rolling',
      initiator: payload.pusher.name,
      metadata: {
        branch: branch,
        before: payload.before,
        after: payload.after,
        commits: payload.commits.length,
        compare_url: payload.compare,
        forced: payload.forced,
        head_commit: {
          id: payload.head_commit.id,
          message: payload.head_commit.message,
          author: payload.head_commit.author.name,
          timestamp: payload.head_commit.timestamp
        }
      }
    };

    await this.recordDeployment(deployment);

    return new Response('Push deployment recorded', { status: 200 });
  }

  /**
   * Handle GitHub pull_request event
   * https://docs.github.com/en/webhooks/webhook-events-and-payloads#pull_request
   */
  async handlePullRequest(payload) {
    // Track PR deployments (preview environments)
    if (payload.action === 'opened' || payload.action === 'synchronize') {
      const pr = payload.pull_request;

      const deployment = {
        service: payload.repository.name,
        version: `pr-${pr.number}`,
        environment: 'preview',
        deployment_type: 'canary',
        initiator: pr.user.login,
        metadata: {
          pull_request_id: pr.id,
          pull_request_number: pr.number,
          title: pr.title,
          body: pr.body,
          state: pr.state,
          head_sha: pr.head.sha,
          base_branch: pr.base.ref,
          head_branch: pr.head.ref,
          mergeable: pr.mergeable,
          draft: pr.draft,
          html_url: pr.html_url
        }
      };

      await this.recordDeployment(deployment);
    }

    return new Response('PR deployment tracked', { status: 200 });
  }

  /**
   * Handle GitHub check_run event
   * https://docs.github.com/en/webhooks/webhook-events-and-payloads#check_run
   */
  async handleCheckRun(payload) {
    if (payload.check_run.name.includes('deploy') &&
        payload.check_run.conclusion === 'success') {

      const checkRun = payload.check_run;

      const deployment = {
        service: payload.repository.name,
        version: checkRun.head_sha.substring(0, 7),
        environment: 'development',
        deployment_type: 'rolling',
        initiator: checkRun.app.owner.login,
        metadata: {
          check_run_id: checkRun.id,
          check_run_name: checkRun.name,
          check_suite_id: checkRun.check_suite.id,
          status: checkRun.status,
          conclusion: checkRun.conclusion,
          started_at: checkRun.started_at,
          completed_at: checkRun.completed_at,
          output: checkRun.output
        }
      };

      await this.recordDeployment(deployment);
    }

    return new Response('Check run processed', { status: 200 });
  }

  /**
   * Handle GitHub workflow_job event
   * https://docs.github.com/en/webhooks/webhook-events-and-payloads#workflow_job
   */
  async handleWorkflowJob(payload) {
    const job = payload.workflow_job;

    // Track deployment jobs
    if (job.name.toLowerCase().includes('deploy') &&
        job.conclusion === 'success') {

      const deployment = {
        service: payload.repository.name,
        version: job.head_sha.substring(0, 7),
        environment: this.inferEnvironmentFromJobName(job.name),
        deployment_type: 'rolling',
        initiator: payload.sender.login,
        metadata: {
          job_id: job.id,
          job_name: job.name,
          workflow_name: job.workflow_name,
          run_id: job.run_id,
          status: job.status,
          conclusion: job.conclusion,
          started_at: job.started_at,
          completed_at: job.completed_at,
          steps: job.steps.map(step => ({
            name: step.name,
            status: step.status,
            conclusion: step.conclusion,
            started_at: step.started_at,
            completed_at: step.completed_at
          }))
        }
      };

      await this.recordDeployment(deployment);
    }

    return new Response('Workflow job processed', { status: 200 });
  }

  /**
   * Helper methods
   */

  inferDeploymentType(deployment) {
    const task = deployment.task?.toLowerCase() || '';

    if (task.includes('canary')) return 'canary';
    if (task.includes('blue-green')) return 'blue-green';
    if (task.includes('rollback')) return 'rollback';
    if (deployment.transient_environment) return 'canary';

    return 'rolling';
  }

  mapGitHubStatus(status) {
    const statusMap = {
      'pending': 'in-progress',
      'in_progress': 'in-progress',
      'queued': 'pending',
      'success': 'succeeded',
      'failure': 'failed',
      'error': 'failed',
      'inactive': 'pending'
    };

    return statusMap[status] || status;
  }

  isDeploymentWorkflow(name) {
    const deploymentKeywords = ['deploy', 'release', 'ship', 'production'];
    return deploymentKeywords.some(keyword =>
      name.toLowerCase().includes(keyword)
    );
  }

  inferEnvironmentFromBranch(branch) {
    if (branch === 'main' || branch === 'master') return 'production';
    if (branch === 'staging' || branch === 'stage') return 'staging';
    if (branch === 'develop' || branch === 'development') return 'development';
    return 'development';
  }

  inferEnvironmentFromJobName(jobName) {
    const name = jobName.toLowerCase();
    if (name.includes('prod')) return 'production';
    if (name.includes('staging')) return 'staging';
    if (name.includes('dev')) return 'development';
    return 'development';
  }

  async checkForAutoRollback(payload) {
    // Check if auto-rollback is configured
    const autoRollback = process.env.AUTO_ROLLBACK === 'true';

    if (autoRollback && payload.deployment.environment === 'production') {
      console.log('Deployment failed, initiating auto-rollback...');

      // Record rollback event
      const rollback = {
        service: payload.repository.name,
        from_version: payload.deployment.ref,
        to_version: 'previous', // Would need to query for previous version
        reason: `Auto-rollback due to deployment failure: ${payload.deployment_status.description}`,
        deployment_id: `github-${payload.deployment.id}`
      };

      await this.recordRollback(rollback);
    }
  }

  /**
   * Integration with MCP server
   */

  async recordDeployment(deployment) {
    // This would integrate with the MCP server's deployment recording
    console.log('Recording deployment:', deployment);
    // Actual implementation would call the MCP server's record_deployment tool
  }

  async updateDeploymentStatus(update) {
    // This would integrate with the MCP server's status update
    console.log('Updating deployment status:', update);
    // Actual implementation would call the MCP server's update_deployment_status tool
  }

  async recordRollback(rollback) {
    // This would integrate with the MCP server's rollback recording
    console.log('Recording rollback:', rollback);
    // Actual implementation would call the MCP server's record_rollback tool
  }
}

export { GitHubWebhookHandler };