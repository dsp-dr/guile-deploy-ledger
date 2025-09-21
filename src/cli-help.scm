#!/usr/bin/env guile
!#

(use-modules (ice-9 format)
             (ice-9 getopt-long)
             (srfi srfi-1))

(define version "1.0.0")

(define (display-version)
  (format #t "guile-deploy-ledger ~a~%" version)
  (format #t "Event-sourced deployment tracking system~%")
  (format #t "Copyright (C) 2024 DSP-DR~%")
  (format #t "License GPLv3+: GNU GPL version 3 or later~%"))

(define (display-help)
  (format #t "Usage: guile-deploy-ledger [OPTION]... COMMAND [ARGS]...~%~%")

  (format #t "Event-sourced deployment tracking and change control system~%~%")

  (format #t "COMMANDS:~%")
  (format #t "  record-deployment    Record a new deployment event~%")
  (format #t "  record-rollback      Record a rollback event~%")
  (format #t "  list-deployments     List deployment events with filters~%")
  (format #t "  get-deployment       Get details of specific deployment~%")
  (format #t "  show-metrics         Display deployment metrics~%")
  (format #t "  analyze             Analyze deployment patterns~%")
  (format #t "  health              Check deployment health~%")
  (format #t "  export              Export deployment data~%")
  (format #t "  visualize           Generate deployment visualizations~%~%")

  (format #t "GLOBAL OPTIONS:~%")
  (format #t "  -h, --help                Show this help message~%")
  (format #t "  -v, --version             Show version information~%")
  (format #t "  -d, --database PATH       Path to SQLite database~%")
  (format #t "                           (default: ~/.deploy-ledger/deployments.db)~%")
  (format #t "  -f, --format FORMAT       Output format: json, csv, org, markdown~%")
  (format #t "                           (default: json)~%")
  (format #t "  -q, --quiet              Suppress non-essential output~%")
  (format #t "  --verbose                Enable verbose output~%")
  (format #t "  --config FILE            Load configuration from file~%~%")

  (format #t "COMMAND-SPECIFIC OPTIONS:~%~%")

  (format #t "record-deployment:~%")
  (format #t "  --service NAME           Service name (required)~%")
  (format #t "  --version VERSION        Version being deployed (required)~%")
  (format #t "  --environment ENV        Target environment (required)~%")
  (format #t "                          Values: development, staging, production~%")
  (format #t "  --type TYPE             Deployment type (required)~%")
  (format #t "                          Values: blue-green, canary, rolling, big-bang~%")
  (format #t "  --initiator NAME        Who/what initiated deployment~%")
  (format #t "  --change-id ID          Change request ID~%")
  (format #t "  --approved-by LIST      Comma-separated list of approvers~%")
  (format #t "  --region REGION         Deployment region~%")
  (format #t "  --cluster CLUSTER       Target cluster~%")
  (format #t "  --replicas COUNT        Number of replicas~%~%")

  (format #t "record-rollback:~%")
  (format #t "  --service NAME          Service name (required)~%")
  (format #t "  --from-version VER      Version rolling back from (required)~%")
  (format #t "  --to-version VER        Version rolling back to (required)~%")
  (format #t "  --reason REASON         Reason for rollback (required)~%")
  (format #t "  --deployment-id ID      ID of deployment being rolled back~%~%")

  (format #t "list-deployments:~%")
  (format #t "  --service NAME          Filter by service name~%")
  (format #t "  --environment ENV       Filter by environment~%")
  (format #t "  --status STATUS         Filter by status~%")
  (format #t "                         Values: pending, in-progress, succeeded, failed, rolled-back~%")
  (format #t "  --limit NUMBER          Maximum results (default: 10)~%")
  (format #t "  --since DATE           Show deployments since DATE (ISO 8601)~%")
  (format #t "  --until DATE           Show deployments until DATE (ISO 8601)~%~%")

  (format #t "show-metrics:~%")
  (format #t "  --service NAME          Service name (or 'all')~%")
  (format #t "  --metric TYPE          Metric type (required)~%")
  (format #t "                         Values: frequency, duration, success_rate, mttr, lead_time~%")
  (format #t "  --period DAYS          Analysis period in days (default: 30)~%")
  (format #t "  --group-by FIELD       Group results by: day, week, month~%~%")

  (format #t "analyze:~%")
  (format #t "  --type ANALYSIS        Analysis type (required)~%")
  (format #t "                        Values: failure_patterns, peak_times, dependencies, risk~%")
  (format #t "  --period DAYS         Analysis period (default: 90)~%")
  (format #t "  --threshold VALUE     Custom threshold for analysis~%~%")

  (format #t "export:~%")
  (format #t "  --format FORMAT        Export format (required)~%")
  (format #t "                        Values: json, csv, markdown, org-mode~%")
  (format #t "  --output FILE         Output file (default: stdout)~%")
  (format #t "  --service NAME        Filter by service~%")
  (format #t "  --date-from DATE      Start date for export~%")
  (format #t "  --date-to DATE        End date for export~%~%")

  (format #t "visualize:~%")
  (format #t "  --type VISUAL         Visualization type (required)~%")
  (format #t "                       Values: timeline, dependency_graph, heatmap, flow~%")
  (format #t "  --format FORMAT      Output format (required)~%")
  (format #t "                       Values: mermaid, graphviz, ascii~%")
  (format #t "  --service NAME       Focus on specific service~%")
  (format #t "  --width WIDTH        Output width (for ASCII)~%")
  (format #t "  --height HEIGHT      Output height (for ASCII)~%~%")

  (format #t "EXAMPLES:~%")
  (format #t "  # Record a deployment~%")
  (format #t "  guile-deploy-ledger record-deployment \\~%")
  (format #t "    --service api-gateway --version v2.3.0 \\~%")
  (format #t "    --environment production --type canary~%~%")

  (format #t "  # List recent deployments~%")
  (format #t "  guile-deploy-ledger list-deployments \\~%")
  (format #t "    --environment production --limit 20~%~%")

  (format #t "  # Show metrics for a service~%")
  (format #t "  guile-deploy-ledger show-metrics \\~%")
  (format #t "    --service api-gateway --metric success_rate --period 7~%~%")

  (format #t "  # Record a rollback~%")
  (format #t "  guile-deploy-ledger record-rollback \\~%")
  (format #t "    --service api-gateway --from-version v2.3.0 \\~%")
  (format #t "    --to-version v2.2.9 --reason \"Memory leak detected\"~%~%")

  (format #t "  # Analyze failure patterns~%")
  (format #t "  guile-deploy-ledger analyze \\~%")
  (format #t "    --type failure_patterns --period 30~%~%")

  (format #t "  # Export deployment data as CSV~%")
  (format #t "  guile-deploy-ledger export \\~%")
  (format #t "    --format csv --output deployments.csv~%~%")

  (format #t "  # Generate dependency visualization~%")
  (format #t "  guile-deploy-ledger visualize \\~%")
  (format #t "    --type dependency_graph --format mermaid~%~%")

  (format #t "ENVIRONMENT VARIABLES:~%")
  (format #t "  DEPLOY_LEDGER_DB         Database path~%")
  (format #t "  DEPLOY_LEDGER_CONFIG     Configuration file path~%")
  (format #t "  DEPLOY_LEDGER_FORMAT     Default output format~%")
  (format #t "  DEPLOY_LEDGER_WEBHOOK    Webhook URL for notifications~%")
  (format #t "  DEPLOY_LEDGER_API_KEY    API key for external integrations~%~%")

  (format #t "CONFIGURATION FILE:~%")
  (format #t "  Configuration can be provided in Scheme format:~%")
  (format #t "    (database . \"/path/to/db\")~%")
  (format #t "    (default-format . \"json\")~%")
  (format #t "    (webhook-url . \"https://...\")~%")
  (format #t "    (retention-days . 90)~%~%")

  (format #t "EXIT STATUS:~%")
  (format #t "  0  Success~%")
  (format #t "  1  General error~%")
  (format #t "  2  Invalid arguments~%")
  (format #t "  3  Database error~%")
  (format #t "  4  Network error~%")
  (format #t "  5  Permission denied~%~%")

  (format #t "REPORTING BUGS:~%")
  (format #t "  Report bugs to: https://github.com/dsp-dr/guile-deploy-ledger/issues~%")
  (format #t "  Documentation: https://github.com/dsp-dr/guile-deploy-ledger~%~%"))

(define option-spec
  '((help      (single-char #\h) (value #f))
    (version   (single-char #\v) (value #f))
    (database  (single-char #\d) (value #t))
    (format    (single-char #\f) (value #t))
    (quiet     (single-char #\q) (value #f))
    (verbose   (value #f))
    (config    (value #t))))

(define (main args)
  (let* ((options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f)))

    (cond
     (version-wanted
      (display-version))
     (help-wanted
      (display-help))
     (else
      (display-help)))

    0))

(exit (main (command-line)))