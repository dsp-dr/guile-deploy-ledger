;;; main.scm -- Main CLI module for deployment ledger
;;; Copyright (C) 2024 DSP-DR
;;;
;;; This file is part of Guile Deploy Ledger.

(define-module (deploy-ledger cli main)
  #:use-module (deploy-ledger cli commands)
  #:use-module (deploy-ledger cli config)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (run-cli))

(define version-string "1.0.0")

(define (display-help)
  (display "guile-deploy-ledger - Deployment tracking and change control system

USAGE:
    guile-deploy-ledger [OPTIONS] COMMAND [ARGS]

OPTIONS:
    -h, --help       Display this help message
    -v, --version    Display version information
    -d, --database   Database file path (default: ~/.deploy-ledger/deployments.db)
    -c, --config     Configuration file path (default: ~/.deploy-ledger.conf)
    -q, --quiet      Suppress informational output
    -V, --verbose    Enable verbose output

COMMANDS:
    record-deployment    Record a new deployment event
    record-rollback      Record a rollback event
    list-deployments     List deployment events
    list-services        List registered services
    show-metrics         Display deployment metrics
    export               Export data in various formats
    visualize           Generate visualizations
    serve               Start the webhook/API server
    import              Import deployment data
    health              Show service health scores
    analyze             Analyze deployment patterns

For detailed help on a command, use:
    guile-deploy-ledger COMMAND --help

EXAMPLES:
    # Record a deployment
    guile-deploy-ledger record-deployment \\
        --service api-gateway \\
        --version 2.3.0 \\
        --environment production \\
        --type blue-green

    # Show metrics for a service
    guile-deploy-ledger show-metrics \\
        --service user-service \\
        --metric frequency \\
        --period 30

    # Export deployment data as JSON
    guile-deploy-ledger export \\
        --format json \\
        --output deployments.json

    # Generate visualization
    guile-deploy-ledger visualize \\
        --format mermaid \\
        --service payment-service \\
        --output timeline.mmd

Report issues at: https://github.com/dsp-dr/guile-deploy-ledger/issues
"))

(define (display-version)
  (format #t "guile-deploy-ledger version ~a~%" version-string)
  (display "Copyright (C) 2024 DSP-DR
License GPLv3+: GNU GPL version 3 or later
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
"))

(define option-spec
  '((help       (single-char #\h) (value #f))
    (version    (single-char #\v) (value #f))
    (database   (single-char #\d) (value #t))
    (config     (single-char #\c) (value #t))
    (quiet      (single-char #\q) (value #f))
    (verbose    (single-char #\V) (value #f))))

(define (parse-global-options args)
  "Parse global options and return (options . remaining-args)"
  (catch 'quit
    (lambda ()
      (getopt-long args option-spec #:stop-at-first-non-option #t))
    (lambda (key . args)
      (format (current-error-port) "Error parsing options: ~a~%" args)
      (exit 1))))

(define (handle-command command args config)
  "Dispatch to appropriate command handler"
  (case (string->symbol command)
    ((record-deployment)
     (handle-record-deployment args config))
    ((record-rollback)
     (handle-record-rollback args config))
    ((list-deployments)
     (handle-list-deployments args config))
    ((list-services)
     (handle-list-services args config))
    ((show-metrics)
     (handle-show-metrics args config))
    ((export)
     (handle-export args config))
    ((visualize)
     (handle-visualize args config))
    ((serve)
     (handle-serve args config))
    ((import)
     (handle-import args config))
    ((health)
     (handle-health args config))
    ((analyze)
     (handle-analyze args config))
    (else
     (format (current-error-port) "Unknown command: ~a~%" command)
     (display-help)
     (exit 1))))

(define (run-cli args)
  "Main entry point for the CLI"
  (let* ((options (parse-global-options args))
         (opt-args (option-ref options '() '())))

    ;; Handle help and version
    (when (option-ref options 'help #f)
      (display-help)
      (exit 0))

    (when (option-ref options 'version #f)
      (display-version)
      (exit 0))

    ;; Load configuration
    (let* ((config-file (option-ref options 'config
                                    (string-append (getenv "HOME")
                                                  "/.deploy-ledger.conf")))
           (config (load-config config-file))
           (database (option-ref options 'database
                                (or (assoc-ref config 'database)
                                   (string-append (getenv "HOME")
                                                 "/.deploy-ledger/deployments.db")))))

      ;; Update config with command-line options
      (set! config (assoc-set! config 'database database))
      (set! config (assoc-set! config 'quiet
                              (option-ref options 'quiet #f)))
      (set! config (assoc-set! config 'verbose
                              (option-ref options 'verbose #f)))

      ;; Get command and dispatch
      (if (null? opt-args)
          (begin
            (display-help)
            (exit 0))
          (let ((command (car opt-args))
                (command-args (cdr opt-args)))
            (handle-command command command-args config))))))