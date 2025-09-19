;;; commands.scm -- CLI command implementations
;;; Copyright (C) 2024 DSP-DR
;;;
;;; This file is part of Guile Deploy Ledger.

(define-module (deploy-ledger cli commands)
  #:use-module (deploy-ledger core types)
  #:use-module (deploy-ledger storage sqlite)
  #:use-module (deploy-ledger query metrics)
  #:use-module (deploy-ledger reporting export)
  #:use-module (deploy-ledger reporting visualize)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19)
  #:export (handle-record-deployment
            handle-record-rollback
            handle-list-deployments
            handle-list-services
            handle-show-metrics
            handle-export
            handle-visualize
            handle-serve
            handle-import
            handle-health
            handle-analyze))

;;; Command option specifications

(define record-deployment-options
  '((help         (single-char #\h) (value #f))
    (service      (single-char #\s) (value #t) (required? #t))
    (version      (single-char #\v) (value #t) (required? #t))
    (environment  (single-char #\e) (value #t) (required? #t))
    (type         (single-char #\t) (value #t))
    (started      (value #t))
    (completed    (value #t))
    (status       (value #t))
    (initiator    (value #t))
    (metadata     (value #t))
    (parent-id    (value #t))))

(define record-rollback-options
  '((help         (single-char #\h) (value #f))
    (service      (single-char #\s) (value #t) (required? #t))
    (from-version (value #t) (required? #t))
    (to-version   (value #t) (required? #t))
    (reason       (single-char #\r) (value #t) (required? #t))
    (timestamp    (value #t))
    (initiator    (value #t))
    (impact       (value #t))
    (deployment-id (value #t))))

(define list-deployments-options
  '((help         (single-char #\h) (value #f))
    (service      (single-char #\s) (value #t))
    (environment  (single-char #\e) (value #t))
    (status       (value #t))
    (from         (value #t))
    (to           (value #t))
    (limit        (single-char #\l) (value #t))
    (format       (single-char #\f) (value #t))))

(define show-metrics-options
  '((help         (single-char #\h) (value #f))
    (service      (single-char #\s) (value #t))
    (metric       (single-char #\m) (value #t))
    (period       (single-char #\p) (value #t))
    (format       (single-char #\f) (value #t))
    (group-by     (value #t))))

;;; Utility functions

(define (parse-time-string str)
  "Parse ISO 8601 time string to SRFI-19 time"
  (if str
      (string->date str "~Y-~m-~dT~H:~M:~S~z")
      (current-time time-utc)))

(define (ensure-database-dir database-path)
  "Ensure the database directory exists"
  (let ((dir (dirname database-path)))
    (unless (file-exists? dir)
      (mkdir-p dir))))

(define (mkdir-p path)
  "Create directory and parents if needed"
  (let loop ((path path))
    (let ((parent (dirname path)))
      (unless (or (string=? parent path)
                  (string=? parent "/")
                  (file-exists? parent))
        (loop parent))
      (unless (file-exists? path)
        (mkdir path)))))

;;; Command handlers

(define (handle-record-deployment args config)
  "Handle the record-deployment command"
  (let* ((options (getopt-long (cons "record-deployment" args)
                               record-deployment-options))
         (db-path (assoc-ref config 'database)))

    (when (option-ref options 'help #f)
      (display "Usage: guile-deploy-ledger record-deployment [OPTIONS]
Record a new deployment event.

Options:
  -h, --help         Show this help message
  -s, --service      Service name (required)
  -v, --version      Service version (required)
  -e, --environment  Deployment environment (required)
  -t, --type         Deployment type (blue-green|canary|rolling|big-bang)
  --started          Start time (ISO 8601)
  --completed        Completion time (ISO 8601)
  --status           Status (pending|in-progress|success|failure)
  --initiator        Who initiated the deployment
  --metadata         Additional metadata (key=value,key=value)
  --parent-id        Parent deployment ID
")
      (exit 0))

    (ensure-database-dir db-path)
    (let* ((db (open-deployment-db db-path))
           (deployment (make-deployment-event
                       #:service-name (option-ref options 'service #f)
                       #:version (option-ref options 'version #f)
                       #:environment (option-ref options 'environment #f)
                       #:deployment-type (string->symbol
                                         (option-ref options 'type "rolling"))
                       #:started (parse-time-string
                                 (option-ref options 'started #f))
                       #:completed (parse-time-string
                                   (option-ref options 'completed #f))
                       #:status (string->symbol
                                (option-ref options 'status "pending"))
                       #:initiator (option-ref options 'initiator (getenv "USER"))
                       #:parent-id (option-ref options 'parent-id #f))))
      (let ((id (store-deployment! db deployment)))
        (unless (assoc-ref config 'quiet)
          (format #t "Deployment recorded: ~a~%" id)))
      (close-deployment-db db))))

(define (handle-record-rollback args config)
  "Handle the record-rollback command"
  (let* ((options (getopt-long (cons "record-rollback" args)
                               record-rollback-options))
         (db-path (assoc-ref config 'database)))

    (when (option-ref options 'help #f)
      (display "Usage: guile-deploy-ledger record-rollback [OPTIONS]
Record a rollback event.

Options:
  -h, --help         Show this help message
  -s, --service      Service name (required)
  --from-version     Version rolling back from (required)
  --to-version       Version rolling back to (required)
  -r, --reason       Reason for rollback (required)
  --timestamp        Rollback time (ISO 8601)
  --initiator        Who initiated the rollback
  --impact           Services impacted (comma-separated)
  --deployment-id    Related deployment ID
")
      (exit 0))

    (ensure-database-dir db-path)
    (let* ((db (open-deployment-db db-path))
           (rollback (make-rollback-event
                     #:service-name (option-ref options 'service #f)
                     #:from-version (option-ref options 'from-version #f)
                     #:to-version (option-ref options 'to-version #f)
                     #:reason (option-ref options 'reason #f)
                     #:timestamp (parse-time-string
                                 (option-ref options 'timestamp #f))
                     #:initiator (option-ref options 'initiator (getenv "USER"))
                     #:deployment-id (option-ref options 'deployment-id #f))))
      (let ((id (store-rollback! db rollback)))
        (unless (assoc-ref config 'quiet)
          (format #t "Rollback recorded: ~a~%" id)))
      (close-deployment-db db))))

(define (handle-list-deployments args config)
  "Handle the list-deployments command"
  (let* ((options (getopt-long (cons "list-deployments" args)
                               list-deployments-options))
         (db-path (assoc-ref config 'database)))

    (when (option-ref options 'help #f)
      (display "Usage: guile-deploy-ledger list-deployments [OPTIONS]
List deployment events.

Options:
  -h, --help         Show this help message
  -s, --service      Filter by service name
  -e, --environment  Filter by environment
  --status           Filter by status
  --from             From date (YYYY-MM-DD)
  --to               To date (YYYY-MM-DD)
  -l, --limit        Limit number of results
  -f, --format       Output format (text|json|csv)
")
      (exit 0))

    (ensure-database-dir db-path)
    (let* ((db (open-deployment-db db-path))
           (deployments (list-deployments db
                         #:service-name (option-ref options 'service #f)
                         #:environment (option-ref options 'environment #f)
                         #:status (and=> (option-ref options 'status #f)
                                        string->symbol)
                         #:limit (and=> (option-ref options 'limit #f)
                                       string->number)))
           (format-type (string->symbol
                        (option-ref options 'format "text"))))

      (case format-type
        ((text)
         (for-each (lambda (d)
                    (format #t "~a | ~a | ~a | ~a | ~a | ~a~%"
                           (deployment-event-id d)
                           (deployment-event-service-name d)
                           (deployment-event-version d)
                           (deployment-event-environment d)
                           (deployment-event-deployment-type d)
                           (deployment-event-status d)))
                  deployments))
        ((json)
         (display (deployments->json deployments))
         (newline))
        ((csv)
         (display "id,service,version,environment,type,status\n")
         (for-each (lambda (d)
                    (format #t "~a,~a,~a,~a,~a,~a~%"
                           (deployment-event-id d)
                           (deployment-event-service-name d)
                           (deployment-event-version d)
                           (deployment-event-environment d)
                           (deployment-event-deployment-type d)
                           (deployment-event-status d)))
                  deployments)))

      (close-deployment-db db))))

(define (handle-list-services args config)
  "Handle the list-services command"
  (let* ((db-path (assoc-ref config 'database)))
    (ensure-database-dir db-path)
    (let* ((db (open-deployment-db db-path))
           (services (list-services db)))
      (for-each (lambda (svc)
                 (format #t "~a (~a) - ~a dependencies~%"
                        (service-metadata-name svc)
                        (service-metadata-type svc)
                        (length (service-metadata-dependencies svc))))
               services)
      (close-deployment-db db))))

(define (handle-show-metrics args config)
  "Handle the show-metrics command"
  (let* ((options (getopt-long (cons "show-metrics" args)
                               show-metrics-options))
         (db-path (assoc-ref config 'database)))

    (when (option-ref options 'help #f)
      (display "Usage: guile-deploy-ledger show-metrics [OPTIONS]
Display deployment metrics.

Options:
  -h, --help         Show this help message
  -s, --service      Service name
  -m, --metric       Metric type (frequency|mttr|success-rate|all)
  -p, --period       Period in days (default: 30)
  -f, --format       Output format (text|json)
  --group-by         Group results by (service|type|environment)
")
      (exit 0))

    (ensure-database-dir db-path)
    (let* ((db (open-deployment-db db-path))
           (service (option-ref options 'service #f))
           (metric-type (string->symbol
                        (option-ref options 'metric "all")))
           (period (string->number
                   (option-ref options 'period "30"))))

      (case metric-type
        ((frequency)
         (format #t "Deployment frequency: ~,2f per day~%"
                (calculate-deployment-frequency db service #:period-days period)))
        ((mttr)
         (format #t "Mean Time To Recovery: ~,2f hours~%"
                (/ (calculate-mttr db service #:period-days period) 3600)))
        ((success-rate)
         (format #t "Success rate: ~,1f%~%"
                (calculate-success-rate db service #:period-days period)))
        ((all)
         (let ((metrics (aggregate-metrics db #:services (if service (list service) #f)
                                             #:period-days period)))
           (for-each (lambda (m)
                      (format #t "Service: ~a~%" (deployment-metrics-service-name m))
                      (format #t "  Total deployments: ~a~%" (deployment-metrics-total-deployments m))
                      (format #t "  Success rate: ~,1f%~%" (deployment-metrics-success-rate m))
                      (format #t "  MTTR: ~,2f hours~%" (/ (deployment-metrics-mttr m) 3600))
                      (format #t "  Frequency: ~,2f/day~%" (deployment-metrics-frequency m))
                      (newline))
                    metrics))))

      (close-deployment-db db))))

(define (handle-export args config)
  "Handle the export command"
  (format #t "Export functionality will be implemented in the reporting module~%"))

(define (handle-visualize args config)
  "Handle the visualize command"
  (format #t "Visualization functionality will be implemented in the reporting module~%"))

(define (handle-serve args config)
  "Handle the serve command"
  (format #t "Server functionality will be implemented in the integrations module~%"))

(define (handle-import args config)
  "Handle the import command"
  (format #t "Import functionality will be implemented~%"))

(define (handle-health args config)
  "Handle the health command"
  (let* ((db-path (assoc-ref config 'database)))
    (ensure-database-dir db-path)
    (let* ((db (open-deployment-db db-path))
           (services (list-services db)))
      (for-each (lambda (svc)
                 (let ((score (service-health-score db (service-metadata-name svc))))
                   (format #t "~a: ~,1f/100~%" (service-metadata-name svc) score)))
               services)
      (close-deployment-db db))))

(define (handle-analyze args config)
  "Handle the analyze command"
  (let* ((db-path (assoc-ref config 'database)))
    (ensure-database-dir db-path)
    (let* ((db (open-deployment-db db-path))
           (patterns (deployment-patterns db)))
      (format #t "Deployment patterns analysis:~%")
      (for-each (lambda (p)
                 (format #t "  ~a: ~a~%" (car p) (cdr p)))
               patterns)
      (close-deployment-db db))))

;;; Helper for JSON export (stub)
(define (deployments->json deployments)
  "Convert deployments to JSON string"
  (format #f "[~{~a~^,~}]"
          (map (lambda (d)
                 (format #f "{\"id\":\"~a\",\"service\":\"~a\",\"version\":\"~a\"}"
                        (deployment-event-id d)
                        (deployment-event-service-name d)
                        (deployment-event-version d)))
               deployments)))