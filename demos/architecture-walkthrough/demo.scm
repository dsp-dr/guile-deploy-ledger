#!/usr/bin/env guile
!#

;;; Architecture Walkthrough Demo
;;; This demo walks through the system architecture with live examples

(use-modules (deploy-ledger core types)
             (srfi srfi-19)
             (ice-9 format)
             (ice-9 match)
             (ice-9 rdelim))

(define (print-section title)
  (newline)
  (format #t "~a~%" (make-string 60 #\=))
  (format #t "  ~a~%" title)
  (format #t "~a~%" (make-string 60 #\=))
  (newline))

(define (print-subsection title)
  (newline)
  (format #t "→ ~a~%" title)
  (format #t "~a~%" (make-string (+ 2 (string-length title)) #\-))
  (newline))

(define (pause-for-input)
  (format #t "~%Press Enter to continue...")
  (read-line))

;;; Demo starts here

(print-section "GUILE DEPLOY LEDGER - ARCHITECTURE WALKTHROUGH")

(format #t "Welcome to the architecture walkthrough of the Guile Deploy Ledger system.~%")
(format #t "This demo will showcase the core components and data flow.~%")

(pause-for-input)

;;; Layer 1: Core Data Types

(print-section "LAYER 1: CORE DATA TYPES")

(format #t "The foundation of our system is built on immutable data structures~%")
(format #t "using SRFI-9 records. Let's create some examples:~%")

(print-subsection "Deployment Event")

(define sample-deployment
  (make-deployment-event
   #:service-name "user-api"
   #:version "v2.5.0"
   #:environment 'production
   #:deployment-type 'blue-green
   #:initiator "ci-pipeline"
   #:metadata '((region . "us-east-1")
               (cluster . "prod-k8s")
               (replicas . 5))))

(format #t "Created deployment event:~%")
(format #t "  Type: ~a~%" (record-type-name (record-type-descriptor sample-deployment)))
(format #t "  ID: ~a~%" (deployment-event-id sample-deployment))
(format #t "  Service: ~a~%" (deployment-event-service-name sample-deployment))
(format #t "  Version: ~a~%" (deployment-event-version sample-deployment))
(format #t "  Environment: ~a~%" (deployment-event-environment sample-deployment))
(format #t "  Status: ~a~%" (deployment-event-status sample-deployment))

(pause-for-input)

(print-subsection "Service Metadata")

(define sample-service
  (make-service-metadata
   #:name "user-api"
   #:type 'microservice
   #:dependencies '("postgres" "redis" "message-queue")
   #:owners '("backend-team@example.com")
   #:repository "https://github.com/example/user-api"
   #:health-check "/health"))

(format #t "Service metadata structure:~%")
(format #t "  Name: ~a~%" (service-metadata-name sample-service))
(format #t "  Type: ~a~%" (service-metadata-type sample-service))
(format #t "  Dependencies: ~a~%" (service-metadata-dependencies sample-service))
(format #t "  Owners: ~a~%" (service-metadata-owners sample-service))

(pause-for-input)

;;; Layer 2: Event Processing

(print-section "LAYER 2: EVENT PROCESSING")

(format #t "Events flow through our system in a pipeline:~%")
(format #t "~%")
(format #t "  [Create Event] → [Validate] → [Store] → [Process] → [Notify]~%")
(format #t "~%")

(print-subsection "Event Validation")

(define (validate-deployment deployment)
  (format #t "Validating deployment ~a...~%"
          (deployment-event-id deployment))
  (let ((checks '(("Service name present" . #t)
                 ("Version format valid" . #t)
                 ("Environment allowed" . #t)
                 ("Deployment type supported" . #t))))
    (for-each
     (lambda (check)
       (format #t "  ✓ ~a~%" (car check)))
     checks)
    #t))

(validate-deployment sample-deployment)

(pause-for-input)

(print-subsection "Event Enrichment")

(format #t "Events are enriched with contextual information:~%~%")

(define (enrich-deployment deployment)
  (format #t "Enriching deployment with context...~%")
  (let ((enriched-metadata
         (append (deployment-event-metadata deployment)
                 `((timestamp . ,(current-time time-utc))
                   (git-commit . "abc123def")
                   (previous-version . "v2.4.0")
                   (approvers . ("alice" "bob"))))))
    (format #t "  Added timestamp~%")
    (format #t "  Added git commit~%")
    (format #t "  Added previous version~%")
    (format #t "  Added approvers~%")
    enriched-metadata))

(enrich-deployment sample-deployment)

(pause-for-input)

;;; Layer 3: Storage Abstraction

(print-section "LAYER 3: STORAGE ABSTRACTION")

(format #t "The storage layer provides a clean abstraction over the database.~%")
(format #t "This allows us to swap storage backends without changing application code.~%~%")

(print-subsection "Storage Interface")

(format #t "Key storage operations:~%~%")

(define storage-operations
  '(("store-deployment!" . "Persist a deployment event")
    ("get-deployment" . "Retrieve deployment by ID")
    ("list-deployments" . "Query deployments with filters")
    ("update-deployment-status!" . "Update deployment state")
    ("with-transaction" . "Execute atomic operations")))

(for-each
 (lambda (op)
   (format #t "  • ~a~%    └─ ~a~%~%" (car op) (cdr op)))
 storage-operations)

(pause-for-input)

;;; Layer 4: Query & Analytics

(print-section "LAYER 4: QUERY & ANALYTICS")

(format #t "The query layer provides powerful analytics capabilities.~%~%")

(print-subsection "Deployment Metrics")

(define (calculate-sample-metrics)
  (format #t "Calculating deployment metrics for last 30 days...~%~%")
  (let ((metrics '((total-deployments . 127)
                  (successful . 119)
                  (failed . 5)
                  (rolled-back . 3)
                  (average-duration . "4m 32s")
                  (deployment-frequency . "4.2/day"))))
    (for-each
     (lambda (metric)
       (format #t "  ~a: ~a~%"
               (car metric)
               (cdr metric)))
     metrics)
    metrics))

(calculate-sample-metrics)

(pause-for-input)

(print-subsection "Service Dependencies Graph")

(format #t "Analyzing service dependency graph...~%~%")

(define service-graph
  '((frontend . (api-gateway cdn))
    (api-gateway . (user-api order-api payment-api))
    (user-api . (postgres redis))
    (order-api . (postgres message-queue))
    (payment-api . (postgres payment-gateway))
    (cdn . ())))

(format #t "Service Dependency Tree:~%~%")
(for-each
 (lambda (service)
   (format #t "  ~a~%" (car service))
   (for-each
    (lambda (dep)
      (format #t "    └─ ~a~%" dep))
    (cdr service)))
 service-graph)

(pause-for-input)

;;; Layer 5: Reporting & Visualization

(print-section "LAYER 5: REPORTING & VISUALIZATION")

(format #t "The reporting layer generates various output formats.~%~%")

(print-subsection "Export Formats")

(define export-formats
  '((json . "Machine-readable data exchange")
    (csv . "Spreadsheet analysis")
    (org-mode . "Emacs org-mode tables")
    (markdown . "Documentation and reports")
    (mermaid . "Deployment flow diagrams")
    (graphviz . "Service dependency graphs")))

(format #t "Supported export formats:~%~%")
(for-each
 (lambda (fmt)
   (format #t "  • ~a - ~a~%"
           (car fmt)
           (cdr fmt)))
 export-formats)

(pause-for-input)

(print-subsection "Sample Mermaid Diagram")

(format #t "Generating deployment flow diagram...~%~%")
(format #t "```mermaid~%")
(format #t "graph LR~%")
(format #t "    A[Deploy Initiated] --> B{Validation}~%")
(format #t "    B -->|Valid| C[Deploy to Staging]~%")
(format #t "    B -->|Invalid| X[Abort]~%")
(format #t "    C --> D[Run Tests]~%")
(format #t "    D -->|Pass| E[Deploy to Production]~%")
(format #t "    D -->|Fail| F[Rollback]~%")
(format #t "    E --> G[Monitor]~%")
(format #t "    G -->|Issues| F~%")
(format #t "    G -->|Success| H[Complete]~%")
(format #t "```~%")

(pause-for-input)

;;; Layer 6: Integration Points

(print-section "LAYER 6: INTEGRATION POINTS")

(format #t "The system integrates with various external services.~%~%")

(print-subsection "Webhook System")

(define webhook-events
  '((deployment-started . "POST to Slack, Teams")
    (deployment-completed . "Update JIRA ticket")
    (deployment-failed . "Page on-call engineer")
    (rollback-initiated . "Create incident ticket")))

(format #t "Webhook event mappings:~%~%")
(for-each
 (lambda (event)
   (format #t "  ~a~%    → ~a~%~%"
           (car event)
           (cdr event)))
 webhook-events)

(pause-for-input)

(print-subsection "Monitoring Integration")

(format #t "Prometheus metrics exposed:~%~%")
(format #t "  deployment_total{service,environment,status}~%")
(format #t "  deployment_duration_seconds{service,type}~%")
(format #t "  rollback_total{service,reason}~%")
(format #t "  deployment_frequency_per_hour{environment}~%")

(pause-for-input)

;;; Summary

(print-section "ARCHITECTURE SUMMARY")

(format #t "The Guile Deploy Ledger architecture provides:~%~%")

(define architecture-benefits
  '("✓ Immutable event sourcing for audit compliance"
    "✓ Pluggable storage backends"
    "✓ Rich query and analytics capabilities"
    "✓ Multiple export and visualization formats"
    "✓ Extensive integration options"
    "✓ Functional programming principles throughout"))

(for-each
 (lambda (benefit)
   (format #t "  ~a~%"  benefit))
 architecture-benefits)

(newline)
(format #t "This modular architecture ensures:~%")
(format #t "  • Scalability - Handle thousands of deployments~%")
(format #t "  • Reliability - Immutable data and transactions~%")
(format #t "  • Extensibility - Easy to add new features~%")
(format #t "  • Maintainability - Clear separation of concerns~%")

(newline)
(print-section "END OF ARCHITECTURE WALKTHROUGH")
(format #t "Thank you for exploring the system architecture!~%~%")