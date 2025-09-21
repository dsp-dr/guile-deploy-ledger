#!/usr/bin/env guile
!#

;;; Rollback Scenario Demo
;;; Demonstrates handling of deployment failures and rollback procedures

(use-modules (deploy-ledger core types)
             (srfi srfi-19)
             (ice-9 format)
             (ice-9 threads)
             (ice-9 rdelim))

;;; Display utilities

(define (print-status message status)
  (let ((status-icon
         (case status
           ((success) "✓")
           ((failure) "✗")
           ((warning) "⚠")
           ((info) "ℹ")
           ((progress) "⟳")
           (else "•")))
        (color-code
         (case status
           ((success) "\x1b[32m")
           ((failure) "\x1b[31m")
           ((warning) "\x1b[33m")
           ((info) "\x1b[36m")
           ((progress) "\x1b[34m")
           (else ""))))
    (format #t "~a~a ~a\x1b[0m~%"
            color-code status-icon message)))

(define (print-alert level message)
  (let ((border (make-string 60 #\═)))
    (format #t "~%\x1b[31m~a\x1b[0m~%" border)
    (format #t "\x1b[31m ALERT: ~a \x1b[0m~%" level)
    (format #t " ~a~%" message)
    (format #t "\x1b[31m~a\x1b[0m~%" border))
  (newline))

(define (simulate-delay seconds)
  "Simulate processing time with progress indicator"
  (do ((i 1 (+ i 1)))
      ((> i seconds))
    (format #t ".")
    (flush-output-port (current-output-port))
    (sleep 1))
  (newline))

(define (display-metrics service)
  "Display service metrics"
  (format #t "~%Service Metrics for ~a:~%" service)
  (format #t "├─ Response Time: ~a~%" "145ms")
  (format #t "├─ Error Rate: ~a~%" "0.3%")
  (format #t "├─ Request/sec: ~a~%" "1,245")
  (format #t "└─ Active Connections: ~a~%" "342")
  (newline))

;;; Scenario setup

(define production-services
  '(("payment-processor" . "v3.1.5")
    ("order-service" . "v2.8.0")
    ("inventory-api" . "v1.9.2")
    ("user-service" . "v4.2.1")
    ("notification-service" . "v2.3.0")))

(define (get-current-version service)
  (assoc-ref production-services service))

;;; Rollback scenario implementation

(define (scenario-introduction)
  (format #t "~%")
  (format #t "════════════════════════════════════════════════════════════~%")
  (format #t "          DEPLOYMENT ROLLBACK SCENARIO DEMO~%")
  (format #t "════════════════════════════════════════════════════════════~%")
  (format #t "~%")
  (format #t "Scenario: Critical production deployment failure requiring~%")
  (format #t "immediate rollback to restore service stability.~%")
  (format #t "~%")
  (format #t "Service: payment-processor~%")
  (format #t "Current Version: v3.1.5 (stable)~%")
  (format #t "Target Version: v3.2.0 (new release)~%")
  (format #t "~%")
  (format #t "Press Enter to begin the deployment...~%")
  (read-line))

(define (phase-1-deployment)
  (print-status "PHASE 1: INITIATING DEPLOYMENT" 'info)
  (newline)

  ;; Create deployment event
  (define deployment
    (make-deployment-event
     #:service-name "payment-processor"
     #:version "v3.2.0"
     #:environment 'production
     #:deployment-type 'rolling
     #:initiator "auto-deploy-pipeline"
     #:metadata '((change-id . "CHG-2024-1234")
                 (approved-by . ("alice" "bob"))
                 (risk-level . "medium"))))

  (print-status "Deployment event created" 'success)
  (format #t "  ID: ~a~%" (deployment-event-id deployment))
  (format #t "  Change ID: CHG-2024-1234~%")
  (newline)

  (print-status "Pre-deployment checks..." 'progress)
  (simulate-delay 2)

  (print-status "✓ Health checks passed" 'success)
  (print-status "✓ Resource availability confirmed" 'success)
  (print-status "✓ Backup created" 'success)
  (newline)

  deployment)

(define (phase-2-rolling-update deployment)
  (print-status "PHASE 2: ROLLING UPDATE IN PROGRESS" 'info)
  (newline)

  (let ((instances 10))
    (format #t "Updating ~a instances...~%" instances)
    (newline)

    (do ((i 1 (+ i 1)))
        ((> i 6))  ; Update 6 instances before failure
      (format #t "  Instance ~a/~a: " i instances)
      (simulate-delay 1)
      (print-status "Updated to v3.2.0" 'success))

    (format #t "  Instance 7/~a: " instances)
    (simulate-delay 2)
    (print-status "Update completed" 'success)

    ;; Simulate error detection
    (newline)
    (print-status "Monitoring new instances..." 'progress)
    (simulate-delay 3)

    ;; Trigger failure
    (print-alert "HIGH SEVERITY"
                "Error rate spike detected in payment processing!")

    #f))  ; Return false to indicate failure

(define (phase-3-problem-detection)
  (print-status "PHASE 3: PROBLEM DETECTION & ANALYSIS" 'failure)
  (newline)

  (format #t "Real-time metrics showing critical issues:~%")
  (newline)

  ;; Show degrading metrics
  (format #t "  Error Rate:~%")
  (format #t "    Baseline: 0.1%~%")
  (format #t "    Current:  \x1b[31m15.3%\x1b[0m ↑~%")
  (newline)

  (format #t "  Response Time:~%")
  (format #t "    Baseline: 145ms~%")
  (format #t "    Current:  \x1b[31m2,450ms\x1b[0m ↑~%")
  (newline)

  (format #t "  Failed Transactions:~%")
  (format #t "    Last hour: \x1b[31m1,247\x1b[0m~%")
  (newline)

  (print-status "Automated alerts triggered:" 'warning)
  (format #t "  • PagerDuty incident created~%")
  (format #t "  • Slack #incidents channel notified~%")
  (format #t "  • On-call engineer paged~%")
  (newline)

  ;; Return problem details
  '((error-rate . 15.3)
    (response-time . 2450)
    (failed-transactions . 1247)
    (severity . critical)))

(define (phase-4-rollback-decision metrics)
  (print-status "PHASE 4: ROLLBACK DECISION" 'warning)
  (newline)

  (format #t "Analyzing impact...~%")
  (simulate-delay 2)

  (format #t "~%Decision Matrix:~%")
  (format #t "┌─────────────────────────┬──────────┐~%")
  (format #t "│ Criteria                │ Status   │~%")
  (format #t "├─────────────────────────┼──────────┤~%")
  (format #t "│ Error rate > 5%         │ \x1b[31m✗ Failed\x1b[0m │~%")
  (format #t "│ Response time < 500ms   │ \x1b[31m✗ Failed\x1b[0m │~%")
  (format #t "│ Transaction success     │ \x1b[31m✗ Failed\x1b[0m │~%")
  (format #t "│ System stable           │ \x1b[31m✗ Failed\x1b[0m │~%")
  (format #t "└─────────────────────────┴──────────┘~%")
  (newline)

  (print-alert "ROLLBACK REQUIRED"
              "Automated rollback initiated due to critical failures")

  #t)  ; Proceed with rollback

(define (phase-5-execute-rollback deployment)
  (print-status "PHASE 5: EXECUTING ROLLBACK" 'progress)
  (newline)

  ;; Create rollback event
  (define rollback
    (make-rollback-event
     #:service-name "payment-processor"
     #:from-version "v3.2.0"
     #:to-version "v3.1.5"
     #:reason "Critical error rate spike - automated rollback"
     #:deployment-id (deployment-event-id deployment)))

  (format #t "Rollback ID: ~a~%" (rollback-event-id rollback))
  (newline)

  (print-status "Step 1: Stopping new traffic to v3.2.0" 'progress)
  (simulate-delay 2)
  (print-status "✓ Traffic routing updated" 'success)

  (print-status "Step 2: Reverting instances to v3.1.5" 'progress)
  (do ((i 7 (- i 1)))
      ((< i 1))
    (format #t "  Reverting instance ~a... " i)
    (simulate-delay 1)
    (format #t "\x1b[32m✓\x1b[0m~%"))

  (print-status "Step 3: Validating rollback" 'progress)
  (simulate-delay 2)
  (print-status "✓ All instances running v3.1.5" 'success)

  (print-status "Step 4: Health checks" 'progress)
  (simulate-delay 2)
  (print-status "✓ All health checks passing" 'success)

  rollback)

(define (phase-6-verify-recovery)
  (print-status "PHASE 6: VERIFYING RECOVERY" 'info)
  (newline)

  (format #t "Post-rollback metrics:~%")
  (newline)

  (format #t "  Error Rate:~%")
  (format #t "    During incident: 15.3%~%")
  (format #t "    Current:        \x1b[32m0.1%\x1b[0m ✓~%")
  (newline)

  (format #t "  Response Time:~%")
  (format #t "    During incident: 2,450ms~%")
  (format #t "    Current:        \x1b[32m142ms\x1b[0m ✓~%")
  (newline)

  (format #t "  System Status:~%")
  (format #t "    \x1b[32m✓\x1b[0m All services operational~%")
  (format #t "    \x1b[32m✓\x1b[0m No active alerts~%")
  (format #t "    \x1b[32m✓\x1b[0m Performance within SLA~%")
  (newline)

  (print-status "SYSTEM RECOVERED - Rollback successful!" 'success)
  #t)

(define (phase-7-post-mortem rollback)
  (print-status "PHASE 7: POST-MORTEM & REPORTING" 'info)
  (newline)

  (format #t "Incident Timeline:~%")
  (format #t "  10:45:00 - Deployment v3.2.0 initiated~%")
  (format #t "  10:52:30 - 70% of instances updated~%")
  (format #t "  10:53:15 - Error rate spike detected~%")
  (format #t "  10:53:45 - Automated rollback triggered~%")
  (format #t "  10:57:00 - Rollback completed~%")
  (format #t "  10:58:00 - System stability confirmed~%")
  (newline)

  (format #t "Impact Summary:~%")
  (format #t "  • Duration: 12 minutes~%")
  (format #t "  • Failed transactions: 1,247~%")
  (format #t "  • Affected users: ~3,500~%")
  (format #t "  • Revenue impact: ~$45,000~%")
  (newline)

  (format #t "Root Cause: Database connection pool exhaustion~%")
  (format #t "due to new connection handling in v3.2.0~%")
  (newline)

  (format #t "Action Items:~%")
  (format #t "  1. Fix connection pool configuration~%")
  (format #t "  2. Add connection pool metrics to canary analysis~%")
  (format #t "  3. Improve load testing scenarios~%")
  (format #t "  4. Update rollback automation thresholds~%")
  (newline)

  (format #t "Reports generated:~%")
  (format #t "  • Incident report: INC-2024-0342~%")
  (format #t "  • RCA document: RCA-2024-0342~%")
  (format #t "  • Deployment ledger updated~%"))

;;; Main scenario execution

(define (run-rollback-scenario)
  (scenario-introduction)

  (let* ((deployment (phase-1-deployment)))
    (format #t "Press Enter to continue with rolling update...~%")
    (read-line)

    (unless (phase-2-rolling-update deployment)
      ;; Deployment failed, proceed with problem detection
      (let ((metrics (phase-3-problem-detection)))
        (format #t "Press Enter to proceed with rollback decision...~%")
        (read-line)

        (when (phase-4-rollback-decision metrics)
          (let ((rollback (phase-5-execute-rollback deployment)))
            (format #t "Press Enter to verify recovery...~%")
            (read-line)

            (when (phase-6-verify-recovery)
              (format #t "Press Enter to view post-mortem...~%")
              (read-line)

              (phase-7-post-mortem rollback)))))))

  (newline)
  (format #t "════════════════════════════════════════════════════════════~%")
  (format #t "              END OF ROLLBACK SCENARIO DEMO~%")
  (format #t "════════════════════════════════════════════════════════════~%")
  (newline)
  (format #t "Key Takeaways:~%")
  (format #t "  • Automated detection and response reduced MTTR~%")
  (format #t "  • Complete audit trail maintained throughout~%")
  (format #t "  • Rollback completed in under 5 minutes~%")
  (format #t "  • System automatically recovered to stable state~%")
  (newline))

;;; Entry point

(when (equal? (car (command-line)) "scenario.scm")
  (run-rollback-scenario))