#!/usr/bin/env guile
!#

;;; Metrics Dashboard Demo
;;; Interactive dashboard showing deployment metrics and analytics

(use-modules (deploy-ledger core types)
             (srfi srfi-19)
             (srfi srfi-1)
             (ice-9 format))

;;; Terminal display utilities

(define (clear-screen)
  (system "clear 2>/dev/null || cls 2>/dev/null || echo"))

(define (make-bar value max-value width)
  "Create a text progress bar"
  (let* ((filled (inexact->exact
                  (floor (* width (/ value max-value)))))
         (empty (- width filled)))
    (string-append
     "["
     (make-string filled #\█)
     (make-string empty #\░)
     "]")))

(define (color-text text color)
  "Add ANSI color codes to text"
  (case color
    ((green) (format #f "\x1b[32m~a\x1b[0m" text))
    ((red) (format #f "\x1b[31m~a\x1b[0m" text))
    ((yellow) (format #f "\x1b[33m~a\x1b[0m" text))
    ((blue) (format #f "\x1b[34m~a\x1b[0m" text))
    ((cyan) (format #f "\x1b[36m~a\x1b[0m" text))
    ((magenta) (format #f "\x1b[35m~a\x1b[0m" text))
    (else text)))

(define (print-header title)
  (let ((line (make-string 80 #\═)))
    (format #t "~a~%" line)
    (format #t "  ~a~%" (color-text title 'cyan))
    (format #t "~a~%" line)))

(define (print-box title content)
  (format #t "~%╔══════════════════════════════════════╗~%")
  (format #t "║ ~a~a ║~%"
          title
          (make-string (- 36 (string-length title)) #\space))
  (format #t "╟──────────────────────────────────────╢~%")
  (for-each
   (lambda (line)
     (format #t "║ ~a~a ║~%"
             line
             (make-string (- 36 (string-length line)) #\space)))
   content)
  (format #t "╚══════════════════════════════════════╝~%"))

;;; Sample data generation

(define (generate-deployment-data)
  "Generate sample deployment data for the last 30 days"
  (let loop ((days 30)
             (data '()))
    (if (= days 0)
        data
        (loop (- days 1)
              (cons (list
                     (- 30 days)
                     (+ 5 (random 15))  ; deployments per day
                     (+ 80 (random 20)) ; success rate
                     (random 3))        ; rollbacks
                    data)))))

(define (generate-service-metrics)
  "Generate metrics for various services"
  '(("api-gateway"      . ((deployments . 45) (success . 97.8) (mttr . 8)))
    ("user-service"     . ((deployments . 34) (success . 94.1) (mttr . 12)))
    ("payment-service"  . ((deployments . 12) (success . 100) (mttr . 5)))
    ("order-service"    . ((deployments . 28) (success . 92.5) (mttr . 15)))
    ("inventory-api"    . ((deployments . 19) (success . 96.3) (mttr . 10)))
    ("notification"     . ((deployments . 56) (success . 98.5) (mttr . 7)))
    ("search-engine"    . ((deployments . 23) (success . 91.2) (mttr . 20)))))

;;; Dashboard components

(define (display-kpi-cards)
  "Display key performance indicator cards"
  (print-box "DEPLOYMENT FREQUENCY"
             (list "Current: 12.4 deployments/day"
                   "Target:  15.0 deployments/day"
                   (make-bar 12.4 15.0 30)))

  (print-box "LEAD TIME FOR CHANGES"
             (list "Current: 2.3 hours"
                   "Target:  2.0 hours"
                   (color-text "↓ 15% improvement" 'green)))

  (print-box "MEAN TIME TO RECOVERY"
             (list "Current: 8 minutes"
                   "Target:  10 minutes"
                   (color-text "✓ Within SLA" 'green)))

  (print-box "CHANGE FAILURE RATE"
             (list "Current: 2.1%"
                   "Target:  < 5%"
                   (color-text "✓ Low failure rate" 'green))))

(define (display-deployment-trend)
  "Display deployment trend graph"
  (format #t "~%~a~%"
          (color-text "DEPLOYMENT TREND (Last 30 Days)" 'cyan))
  (format #t "~%")

  (let ((data (generate-deployment-data))
        (max-deployments 20))
    ;; Y-axis and bars
    (do ((y max-deployments (- y 4)))
        ((< y 0))
      (format #t "~2d │" y)
      (for-each
       (lambda (day-data)
         (let ((deployments (cadr day-data)))
           (if (>= deployments y)
               (format #t "█")
               (format #t " "))))
       data)
      (newline))

    ;; X-axis
    (format #t "   └")
    (format #t "~a" (make-string 30 #\─))
    (format #t "→ Days~%")
    (format #t "    1")
    (format #t "~a" (make-string 25 #\space))
    (format #t "30~%")))

(define (display-service-health-matrix)
  "Display service health matrix"
  (format #t "~%~a~%~%"
          (color-text "SERVICE HEALTH MATRIX" 'cyan))

  (format #t "Service          │ Deploys │ Success │ MTTR  │ Health~%")
  (format #t "─────────────────┼─────────┼─────────┼───────┼──────────~%")

  (for-each
   (lambda (service)
     (let* ((name (car service))
            (metrics (cdr service))
            (deployments (assoc-ref metrics 'deployments))
            (success (assoc-ref metrics 'success))
            (mttr (assoc-ref metrics 'mttr))
            (health (cond
                    ((> success 95) 'green)
                    ((> success 90) 'yellow)
                    (else 'red)))
            (health-icon (case health
                          ((green) "●")
                          ((yellow) "●")
                          ((red) "●"))))
       (format #t "~16a │ ~7a │ ~6,1f% │ ~4dm │ ~a~%"
               name
               deployments
               success
               mttr
               (color-text health-icon health))))
   (generate-service-metrics)))

(define (display-recent-deployments)
  "Display recent deployment events"
  (format #t "~%~a~%~%"
          (color-text "RECENT DEPLOYMENTS" 'cyan))

  (let ((recent-deployments
         '(("10:45" "api-gateway" "v2.4.0" "production" "succeeded")
           ("10:32" "user-service" "v3.1.2" "staging" "succeeded")
           ("10:15" "payment-service" "v1.8.0" "production" "failed")
           ("09:45" "order-service" "v2.2.1" "production" "succeeded")
           ("09:30" "inventory-api" "v1.5.3" "staging" "succeeded"))))

    (format #t "Time  │ Service         │ Version │ Env        │ Status~%")
    (format #t "──────┼─────────────────┼─────────┼────────────┼──────────~%")

    (for-each
     (lambda (deployment)
       (let ((time (list-ref deployment 0))
             (service (list-ref deployment 1))
             (version (list-ref deployment 2))
             (env (list-ref deployment 3))
             (status (list-ref deployment 4)))
         (format #t "~5a │ ~15a │ ~7a │ ~10a │ ~a~%"
                 time service version env
                 (case (string->symbol status)
                   ((succeeded) (color-text "✓ Success" 'green))
                   ((failed) (color-text "✗ Failed" 'red))
                   (else status)))))
     recent-deployments)))

(define (display-alert-panel)
  "Display active alerts and warnings"
  (format #t "~%~a~%~%"
          (color-text "ACTIVE ALERTS" 'yellow))

  (let ((alerts
         '(("HIGH" "payment-service" "Error rate above threshold (5.2%)")
           ("MEDIUM" "api-gateway" "Response time degradation")
           ("LOW" "user-service" "Deployment queue backlog"))))

    (for-each
     (lambda (alert)
       (let ((severity (car alert))
             (service (cadr alert))
             (message (caddr alert)))
         (format #t "~a [~a] ~a: ~a~%"
                 (case (string->symbol severity)
                   ((HIGH) (color-text "▲" 'red))
                   ((MEDIUM) (color-text "▲" 'yellow))
                   ((LOW) (color-text "▲" 'blue)))
                 severity
                 service
                 message)))
     alerts)))

(define (display-deployment-stats)
  "Display deployment statistics summary"
  (format #t "~%~a~%~%"
          (color-text "DEPLOYMENT STATISTICS (Last 24 Hours)" 'cyan))

  (let ((stats '(("Total Deployments" . "127")
                ("Successful" . "119 (93.7%)")
                ("Failed" . "5 (3.9%)")
                ("Rolled Back" . "3 (2.4%)")
                ("Environments" . "prod: 34, staging: 56, dev: 37")
                ("Top Deployer" . "jenkins-pipeline (89)")
                ("Avg Duration" . "4m 32s")
                ("Peak Hour" . "14:00 - 15:00 (18 deployments)"))))

    (for-each
     (lambda (stat)
       (format #t "  ~20a : ~a~%"
               (car stat)
               (cdr stat)))
     stats)))

;;; Main dashboard loop

(define (refresh-dashboard)
  "Refresh the entire dashboard display"
  (clear-screen)
  (print-header "DEPLOYMENT METRICS DASHBOARD")
  (format #t "Last updated: ~a~%"
          (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))

  (display-kpi-cards)
  (display-deployment-trend)
  (display-service-health-matrix)
  (display-recent-deployments)
  (display-alert-panel)
  (display-deployment-stats)

  (format #t "~%")
  (format #t "~a~%"
          (color-text "Press 'q' to quit, 'r' to refresh, or wait for auto-refresh (10s)" 'cyan)))

(define (run-dashboard)
  "Run the interactive dashboard"
  (format #t "Starting Deployment Metrics Dashboard...~%")
  (sleep 1)

  (let loop ()
    (refresh-dashboard)

    ;; Simple input handling (in real implementation would be async)
    (format #t "~%> ")
    (flush-output-port (current-output-port))

    ;; For demo, just refresh every 10 seconds
    (sleep 10)
    (loop)))

;;; Entry point

(define (main args)
  (catch #t
    (lambda ()
      (run-dashboard))
    (lambda (key . args)
      (format #t "~%Dashboard exited.~%"))))

;; Run if executed directly
(when (equal? (car (command-line)) "dashboard.scm")
  (main (command-line)))