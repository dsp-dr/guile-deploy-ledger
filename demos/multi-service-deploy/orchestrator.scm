#!/usr/bin/env guile
!#

;;; Multi-Service Deployment Orchestrator Demo
;;; Demonstrates coordinated deployment of multiple interdependent services

(use-modules (deploy-ledger core types)
             (srfi srfi-19)
             (srfi srfi-1)
             (ice-9 format)
             (ice-9 match)
             (ice-9 rdelim))

;;; Service dependency graph

(define service-graph
  '((frontend-ui
     (version . "v4.0.0")
     (depends . (api-gateway cdn-service))
     (replicas . 5)
     (strategy . blue-green))

    (api-gateway
     (version . "v2.4.0")
     (depends . (auth-service user-service order-service))
     (replicas . 3)
     (strategy . canary))

    (auth-service
     (version . "v1.8.0")
     (depends . (redis-cache postgres-db))
     (replicas . 2)
     (strategy . rolling))

    (user-service
     (version . "v3.1.0")
     (depends . (postgres-db redis-cache))
     (replicas . 3)
     (strategy . rolling))

    (order-service
     (version . "v2.5.0")
     (depends . (postgres-db message-queue inventory-service))
     (replicas . 4)
     (strategy . canary))

    (inventory-service
     (version . "v1.6.0")
     (depends . (postgres-db))
     (replicas . 2)
     (strategy . rolling))

    (cdn-service
     (version . "v1.2.0")
     (depends . ())
     (replicas . 1)
     (strategy . blue-green))

    (postgres-db
     (version . "v13.4")
     (depends . ())
     (replicas . 1)
     (strategy . blue-green))

    (redis-cache
     (version . "v6.2")
     (depends . ())
     (replicas . 1)
     (strategy . rolling))

    (message-queue
     (version . "v3.8.0")
     (depends . ())
     (replicas . 1)
     (strategy . rolling))))

;;; Deployment orchestration utilities

(define (get-service-info name)
  (assoc name service-graph))

(define (get-dependencies service-name)
  (let ((service (get-service-info service-name)))
    (if service
        (assoc-ref (cdr service) 'depends)
        '())))

(define (topological-sort services)
  "Sort services in dependency order for deployment"
  (define (visit node visited sorted)
    (if (member node visited)
        (values visited sorted)
        (let ((deps (get-dependencies node)))
          (let loop ((deps deps)
                     (vis visited)
                     (srt sorted))
            (if (null? deps)
                (values (cons node vis)
                        (cons node srt))
                (let-values (((new-vis new-srt)
                             (visit (car deps) vis srt)))
                  (loop (cdr deps) new-vis new-srt)))))))

  (let loop ((nodes services)
             (visited '())
             (sorted '()))
    (if (null? nodes)
        (reverse sorted)
        (let-values (((new-visited new-sorted)
                     (visit (car nodes) visited sorted)))
          (loop (cdr nodes) new-visited new-sorted)))))

;;; Display utilities

(define (print-header title)
  (let ((width 70)
        (padding (make-string 70 #\═)))
    (format #t "~%~a~%" padding)
    (format #t "  ~a~%" title)
    (format #t "~a~%~%" padding)))

(define (print-service-tree)
  "Display the service dependency tree"
  (format #t "Service Dependency Tree:~%")
  (format #t "~%")

  (define (print-node name level)
    (let ((indent (make-string (* level 2) #\space))
          (service (get-service-info name)))
      (format #t "~a├─ ~a (~a)~%"
              indent
              name
              (assoc-ref (cdr service) 'version))
      (let ((deps (get-dependencies name)))
        (for-each
         (lambda (dep)
           (unless (> level 3)  ; Prevent infinite recursion
             (print-node dep (+ level 1))))
         deps))))

  (print-node 'frontend-ui 0))

(define (deployment-progress-bar current total)
  "Create a visual progress bar"
  (let* ((width 30)
         (filled (inexact->exact
                 (floor (* width (/ current total)))))
         (empty (- width filled))
         (percentage (inexact->exact
                     (floor (* 100 (/ current total))))))
    (format #f "[~a~a] ~a%"
            (make-string filled #\█)
            (make-string empty #\░)
            percentage)))

;;; Deployment simulation

(define (simulate-deployment service-name)
  "Simulate deploying a single service"
  (let* ((service (get-service-info service-name))
         (version (assoc-ref (cdr service) 'version))
         (replicas (assoc-ref (cdr service) 'replicas))
         (strategy (assoc-ref (cdr service) 'strategy)))

    ;; Create deployment event
    (define deployment
      (make-deployment-event
       #:service-name (symbol->string service-name)
       #:version version
       #:environment 'production
       #:deployment-type strategy
       #:initiator "orchestrator"
       #:metadata `((replicas . ,replicas)
                   (orchestration-id . "orch-12345"))))

    (format #t "~%Deploying ~a:~%" service-name)
    (format #t "  Version: ~a~%" version)
    (format #t "  Strategy: ~a~%" strategy)
    (format #t "  Replicas: ~a~%" replicas)
    (format #t "  Status: ")

    ;; Simulate deployment progress
    (do ((i 0 (+ i 1)))
        ((>= i replicas))
      (format #t ".")
      (flush-output-port (current-output-port))
      (sleep 1))

    (format #t " \x1b[32m✓ Deployed\x1b[0m~%")

    deployment))

(define (check-service-health service-name)
  "Check if a service is healthy"
  (format #t "  Health check for ~a... " service-name)
  (sleep 1)
  (format #t "\x1b[32m✓ Healthy\x1b[0m~%")
  #t)

(define (rollback-service service-name deployment)
  "Rollback a service deployment"
  (format #t "  \x1b[31mRolling back ~a...\x1b[0m " service-name)
  (sleep 2)
  (format #t "\x1b[32m✓ Rolled back\x1b[0m~%"))

;;; Main orchestration logic

(define (orchestrate-deployment)
  "Main function to orchestrate multi-service deployment"

  (print-header "MULTI-SERVICE DEPLOYMENT ORCHESTRATOR")

  (format #t "Deployment ID: orch-~a~%"
          (number->string (time-second (current-time time-utc))))
  (format #t "Target Environment: PRODUCTION~%")
  (format #t "Total Services: ~a~%"
          (length service-graph))
  (newline)

  ;; Show dependency tree
  (print-service-tree)

  (format #t "~%Press Enter to begin orchestrated deployment...~%")
  (read-line)

  ;; Calculate deployment order
  (define all-services (map car service-graph))
  (define deployment-order (topological-sort all-services))

  (format #t "~%Deployment Order (respecting dependencies):~%")
  (let loop ((services deployment-order)
             (index 1))
    (unless (null? services)
      (format #t "  ~a. ~a~%" index (car services))
      (loop (cdr services) (+ index 1))))

  (format #t "~%Starting deployment sequence...~%")
  (format #t "~%")

  ;; Track deployments
  (define successful-deployments '())
  (define failed-deployments '())

  ;; Deploy each service in order
  (let deploy-loop ((remaining deployment-order)
                    (completed '()))
    (unless (null? remaining)
      (let* ((service-name (car remaining))
             (deps (get-dependencies service-name))
             (deps-ready? (every (lambda (dep)
                                  (member dep completed))
                                deps)))

        (if deps-ready?
            (begin
              (format #t "~%")
              (format #t "────────────────────────────────────~%")
              (format #t "Service: ~a~%" service-name)
              (format #t "Dependencies: ~a~%"
                      (if (null? deps) "None" deps))

              ;; Check dependencies are healthy
              (let ((deps-healthy?
                     (every check-service-health deps)))

                (if deps-healthy?
                    (let ((deployment (simulate-deployment service-name)))
                      ;; Check if deployment succeeded
                      (if (check-service-health service-name)
                          (begin
                            (set! successful-deployments
                                  (cons service-name successful-deployments))
                            (deploy-loop (cdr remaining)
                                       (cons service-name completed)))
                          (begin
                            ;; Deployment failed
                            (format #t "  \x1b[31m✗ Deployment failed!\x1b[0m~%")
                            (set! failed-deployments
                                  (cons service-name failed-deployments))

                            ;; Initiate rollback
                            (format #t "~%\x1b[33mInitiating cascade rollback...\x1b[0m~%")
                            (for-each
                             (lambda (svc)
                               (rollback-service svc #f))
                             successful-deployments))))
                    (begin
                      (format #t "  \x1b[31m✗ Dependencies unhealthy!\x1b[0m~%")
                      (set! failed-deployments
                            (cons service-name failed-deployments))))))
            (format #t "~%\x1b[33mSkipping ~a - dependencies not ready\x1b[0m~%"
                    service-name)))))

  ;; Final summary
  (newline)
  (print-header "DEPLOYMENT SUMMARY")

  (format #t "Deployment Statistics:~%")
  (format #t "  Total Services: ~a~%"
          (length service-graph))
  (format #t "  Successful: \x1b[32m~a\x1b[0m~%"
          (length successful-deployments))
  (format #t "  Failed: \x1b[31m~a\x1b[0m~%"
          (length failed-deployments))
  (format #t "  Success Rate: ~a%~%"
          (inexact->exact
           (floor (* 100 (/ (length successful-deployments)
                           (length service-graph))))))
  (newline)

  (unless (null? successful-deployments)
    (format #t "Successfully Deployed Services:~%")
    (for-each
     (lambda (svc)
       (format #t "  \x1b[32m✓\x1b[0m ~a~%" svc))
     (reverse successful-deployments))
    (newline))

  (unless (null? failed-deployments)
    (format #t "Failed Services:~%")
    (for-each
     (lambda (svc)
       (format #t "  \x1b[31m✗\x1b[0m ~a~%" svc))
     (reverse failed-deployments))
    (newline))

  ;; Deployment timeline
  (format #t "Deployment Timeline:~%")
  (let ((start-time (current-time time-utc)))
    (format #t "  Started: ~a~%"
            (date->string (time-utc->date start-time)
                         "~Y-~m-~d ~H:~M:~S"))
    (format #t "  Completed: ~a~%"
            (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))
    (format #t "  Duration: ~a seconds~%"
            (length successful-deployments)))

  (newline)
  (format #t "Deployment orchestration complete.~%"))

;;; Monitoring dashboard

(define (display-deployment-status services)
  "Display real-time status of all services"
  (format #t "~%")
  (format #t "╔════════════════════════════════════════════════════════════╗~%")
  (format #t "║            MULTI-SERVICE DEPLOYMENT STATUS                 ║~%")
  (format #t "╠════════════════════════════════════════════════════════════╣~%")
  (format #t "║ Service            │ Version │ Status     │ Progress       ║~%")
  (format #t "╟────────────────────┼─────────┼────────────┼────────────────╢~%")

  (for-each
   (lambda (service-name)
     (let* ((service (get-service-info service-name))
            (version (assoc-ref (cdr service) 'version))
            (status "Deployed")
            (progress (deployment-progress-bar 100 100)))
       (format #t "║ ~18a │ ~7a │ ~10a │ ~14a ║~%"
               service-name version status progress)))
   services)

  (format #t "╚════════════════════════════════════════════════════════════╝~%"))

;;; Entry point

(define (main)
  (catch #t
    (lambda ()
      (orchestrate-deployment)
      (newline)
      (format #t "Demo completed successfully!~%"))
    (lambda (key . args)
      (format #t "~%Error during orchestration: ~a~%" key))))

;; Run if executed directly
(when (equal? (car (command-line)) "orchestrator.scm")
  (main))