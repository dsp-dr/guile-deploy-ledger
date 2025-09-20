;;; monolith-deployment.scm -- Example monolithic application deployment
;;; Copyright (C) 2024 DSP-DR

(add-to-load-path "../src")

(use-modules (deploy-ledger core types)
             (deploy-ledger storage sqlite)
             (deploy-ledger query metrics)
             (ice-9 format))

(define db-file "example-monolith.db")

(define (simulate-monolith-deployment)
  "Simulate a blue-green deployment for a monolithic application"
  (let ((db (open-deployment-db db-file)))

    ;; Register the monolithic application
    (store-service-metadata! db
      (make-service-metadata
       #:name "legacy-erp-system"
       #:type 'monolith
       #:dependencies '("oracle-db" "redis-cache" "file-storage")
       #:owners '("platform-team" "erp-team")
       #:repository "https://github.com/company/legacy-erp"
       #:health-check "/health"))

    ;; Phase 1: Deploy to blue environment
    (format #t "~%=== Phase 1: Deploying to Blue Environment ===~%")
    (let ((blue-deployment
           (make-deployment-event
            #:service-name "legacy-erp-system"
            #:version "2024.1.15"
            #:environment "production-blue"
            #:deployment-type 'blue-green
            #:status 'in-progress
            #:initiator "jenkins-pipeline"
            #:metadata '((deployment-window . "02:00-04:00 UTC")
                        (downtime-expected . #f)
                        (database-migration . "v2024.1.15_schema.sql")))))

      (store-deployment! db blue-deployment)
      (format #t "Deployment started: ~a~%" (deployment-event-id blue-deployment))

      ;; Simulate deployment progress
      (sleep 2)
      (format #t "Running database migrations...~%")
      (sleep 2)
      (format #t "Copying application artifacts...~%")
      (sleep 2)
      (format #t "Starting application servers...~%")
      (sleep 2)

      ;; Update status to success
      (update-deployment-status! db (deployment-event-id blue-deployment)
                                 'success (current-time time-utc))
      (format #t "Blue environment deployment complete!~%"))

    ;; Phase 2: Health checks
    (format #t "~%=== Phase 2: Running Health Checks ===~%")
    (sleep 1)
    (format #t "✓ Database connectivity: OK~%")
    (sleep 1)
    (format #t "✓ Cache connectivity: OK~%")
    (sleep 1)
    (format #t "✓ Application endpoints: OK~%")
    (sleep 1)
    (format #t "✓ Background jobs: OK~%")

    ;; Phase 3: Traffic switch
    (format #t "~%=== Phase 3: Switching Traffic to Blue ===~%")
    (let ((switch-deployment
           (make-deployment-event
            #:service-name "legacy-erp-system"
            #:version "2024.1.15"
            #:environment "production"
            #:deployment-type 'blue-green
            #:status 'success
            #:initiator "jenkins-pipeline"
            #:parent-id (deployment-event-id blue-deployment)
            #:metadata '((traffic-switched . #t)
                        (previous-version . "2024.1.14")))))

      (store-deployment! db switch-deployment)
      (format #t "Traffic switched successfully!~%"))

    ;; Simulate a failure scenario requiring rollback
    (format #t "~%=== Simulating Performance Issue ===~%")
    (sleep 3)
    (format #t "⚠ Performance degradation detected!~%")
    (format #t "  - Response time increased by 300%~%")
    (format #t "  - CPU usage at 95%~%")

    ;; Perform rollback
    (format #t "~%=== Initiating Rollback ===~%")
    (let ((rollback
           (make-rollback-event
            #:service-name "legacy-erp-system"
            #:from-version "2024.1.15"
            #:to-version "2024.1.14"
            #:reason "Performance degradation - response time exceeded SLA"
            #:initiator "automated-monitoring"
            #:impact '("user-sessions" "batch-processing" "reporting"))))

      (store-rollback! db rollback)
      (format #t "Rollback initiated: ~a~%" (rollback-event-id rollback))
      (sleep 2)
      (format #t "Switching traffic back to green environment...~%")
      (sleep 2)
      (format #t "Rollback complete!~%"))

    ;; Display metrics
    (format #t "~%=== Deployment Metrics ===~%")
    (format #t "Total deployments: ~a~%"
           (length (list-deployments db #:service-name "legacy-erp-system")))
    (format #t "Success rate: ~,1f%~%"
           (calculate-success-rate db "legacy-erp-system"))
    (format #t "MTTR: ~,1f minutes~%"
           (/ (calculate-mttr db "legacy-erp-system") 60))

    (close-deployment-db db)))

;; Run the simulation
(simulate-monolith-deployment)