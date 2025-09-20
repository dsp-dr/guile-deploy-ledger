;;; microservice-canary.scm -- Example microservice canary deployment
;;; Copyright (C) 2024 DSP-DR

(add-to-load-path "../src")

(use-modules (deploy-ledger core types)
             (deploy-ledger storage sqlite)
             (deploy-ledger query metrics)
             (ice-9 format)
             (srfi srfi-19))

(define db-file "example-microservices.db")

(define (setup-microservices db)
  "Register microservices and their dependencies"
  (for-each
   (lambda (service-def)
     (store-service-metadata! db (apply make-service-metadata service-def)))
   '((#:name "api-gateway"
      #:type microservice
      #:dependencies ("user-service" "payment-service" "notification-service"))
     (#:name "user-service"
      #:type microservice
      #:dependencies ("postgres-users" "redis-sessions"))
     (#:name "payment-service"
      #:type microservice
      #:dependencies ("postgres-payments" "stripe-api" "fraud-detection"))
     (#:name "notification-service"
      #:type microservice
      #:dependencies ("rabbitmq" "email-provider" "sms-provider"))
     (#:name "fraud-detection"
      #:type microservice
      #:dependencies ("ml-model-store" "postgres-fraud")))))

(define (simulate-canary-deployment db service-name version)
  "Simulate a canary deployment with progressive rollout"
  (format #t "~%=== Starting Canary Deployment for ~a ===~%" service-name)

  ;; Phase 1: Deploy canary (10% traffic)
  (format #t "~%Phase 1: Deploying canary (10% traffic)~%")
  (let ((canary-10
         (make-deployment-event
          #:service-name service-name
          #:version version
          #:environment "production"
          #:deployment-type 'canary
          #:status 'in-progress
          #:metadata '((canary-percentage . 10)
                      (baseline-version . "1.5.1")
                      (feature-flags . ("new-algorithm" "enhanced-logging"))))))

    (store-deployment! db canary-10)
    (format #t "Canary deployed: ~a~%" (deployment-event-id canary-10))
    (sleep 2)

    ;; Monitor metrics
    (format #t "Monitoring canary metrics...~%")
    (format #t "  Error rate: 0.1% (baseline: 0.2%) ✓~%")
    (format #t "  P99 latency: 120ms (baseline: 150ms) ✓~%")
    (format #t "  Success rate: 99.9% ✓~%")
    (sleep 2)

    ;; Phase 2: Increase to 25%
    (format #t "~%Phase 2: Increasing to 25% traffic~%")
    (let ((canary-25
           (make-deployment-event
            #:service-name service-name
            #:version version
            #:environment "production"
            #:deployment-type 'canary
            #:status 'in-progress
            #:parent-id (deployment-event-id canary-10)
            #:metadata '((canary-percentage . 25)))))

      (store-deployment! db canary-25)
      (sleep 2)
      (format #t "Monitoring expanded canary...~%")
      (format #t "  Error rate: 0.15% ✓~%")
      (format #t "  P99 latency: 125ms ✓~%")
      (sleep 2))

    ;; Phase 3: Increase to 50%
    (format #t "~%Phase 3: Increasing to 50% traffic~%")
    (let ((canary-50
           (make-deployment-event
            #:service-name service-name
            #:version version
            #:environment "production"
            #:deployment-type 'canary
            #:status 'in-progress
            #:parent-id (deployment-event-id canary-25)
            #:metadata '((canary-percentage . 50)))))

      (store-deployment! db canary-50)
      (sleep 2)
      (format #t "Monitoring half traffic...~%")
      (format #t "  Error rate: 0.2% ✓~%")
      (format #t "  P99 latency: 130ms ✓~%")
      (sleep 2))

    ;; Phase 4: Full rollout
    (format #t "~%Phase 4: Full rollout (100% traffic)~%")
    (let ((full-rollout
           (make-deployment-event
            #:service-name service-name
            #:version version
            #:environment "production"
            #:deployment-type 'canary
            #:status 'success
            #:completed (current-time time-utc)
            #:parent-id (deployment-event-id canary-50)
            #:metadata '((canary-percentage . 100)
                        (rollout-duration-minutes . 15)))))

      (store-deployment! db full-rollout)
      (format #t "Full rollout complete! ✓~%"))))

(define (simulate-coordinated-deployment db)
  "Simulate a coordinated multi-service deployment"
  (format #t "~%=== Coordinated Multi-Service Deployment ===~%")
  (format #t "Deploying new feature across multiple services~%~%")

  ;; Deploy services in dependency order
  (let ((services '(("fraud-detection" "2.0.0")
                   ("payment-service" "3.2.0")
                   ("user-service" "1.6.0")
                   ("notification-service" "2.1.0")
                   ("api-gateway" "4.0.0"))))

    (for-each
     (lambda (service-info)
       (let* ((name (car service-info))
              (version (cadr service-info)))
         (format #t "Deploying ~a v~a...~%" name version)
         (store-deployment! db
           (make-deployment-event
            #:service-name name
            #:version version
            #:environment "production"
            #:deployment-type 'rolling
            #:status 'success
            #:metadata '((coordinated-deployment . #t)
                        (feature . "multi-currency-support"))))
         (sleep 1)))
     services)

    (format #t "~%Coordinated deployment complete!~%")))

(define (display-system-metrics db)
  "Display overall system metrics"
  (format #t "~%=== System-Wide Metrics ===~%")

  (let ((services '("api-gateway" "user-service" "payment-service"
                   "notification-service" "fraud-detection")))
    (for-each
     (lambda (service)
       (format #t "~%~a:~%" service)
       (format #t "  Deployments: ~a~%"
              (length (list-deployments db #:service-name service)))
       (format #t "  Success rate: ~,1f%~%"
              (calculate-success-rate db service))
       (format #t "  Health score: ~,1f/100~%"
              (service-health-score db service)))
     services))

  ;; Dependency impact analysis
  (format #t "~%=== Dependency Impact Analysis ===~%")
  (let ((impact (impact-analysis db "payment-service")))
    (format #t "Services affected by payment-service deployment:~%")
    (for-each (lambda (s) (format #t "  - ~a~%" s)) impact)))

;; Run the simulation
(let ((db (open-deployment-db db-file)))
  (setup-microservices db)

  ;; Simulate various deployment scenarios
  (simulate-canary-deployment db "user-service" "1.5.2")
  (sleep 2)
  (simulate-canary-deployment db "payment-service" "3.1.0")
  (sleep 2)
  (simulate-coordinated-deployment db)

  ;; Display metrics
  (display-system-metrics db)

  (close-deployment-db db))