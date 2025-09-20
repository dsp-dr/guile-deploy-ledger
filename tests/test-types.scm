;;; test-types.scm -- Tests for core data types
;;; Copyright (C) 2024 DSP-DR

(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))

(use-modules (deploy-ledger core types)
             (srfi srfi-64)
             (srfi srfi-19))

(test-begin "deployment-types")

(test-group "deployment-event"
  (test-assert "create deployment event"
    (let ((deployment (make-deployment-event
                      #:service-name "test-service"
                      #:version "1.0.0"
                      #:environment "staging"
                      #:deployment-type 'rolling)))
      (and (deployment-event? deployment)
           (string=? (deployment-event-service-name deployment) "test-service")
           (string=? (deployment-event-version deployment) "1.0.0")
           (string=? (deployment-event-environment deployment) "staging")
           (eq? (deployment-event-deployment-type deployment) 'rolling)
           (eq? (deployment-event-status deployment) 'pending))))

  (test-assert "deployment with custom ID"
    (let ((deployment (make-deployment-event
                      #:id "custom-id-123"
                      #:service-name "test-service"
                      #:version "1.0.0"
                      #:environment "production")))
      (string=? (deployment-event-id deployment) "custom-id-123")))

  (test-assert "deployment status update"
    (let ((deployment (make-deployment-event
                      #:service-name "test-service"
                      #:version "1.0.0"
                      #:environment "staging")))
      (set-deployment-event-status! deployment 'success)
      (eq? (deployment-event-status deployment) 'success)))

  (test-error "invalid deployment type"
    (make-deployment-event
     #:service-name "test"
     #:version "1.0"
     #:environment "prod"
     #:deployment-type 'invalid-type))

  (test-error "invalid deployment status"
    (make-deployment-event
     #:service-name "test"
     #:version "1.0"
     #:environment "prod"
     #:status 'invalid-status)))

(test-group "rollback-event"
  (test-assert "create rollback event"
    (let ((rollback (make-rollback-event
                    #:service-name "test-service"
                    #:from-version "2.0.0"
                    #:to-version "1.9.0"
                    #:reason "Performance issues")))
      (and (rollback-event? rollback)
           (string=? (rollback-event-service-name rollback) "test-service")
           (string=? (rollback-event-from-version rollback) "2.0.0")
           (string=? (rollback-event-to-version rollback) "1.9.0")
           (string=? (rollback-event-reason rollback) "Performance issues"))))

  (test-assert "rollback with deployment ID"
    (let ((rollback (make-rollback-event
                    #:service-name "test-service"
                    #:from-version "2.0.0"
                    #:to-version "1.9.0"
                    #:reason "Failed health checks"
                    #:deployment-id "deploy-123")))
      (string=? (rollback-event-deployment-id rollback) "deploy-123"))))

(test-group "service-metadata"
  (test-assert "create service metadata"
    (let ((metadata (make-service-metadata
                    #:name "api-gateway"
                    #:type 'microservice
                    #:dependencies '("auth-service" "user-service"))))
      (and (service-metadata? metadata)
           (string=? (service-metadata-name metadata) "api-gateway")
           (eq? (service-metadata-type metadata) 'microservice)
           (equal? (service-metadata-dependencies metadata)
                  '("auth-service" "user-service")))))

  (test-error "invalid service type"
    (make-service-metadata
     #:name "test"
     #:type 'invalid-type)))

(test-group "deployment-metrics"
  (test-assert "create deployment metrics"
    (let* ((start (days-ago 30))
           (end (current-time time-utc))
           (metrics (make-deployment-metrics
                    #:service-name "test-service"
                    #:period-start start
                    #:period-end end
                    #:total-deployments 50
                    #:successful-deployments 47
                    #:failed-deployments 3
                    #:success-rate 94.0)))
      (and (deployment-metrics? metrics)
           (string=? (deployment-metrics-service-name metrics) "test-service")
           (= (deployment-metrics-total-deployments metrics) 50)
           (= (deployment-metrics-success-rate metrics) 94.0)))))

(test-group "utility-functions"
  (test-assert "generate-id creates unique IDs"
    (let ((id1 (generate-id))
          (id2 (generate-id)))
      (and (string? id1)
           (string? id2)
           (not (string=? id1 id2))
           (string-prefix? "deploy-" id1))))

  (test-assert "current-timestamp returns time"
    (let ((timestamp (current-timestamp)))
      (time? timestamp)))

  (test-assert "deployment type validation"
    (and (deployment-type? 'blue-green)
         (deployment-type? 'canary)
         (deployment-type? 'rolling)
         (not (deployment-type? 'invalid))))

  (test-assert "deployment status validation"
    (and (deployment-status? 'success)
         (deployment-status? 'failure)
         (deployment-status? 'in-progress)
         (not (deployment-status? 'invalid))))

  (test-assert "service type validation"
    (and (service-type? 'monolith)
         (service-type? 'microservice)
         (not (service-type? 'invalid)))))

(test-end "deployment-types")