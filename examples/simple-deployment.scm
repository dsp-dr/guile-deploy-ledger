#!/usr/bin/env guile
!#

;;; Simple deployment example demonstrating the deployment ledger
;;; This example doesn't require SQLite3 module

(use-modules (deploy-ledger core types)
             (srfi srfi-19)
             (ice-9 format))

;; Create a deployment event
(define deployment
  (make-deployment-event
   #:service-name "api-service"
   #:version "v1.2.3"
   #:environment 'production
   #:deployment-type 'blue-green
   #:initiator "john.doe@example.com"
   #:metadata '((region . "us-west-2")
               (cluster . "prod-cluster-1"))))

;; Display the deployment
(format #t "Created deployment:~%")
(format #t "  ID: ~a~%" (deployment-event-id deployment))
(format #t "  Service: ~a~%" (deployment-event-service-name deployment))
(format #t "  Version: ~a~%" (deployment-event-version deployment))
(format #t "  Environment: ~a~%" (deployment-event-environment deployment))
(format #t "  Type: ~a~%" (deployment-event-deployment-type deployment))
(format #t "  Status: ~a~%" (deployment-event-status deployment))
(format #t "  Initiator: ~a~%" (deployment-event-initiator deployment))
(format #t "  Started: ~a~%"
        (date->string
         (time-utc->date (deployment-event-started deployment))
         "~Y-~m-~d ~H:~M:~S"))

;; Simulate deployment completion
;; Note: In a real scenario, this would be done via the storage module
(format #t "~%Deployment status: ~a~%" (deployment-event-status deployment))

;; Create a rollback event
(define rollback
  (make-rollback-event
   #:service-name "api-service"
   #:from-version "v1.2.3"
   #:to-version "v1.2.2"
   #:reason "Performance regression detected"
   #:deployment-id (deployment-event-id deployment)))

(format #t "~%Created rollback:~%")
(format #t "  ID: ~a~%" (rollback-event-id rollback))
(format #t "  Service: ~a~%" (rollback-event-service-name rollback))
(format #t "  From: ~a -> To: ~a~%"
        (rollback-event-from-version rollback)
        (rollback-event-to-version rollback))
(format #t "  Reason: ~a~%" (rollback-event-reason rollback))
(format #t "  Timestamp: ~a~%"
        (date->string
         (time-utc->date (rollback-event-timestamp rollback))
         "~Y-~m-~d ~H:~M:~S"))

;; Create service metadata
(define service-meta
  (make-service-metadata
   #:name "api-service"
   #:type 'microservice
   #:dependencies '("database" "cache" "message-queue")
   #:owners '("platform-team@example.com")
   #:repository "https://github.com/example/api-service"
   #:health-check "https://api.example.com/health"))

(format #t "~%Service metadata:~%")
(format #t "  Name: ~a~%" (service-metadata-name service-meta))
(format #t "  Type: ~a~%" (service-metadata-type service-meta))
(format #t "  Owners: ~a~%" (service-metadata-owners service-meta))
(format #t "  Dependencies: ~a~%" (service-metadata-dependencies service-meta))
(format #t "  Repository: ~a~%" (service-metadata-repository service-meta))
(format #t "  Health check: ~a~%" (service-metadata-health-check service-meta))