;;; types.scm -- Core data types for deployment ledger
;;; Copyright (C) 2024 DSP-DR
;;;
;;; This file is part of Guile Deploy Ledger.

(define-module (deploy-ledger core types)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:export (make-deployment-event
            deployment-event?
            deployment-event-id
            deployment-event-service-name
            deployment-event-version
            deployment-event-environment
            deployment-event-deployment-type
            deployment-event-started
            deployment-event-completed
            deployment-event-status
            deployment-event-initiator
            deployment-event-metadata
            deployment-event-parent-id
            set-deployment-event-completed!
            set-deployment-event-status!

            make-rollback-event
            rollback-event?
            rollback-event-id
            rollback-event-service-name
            rollback-event-from-version
            rollback-event-to-version
            rollback-event-reason
            rollback-event-timestamp
            rollback-event-initiator
            rollback-event-impact
            rollback-event-deployment-id

            make-service-metadata
            service-metadata?
            service-metadata-name
            service-metadata-type
            service-metadata-dependencies
            service-metadata-owners
            service-metadata-repository
            service-metadata-health-check
            service-metadata-created
            service-metadata-updated

            make-deployment-metrics
            deployment-metrics?
            deployment-metrics-service-name
            deployment-metrics-period-start
            deployment-metrics-period-end
            deployment-metrics-total-deployments
            deployment-metrics-successful-deployments
            deployment-metrics-failed-deployments
            deployment-metrics-rollback-count
            deployment-metrics-average-duration
            deployment-metrics-mttr
            deployment-metrics-frequency
            deployment-metrics-success-rate

            deployment-type?
            valid-deployment-types
            deployment-status?
            valid-deployment-statuses
            service-type?
            valid-service-types

            generate-id
            current-timestamp))

;;; Enumerations

(define valid-deployment-types
  '(blue-green canary rolling big-bang feature-flag a-b-test))

(define valid-deployment-statuses
  '(pending in-progress success failure rolled-back partial cancelled))

(define valid-service-types
  '(monolith microservice serverless batch-job edge-function))

;;; Predicates

(define (deployment-type? sym)
  (memq sym valid-deployment-types))

(define (deployment-status? sym)
  (memq sym valid-deployment-statuses))

(define (service-type? sym)
  (memq sym valid-service-types))

;;; Utility functions

(define (generate-id)
  "Generate a unique ID for events"
  (string-append "deploy-"
                 (number->string (current-time))
                 "-"
                 (number->string (random 999999))))

(define (current-timestamp)
  "Get current timestamp as SRFI-19 time object"
  (current-time time-utc))

;;; Deployment Event Record

(define-record-type <deployment-event>
  (make-deployment-event-internal id service-name version environment
                                  deployment-type started completed status
                                  initiator metadata parent-id)
  deployment-event?
  (id deployment-event-id)
  (service-name deployment-event-service-name)
  (version deployment-event-version)
  (environment deployment-event-environment)
  (deployment-type deployment-event-deployment-type)
  (started deployment-event-started)
  (completed deployment-event-completed set-deployment-event-completed!)
  (status deployment-event-status set-deployment-event-status!)
  (initiator deployment-event-initiator)
  (metadata deployment-event-metadata)
  (parent-id deployment-event-parent-id))

(define* (make-deployment-event #:key
                                 (id (generate-id))
                                 service-name
                                 version
                                 environment
                                 (deployment-type 'rolling)
                                 (started (current-timestamp))
                                 (completed #f)
                                 (status 'pending)
                                 (initiator "system")
                                 (metadata '())
                                 (parent-id #f))
  "Create a new deployment event record"
  (unless (deployment-type? deployment-type)
    (error "Invalid deployment type" deployment-type))
  (unless (deployment-status? status)
    (error "Invalid deployment status" status))
  (make-deployment-event-internal id service-name version environment
                                   deployment-type started completed status
                                   initiator metadata parent-id))

;;; Rollback Event Record

(define-record-type <rollback-event>
  (make-rollback-event-internal id service-name from-version to-version
                                 reason timestamp initiator impact
                                 deployment-id)
  rollback-event?
  (id rollback-event-id)
  (service-name rollback-event-service-name)
  (from-version rollback-event-from-version)
  (to-version rollback-event-to-version)
  (reason rollback-event-reason)
  (timestamp rollback-event-timestamp)
  (initiator rollback-event-initiator)
  (impact rollback-event-impact)
  (deployment-id rollback-event-deployment-id))

(define* (make-rollback-event #:key
                               (id (generate-id))
                               service-name
                               from-version
                               to-version
                               reason
                               (timestamp (current-timestamp))
                               (initiator "system")
                               (impact '())
                               deployment-id)
  "Create a new rollback event record"
  (make-rollback-event-internal id service-name from-version to-version
                                 reason timestamp initiator impact
                                 deployment-id))

;;; Service Metadata Record

(define-record-type <service-metadata>
  (make-service-metadata-internal name type dependencies owners
                                   repository health-check created updated)
  service-metadata?
  (name service-metadata-name)
  (type service-metadata-type)
  (dependencies service-metadata-dependencies)
  (owners service-metadata-owners)
  (repository service-metadata-repository)
  (health-check service-metadata-health-check)
  (created service-metadata-created)
  (updated service-metadata-updated))

(define* (make-service-metadata #:key
                                 name
                                 (type 'microservice)
                                 (dependencies '())
                                 (owners '())
                                 (repository #f)
                                 (health-check #f)
                                 (created (current-timestamp))
                                 (updated (current-timestamp)))
  "Create a new service metadata record"
  (unless (service-type? type)
    (error "Invalid service type" type))
  (make-service-metadata-internal name type dependencies owners
                                   repository health-check created updated))

;;; Deployment Metrics Record

(define-record-type <deployment-metrics>
  (make-deployment-metrics-internal service-name period-start period-end
                                     total-deployments successful-deployments
                                     failed-deployments rollback-count
                                     average-duration mttr frequency
                                     success-rate)
  deployment-metrics?
  (service-name deployment-metrics-service-name)
  (period-start deployment-metrics-period-start)
  (period-end deployment-metrics-period-end)
  (total-deployments deployment-metrics-total-deployments)
  (successful-deployments deployment-metrics-successful-deployments)
  (failed-deployments deployment-metrics-failed-deployments)
  (rollback-count deployment-metrics-rollback-count)
  (average-duration deployment-metrics-average-duration)
  (mttr deployment-metrics-mttr)
  (frequency deployment-metrics-frequency)
  (success-rate deployment-metrics-success-rate))

(define* (make-deployment-metrics #:key
                                   service-name
                                   period-start
                                   period-end
                                   (total-deployments 0)
                                   (successful-deployments 0)
                                   (failed-deployments 0)
                                   (rollback-count 0)
                                   (average-duration 0)
                                   (mttr 0)
                                   (frequency 0)
                                   (success-rate 0))
  "Create a new deployment metrics record"
  (make-deployment-metrics-internal service-name period-start period-end
                                     total-deployments successful-deployments
                                     failed-deployments rollback-count
                                     average-duration mttr frequency
                                     success-rate))