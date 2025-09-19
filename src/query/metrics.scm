;;; metrics.scm -- Query interface and metrics calculations
;;; Copyright (C) 2024 DSP-DR
;;;
;;; This file is part of Guile Deploy Ledger.

(define-module (deploy-ledger query metrics)
  #:use-module (deploy-ledger core types)
  #:use-module (deploy-ledger storage sqlite)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:export (calculate-deployment-frequency
            calculate-mttr
            calculate-success-rate
            calculate-rollback-rate
            calculate-average-duration
            get-deployment-timeline
            find-failure-patterns
            analyze-deployment-velocity
            get-service-dependencies
            impact-analysis
            deployment-frequency-trend
            service-health-score
            deployment-patterns
            query-deployments
            query-rollbacks
            aggregate-metrics))

;;; Time utilities

(define (time-difference t1 t2)
  "Calculate difference between two times in seconds"
  (let ((diff (time-difference t1 t2)))
    (+ (time-second diff)
       (/ (time-nanosecond diff) 1000000000))))

(define (days-ago n)
  "Get time n days ago from now"
  (let ((now (current-time time-utc)))
    (make-time time-utc 0 (- (time-second now) (* n 86400)))))

(define (hours-ago n)
  "Get time n hours ago from now"
  (let ((now (current-time time-utc)))
    (make-time time-utc 0 (- (time-second now) (* n 3600)))))

(define (time-in-range? time start end)
  "Check if time is within range"
  (and (time>=? time start)
       (time<=? time end)))

;;; Basic metrics calculations

(define* (calculate-deployment-frequency db service-name
                                          #:key (period-days 30))
  "Calculate deployment frequency for a service over a period"
  (let* ((start-time (days-ago period-days))
         (deployments (list-deployments db
                                         #:service-name service-name
                                         #:from-time start-time)))
    (if (zero? period-days)
        0
        (/ (length deployments) period-days))))

(define* (calculate-mttr db service-name #:key (period-days 30))
  "Calculate Mean Time To Recovery for a service"
  (let* ((start-time (days-ago period-days))
         (deployments (list-deployments db
                                         #:service-name service-name
                                         #:from-time start-time))
         (failures (filter (lambda (d)
                            (eq? (deployment-event-status d) 'failure))
                          deployments))
         (rollbacks (list-rollbacks db
                                   #:service-name service-name
                                   #:from-time start-time)))
    (if (zero? (length failures))
        0
        (let ((recovery-times
               (map (lambda (failure)
                      (let ((recovery (find (lambda (d)
                                             (and (eq? (deployment-event-status d) 'success)
                                                  (time>? (deployment-event-started d)
                                                         (deployment-event-started failure))))
                                           deployments)))
                        (if recovery
                            (time-difference (deployment-event-started recovery)
                                           (deployment-event-started failure))
                            #f)))
                    failures)))
          (let ((valid-times (filter number? recovery-times)))
            (if (null? valid-times)
                0
                (/ (apply + valid-times) (length valid-times))))))))

(define* (calculate-success-rate db service-name #:key (period-days 30))
  "Calculate deployment success rate for a service"
  (let* ((start-time (days-ago period-days))
         (deployments (list-deployments db
                                         #:service-name service-name
                                         #:from-time start-time))
         (successful (filter (lambda (d)
                              (eq? (deployment-event-status d) 'success))
                            deployments))
         (total (length deployments)))
    (if (zero? total)
        0
        (* 100 (/ (length successful) total)))))

(define* (calculate-rollback-rate db service-name #:key (period-days 30))
  "Calculate rollback rate for a service"
  (let* ((start-time (days-ago period-days))
         (deployments (list-deployments db
                                         #:service-name service-name
                                         #:from-time start-time))
         (rollbacks (list-rollbacks db
                                   #:service-name service-name
                                   #:from-time start-time))
         (total (length deployments)))
    (if (zero? total)
        0
        (* 100 (/ (length rollbacks) total)))))

(define* (calculate-average-duration db service-name #:key (period-days 30))
  "Calculate average deployment duration for a service"
  (let* ((start-time (days-ago period-days))
         (deployments (list-deployments db
                                         #:service-name service-name
                                         #:from-time start-time))
         (completed (filter deployment-event-completed deployments)))
    (if (null? completed)
        0
        (let ((durations
               (map (lambda (d)
                      (time-difference (deployment-event-completed d)
                                      (deployment-event-started d)))
                    completed)))
          (/ (apply + durations) (length durations))))))

;;; Advanced queries

(define* (get-deployment-timeline db #:key service-name
                                   (from-time (days-ago 30))
                                   (to-time (current-time time-utc)))
  "Get deployment timeline with events and status"
  (let ((deployments (list-deployments db
                                        #:service-name service-name
                                        #:from-time from-time
                                        #:to-time to-time))
        (rollbacks (list-rollbacks db
                                  #:service-name service-name
                                  #:from-time from-time
                                  #:to-time to-time)))
    (sort (append
           (map (lambda (d)
                  `((type . deployment)
                    (time . ,(deployment-event-started d))
                    (service . ,(deployment-event-service-name d))
                    (version . ,(deployment-event-version d))
                    (status . ,(deployment-event-status d))
                    (deployment-type . ,(deployment-event-deployment-type d))))
                deployments)
           (map (lambda (r)
                  `((type . rollback)
                    (time . ,(rollback-event-timestamp r))
                    (service . ,(rollback-event-service-name r))
                    (from-version . ,(rollback-event-from-version r))
                    (to-version . ,(rollback-event-to-version r))
                    (reason . ,(rollback-event-reason r))))
                rollbacks))
          (lambda (a b)
            (time<? (assoc-ref a 'time)
                   (assoc-ref b 'time))))))

(define* (find-failure-patterns db #:key (period-days 30) (min-occurrences 2))
  "Find patterns in deployment failures"
  (let* ((start-time (days-ago period-days))
         (deployments (list-deployments db #:from-time start-time))
         (failures (filter (lambda (d)
                            (eq? (deployment-event-status d) 'failure))
                          deployments)))
    (let ((patterns '()))
      ;; Group by service and deployment type
      (for-each
       (lambda (f)
         (let* ((key (cons (deployment-event-service-name f)
                          (deployment-event-deployment-type f)))
                (existing (assoc key patterns)))
           (if existing
               (set-cdr! existing (+ 1 (cdr existing)))
               (set! patterns (cons (cons key 1) patterns)))))
       failures)
      ;; Filter by minimum occurrences
      (filter (lambda (p) (>= (cdr p) min-occurrences)) patterns))))

(define* (analyze-deployment-velocity db #:key (period-days 30) (interval 'daily))
  "Analyze deployment velocity over time"
  (let* ((start-time (days-ago period-days))
         (deployments (list-deployments db #:from-time start-time))
         (interval-seconds (case interval
                            ((hourly) 3600)
                            ((daily) 86400)
                            ((weekly) 604800)
                            (else 86400)))
         (buckets (make-hash-table)))
    ;; Group deployments by interval
    (for-each
     (lambda (d)
       (let* ((time-val (time-second (deployment-event-started d)))
              (bucket (quotient time-val interval-seconds))
              (current (hashq-ref buckets bucket 0)))
         (hashq-set! buckets bucket (+ 1 current))))
     deployments)
    ;; Convert to sorted list
    (sort (hash-map->list cons buckets)
          (lambda (a b) (< (car a) (car b))))))

(define (get-service-dependencies db service-name)
  "Get dependencies for a service"
  (let ((metadata (get-service-metadata db service-name)))
    (if metadata
        (service-metadata-dependencies metadata)
        '())))

(define* (impact-analysis db service-name #:key (depth 3))
  "Analyze potential impact of deploying a service"
  (let ((all-services (list-services db))
        (impacted '()))
    ;; Find services that depend on the given service
    (let analyze ((current service-name) (level 0))
      (when (< level depth)
        (for-each
         (lambda (svc)
           (let ((deps (service-metadata-dependencies svc)))
             (when (member current deps)
               (unless (member (service-metadata-name svc) impacted)
                 (set! impacted (cons (service-metadata-name svc) impacted))
                 (analyze (service-metadata-name svc) (+ level 1))))))
         all-services)))
    impacted))

;;; Trend analysis

(define* (deployment-frequency-trend db service-name
                                      #:key (period-days 90) (bucket-days 7))
  "Calculate deployment frequency trend over time"
  (let* ((start-time (days-ago period-days))
         (deployments (list-deployments db
                                         #:service-name service-name
                                         #:from-time start-time))
         (buckets (quotient period-days bucket-days))
         (trend '()))
    (do ((i 0 (+ i 1)))
        ((>= i buckets) (reverse trend))
      (let* ((bucket-start (days-ago (- period-days (* i bucket-days))))
             (bucket-end (days-ago (- period-days (* (+ i 1) bucket-days))))
             (bucket-deployments
              (filter (lambda (d)
                        (time-in-range? (deployment-event-started d)
                                       bucket-end bucket-start))
                      deployments)))
        (set! trend (cons (cons i (length bucket-deployments)) trend))))))

(define* (service-health-score db service-name #:key (period-days 30))
  "Calculate overall health score for a service (0-100)"
  (let* ((success-rate (calculate-success-rate db service-name
                                                #:period-days period-days))
         (rollback-rate (calculate-rollback-rate db service-name
                                                 #:period-days period-days))
         (frequency (calculate-deployment-frequency db service-name
                                                    #:period-days period-days))
         (mttr-hours (/ (calculate-mttr db service-name
                                        #:period-days period-days) 3600)))
    ;; Weighted scoring
    (let* ((success-score (* 0.4 success-rate))
           (rollback-score (* 0.3 (- 100 rollback-rate)))
           (frequency-score (* 0.2 (min 100 (* frequency 10))))
           (mttr-score (* 0.1 (max 0 (- 100 (* mttr-hours 10))))))
      (+ success-score rollback-score frequency-score mttr-score))))

;;; Pattern detection

(define* (deployment-patterns db #:key (period-days 30))
  "Detect common deployment patterns"
  (let* ((start-time (days-ago period-days))
         (deployments (list-deployments db #:from-time start-time))
         (patterns '()))
    ;; Time-of-day pattern
    (let ((hour-buckets (make-vector 24 0)))
      (for-each
       (lambda (d)
         (let* ((time-val (deployment-event-started d))
                (date (time-utc->date time-val))
                (hour (date-hour date)))
           (vector-set! hour-buckets hour
                       (+ 1 (vector-ref hour-buckets hour)))))
       deployments)
      (set! patterns (cons `(hourly-distribution . ,hour-buckets) patterns)))

    ;; Day-of-week pattern
    (let ((day-buckets (make-vector 7 0)))
      (for-each
       (lambda (d)
         (let* ((time-val (deployment-event-started d))
                (date (time-utc->date time-val))
                (dow (date-week-day date)))
           (vector-set! day-buckets dow
                       (+ 1 (vector-ref day-buckets dow)))))
       deployments)
      (set! patterns (cons `(weekly-distribution . ,day-buckets) patterns)))

    ;; Deployment type preference
    (let ((type-counts (make-hash-table)))
      (for-each
       (lambda (d)
         (let* ((dtype (deployment-event-deployment-type d))
                (current (hashq-ref type-counts dtype 0)))
           (hashq-set! type-counts dtype (+ 1 current))))
       deployments)
      (set! patterns (cons `(type-preference . ,(hash-map->list cons type-counts))
                          patterns)))
    patterns))

;;; Aggregation functions

(define* (aggregate-metrics db #:key services (period-days 30))
  "Aggregate metrics across multiple services"
  (let* ((service-list (or services
                           (map service-metadata-name (list-services db))))
         (metrics '()))
    (for-each
     (lambda (service)
       (let ((m (make-deployment-metrics
                 #:service-name service
                 #:period-start (days-ago period-days)
                 #:period-end (current-time time-utc)
                 #:total-deployments (length (list-deployments db
                                                               #:service-name service
                                                               #:from-time (days-ago period-days)))
                 #:successful-deployments (length
                                          (filter (lambda (d)
                                                   (eq? (deployment-event-status d) 'success))
                                                 (list-deployments db
                                                                  #:service-name service
                                                                  #:from-time (days-ago period-days))))
                 #:failed-deployments (length
                                      (filter (lambda (d)
                                               (eq? (deployment-event-status d) 'failure))
                                             (list-deployments db
                                                              #:service-name service
                                                              #:from-time (days-ago period-days))))
                 #:rollback-count (length (list-rollbacks db
                                                         #:service-name service
                                                         #:from-time (days-ago period-days)))
                 #:average-duration (calculate-average-duration db service
                                                                #:period-days period-days)
                 #:mttr (calculate-mttr db service #:period-days period-days)
                 #:frequency (calculate-deployment-frequency db service
                                                            #:period-days period-days)
                 #:success-rate (calculate-success-rate db service
                                                        #:period-days period-days))))
         (set! metrics (cons m metrics))))
     service-list)
    (reverse metrics)))

;;; Query wrapper functions

(define* (query-deployments db criteria)
  "Query deployments with complex criteria"
  (apply list-deployments db criteria))

(define* (query-rollbacks db criteria)
  "Query rollbacks with complex criteria"
  (apply list-rollbacks db criteria))