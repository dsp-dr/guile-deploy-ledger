;;; test-metrics.scm -- Tests for query and metrics module
;;; Copyright (C) 2024 DSP-DR

(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))

(use-modules (deploy-ledger core types)
             (deploy-ledger storage sqlite)
             (deploy-ledger query metrics)
             (srfi srfi-64)
             (srfi srfi-19))

(define test-db-file "/tmp/test-metrics.db")

(define (cleanup-test-db)
  "Remove test database file"
  (when (file-exists? test-db-file)
    (delete-file test-db-file)))

(define (setup-test-data db)
  "Create test deployments for metrics testing"
  ;; Create deployments with various statuses over time
  (let ((now (current-time time-utc)))
    ;; Successful deployments
    (for-each
     (lambda (i)
       (store-deployment! db
         (make-deployment-event
          #:id (format #f "success-~a" i)
          #:service-name "metrics-service"
          #:version (format #f "1.0.~a" i)
          #:environment "production"
          #:started (make-time time-utc 0 (- (time-second now) (* i 86400)))
          #:completed (make-time time-utc 0 (- (time-second now) (- (* i 86400) 3600)))
          #:status 'success)))
     (iota 10))

    ;; Failed deployments
    (for-each
     (lambda (i)
       (store-deployment! db
         (make-deployment-event
          #:id (format #f "failure-~a" i)
          #:service-name "metrics-service"
          #:version (format #f "2.0.~a" i)
          #:environment "production"
          #:started (make-time time-utc 0 (- (time-second now) (* i 86400)))
          #:status 'failure)))
     (iota 3))

    ;; Rollback events
    (store-rollback! db
      (make-rollback-event
       #:service-name "metrics-service"
       #:from-version "2.0.0"
       #:to-version "1.9.0"
       #:reason "Performance regression"
       #:timestamp (make-time time-utc 0 (- (time-second now) 172800))))))

(test-begin "metrics")

(test-group "basic-metrics"
  (cleanup-test-db)
  (let ((db (open-deployment-db test-db-file)))
    (setup-test-data db)

    (test-assert "calculate deployment frequency"
      (let ((freq (calculate-deployment-frequency db "metrics-service" #:period-days 30)))
        (and (number? freq)
             (> freq 0))))

    (test-assert "calculate success rate"
      (let ((rate (calculate-success-rate db "metrics-service" #:period-days 30)))
        (and (number? rate)
             (>= rate 0)
             (<= rate 100))))

    (test-assert "calculate rollback rate"
      (let ((rate (calculate-rollback-rate db "metrics-service" #:period-days 30)))
        (and (number? rate)
             (>= rate 0))))

    (test-assert "calculate average duration"
      (let ((duration (calculate-average-duration db "metrics-service" #:period-days 30)))
        (and (number? duration)
             (>= duration 0))))

    (test-assert "calculate MTTR"
      (let ((mttr (calculate-mttr db "metrics-service" #:period-days 30)))
        (and (number? mttr)
             (>= mttr 0))))

    (close-deployment-db db)
    (cleanup-test-db)))

(test-group "advanced-queries"
  (cleanup-test-db)
  (let ((db (open-deployment-db test-db-file)))
    (setup-test-data db)

    (test-assert "get deployment timeline"
      (let ((timeline (get-deployment-timeline db #:service-name "metrics-service")))
        (and (list? timeline)
             (> (length timeline) 0)
             (every (lambda (event)
                     (and (assoc-ref event 'type)
                          (assoc-ref event 'time)))
                   timeline))))

    (test-assert "find failure patterns"
      (let ((patterns (find-failure-patterns db #:period-days 30)))
        (list? patterns)))

    (test-assert "analyze deployment velocity"
      (let ((velocity (analyze-deployment-velocity db #:period-days 30)))
        (and (list? velocity)
             (every pair? velocity))))

    (test-assert "service health score"
      (let ((score (service-health-score db "metrics-service")))
        (and (number? score)
             (>= score 0)
             (<= score 100))))

    (test-assert "deployment patterns detection"
      (let ((patterns (deployment-patterns db)))
        (and (list? patterns)
             (assoc-ref patterns 'hourly-distribution)
             (assoc-ref patterns 'weekly-distribution))))

    (close-deployment-db db)
    (cleanup-test-db)))

(test-group "aggregation"
  (cleanup-test-db)
  (let ((db (open-deployment-db test-db-file)))
    ;; Create data for multiple services
    (for-each
     (lambda (service)
       (for-each
        (lambda (i)
          (store-deployment! db
            (make-deployment-event
             #:service-name service
             #:version (format #f "1.0.~a" i)
             #:environment "production"
             #:status (if (zero? (modulo i 3)) 'failure 'success))))
        (iota 5)))
     '("service-a" "service-b" "service-c"))

    (test-assert "aggregate metrics across services"
      (let ((metrics (aggregate-metrics db #:period-days 30)))
        (and (list? metrics)
             (every deployment-metrics? metrics))))

    (test-assert "aggregate specific services"
      (let ((metrics (aggregate-metrics db
                      #:services '("service-a" "service-b")
                      #:period-days 30)))
        (= (length metrics) 2)))

    (close-deployment-db db)
    (cleanup-test-db)))

(test-group "time-utilities"
  (test-assert "days-ago calculation"
    (let* ((now (current-time time-utc))
           (past (days-ago 7))
           (diff (- (time-second now) (time-second past))))
      (and (> diff (* 6 86400))
           (< diff (* 8 86400)))))

  (test-assert "hours-ago calculation"
    (let* ((now (current-time time-utc))
           (past (hours-ago 12))
           (diff (- (time-second now) (time-second past))))
      (and (> diff (* 11 3600))
           (< diff (* 13 3600)))))

  (test-assert "time-in-range check"
    (let* ((now (current-time time-utc))
           (start (days-ago 7))
           (end (days-ago 1))
           (middle (days-ago 4)))
      (and (time-in-range? middle start end)
           (not (time-in-range? now start end))))))

(test-end "metrics")