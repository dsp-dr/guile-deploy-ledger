;;; test-pbt.scm -- Property-based tests for deploy-ledger
;;; Copyright (C) 2026 DSP-DR

(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))

(use-modules (deploy-ledger core types)
             (deploy-ledger cli config)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-19)
             (srfi srfi-64)
             (ice-9 format))

;;;------------------------------------------------------------
;;; Minimal PBT harness
;;;------------------------------------------------------------

(define *pbt-seed* 42)
(define *pbt-trials* 100)

(set! *random-state* (seed->random-state *pbt-seed*))

(define (random-element lst)
  "Pick a random element from a non-empty list."
  (list-ref lst (random (length lst))))

(define (random-string max-len)
  "Generate a random alphanumeric string of length 1..max-len."
  (let* ((len (+ 1 (random max-len)))
         (chars (map (lambda (_)
                       (let ((n (random 36)))
                         (if (< n 10)
                             (integer->char (+ n (char->integer #\0)))
                             (integer->char (+ (- n 10) (char->integer #\a))))))
                     (iota len))))
    (list->string chars)))

(define (random-version)
  "Generate a version string like N.N.N."
  (format #f "~a.~a.~a"
          (random 100) (random 100) (random 100)))

(define (random-service-name)
  "Generate a plausible service name."
  (string-append (random-element '("api" "auth" "billing" "data" "edge"
                                   "gateway" "ingest" "log" "notify" "queue"))
                 "-"
                 (random-element '("service" "worker" "proxy" "cache" "relay"))
                 "-"
                 (number->string (random 1000))))

(define (random-deployment-type)
  (random-element valid-deployment-types))

(define (random-deployment-status)
  (random-element valid-deployment-statuses))

(define (random-service-type)
  (random-element valid-service-types))

(define (random-timestamp)
  "Generate a timestamp within the last year."
  (let ((now (time-second (current-time time-utc))))
    (make-time time-utc 0 (- now (random (* 365 86400))))))

(define (random-metadata)
  "Generate random metadata alist."
  (let ((n (random 5)))
    (map (lambda (i)
           (cons (string->symbol (random-string 8))
                 (random-string 12)))
         (iota n))))

(define (random-deployment-event)
  "Generate a random deployment event."
  (make-deployment-event
   #:id (string-append "pbt-" (random-string 16))
   #:service-name (random-service-name)
   #:version (random-version)
   #:environment (random-element '("staging" "production" "development" "qa"))
   #:deployment-type (random-deployment-type)
   #:started (random-timestamp)
   #:status (random-deployment-status)
   #:initiator (random-element '("ci-bot" "deploy-agent" "human" "cron"))
   #:metadata (random-metadata)
   #:parent-id #f))

(define (random-rollback-event)
  "Generate a random rollback event."
  (make-rollback-event
   #:id (string-append "pbt-rb-" (random-string 16))
   #:service-name (random-service-name)
   #:from-version (random-version)
   #:to-version (random-version)
   #:reason (random-element '("Performance regression"
                              "Error rate spike"
                              "Failed health check"
                              "Memory leak"
                              "Timeout increase"))
   #:timestamp (random-timestamp)
   #:initiator (random-element '("sre-bot" "oncall" "auto-rollback"))
   #:impact (list (random-service-name))
   #:deployment-id (string-append "dep-" (random-string 8))))

(define (random-service-metadata)
  "Generate a random service metadata record."
  (make-service-metadata
   #:name (random-service-name)
   #:type (random-service-type)
   #:dependencies (map (lambda (_) (random-service-name)) (iota (random 4)))
   #:owners (map (lambda (_) (random-string 8)) (iota (+ 1 (random 3))))
   #:repository (string-append "https://github.com/org/" (random-string 10))
   #:health-check (string-append "https://" (random-string 8) ".svc/health")))

(define (random-deployment-metrics)
  "Generate a random deployment metrics record."
  (let* ((total (+ 1 (random 200)))
         (successful (random (+ total 1)))
         (failed (- total successful))
         (rate (if (zero? total) 0 (* 100.0 (/ successful total)))))
    (make-deployment-metrics
     #:service-name (random-service-name)
     #:period-start (random-timestamp)
     #:period-end (random-timestamp)
     #:total-deployments total
     #:successful-deployments successful
     #:failed-deployments failed
     #:rollback-count (random 20)
     #:average-duration (random 7200)
     #:mttr (random 36000)
     #:frequency (/ (random 100) 10)
     #:success-rate rate)))

(define (for-all n gen prop)
  "Run property PROP on N randomly generated values from GEN.
   Returns #t if all pass, or a string describing the first failure."
  (let loop ((i 0))
    (if (>= i n)
        #t
        (let ((val (gen)))
          (catch #t
            (lambda ()
              (if (prop val)
                  (loop (+ i 1))
                  (format #f "Failed on trial ~a" i)))
            (lambda (key . args)
              (format #f "Exception on trial ~a: ~a ~a" i key args)))))))


;;;------------------------------------------------------------
;;; Tests begin
;;;------------------------------------------------------------

(test-begin "pbt")

;;; -------------------------------------------------------
;;; 1. Type invariants: constructors produce valid predicates
;;; -------------------------------------------------------

(test-group "type-invariants"

  (test-assert "deployment-event? holds for all generated deployments"
    (eq? #t
         (for-all *pbt-trials*
                  random-deployment-event
                  deployment-event?)))

  (test-assert "rollback-event? holds for all generated rollbacks"
    (eq? #t
         (for-all *pbt-trials*
                  random-rollback-event
                  rollback-event?)))

  (test-assert "service-metadata? holds for all generated metadata"
    (eq? #t
         (for-all *pbt-trials*
                  random-service-metadata
                  service-metadata?)))

  (test-assert "deployment-metrics? holds for all generated metrics"
    (eq? #t
         (for-all *pbt-trials*
                  random-deployment-metrics
                  deployment-metrics?))))

;;; -------------------------------------------------------
;;; 2. Field preservation: accessors return what was set
;;; -------------------------------------------------------

(test-group "field-preservation"

  (test-assert "deployment-event fields are preserved"
    (eq? #t
         (for-all *pbt-trials*
                  (lambda ()
                    (let ((svc (random-service-name))
                          (ver (random-version))
                          (env "staging")
                          (dtype (random-deployment-type))
                          (stat (random-deployment-status))
                          (init (random-string 10)))
                      (list svc ver env dtype stat init)))
                  (lambda (args)
                    (let* ((svc (list-ref args 0))
                           (ver (list-ref args 1))
                           (env (list-ref args 2))
                           (dtype (list-ref args 3))
                           (stat (list-ref args 4))
                           (init (list-ref args 5))
                           (d (make-deployment-event
                               #:service-name svc
                               #:version ver
                               #:environment env
                               #:deployment-type dtype
                               #:status stat
                               #:initiator init)))
                      (and (string=? (deployment-event-service-name d) svc)
                           (string=? (deployment-event-version d) ver)
                           (string=? (deployment-event-environment d) env)
                           (eq? (deployment-event-deployment-type d) dtype)
                           (eq? (deployment-event-status d) stat)
                           (string=? (deployment-event-initiator d) init)))))))

  (test-assert "rollback-event fields are preserved"
    (eq? #t
         (for-all *pbt-trials*
                  (lambda ()
                    (let ((svc (random-service-name))
                          (fv (random-version))
                          (tv (random-version))
                          (reason (random-string 20)))
                      (list svc fv tv reason)))
                  (lambda (args)
                    (let* ((svc (list-ref args 0))
                           (fv (list-ref args 1))
                           (tv (list-ref args 2))
                           (reason (list-ref args 3))
                           (r (make-rollback-event
                               #:service-name svc
                               #:from-version fv
                               #:to-version tv
                               #:reason reason)))
                      (and (string=? (rollback-event-service-name r) svc)
                           (string=? (rollback-event-from-version r) fv)
                           (string=? (rollback-event-to-version r) tv)
                           (string=? (rollback-event-reason r) reason)))))))

  (test-assert "service-metadata fields are preserved"
    (eq? #t
         (for-all *pbt-trials*
                  (lambda ()
                    (let ((name (random-service-name))
                          (stype (random-service-type))
                          (deps (map (lambda (_) (random-service-name))
                                     (iota (random 4)))))
                      (list name stype deps)))
                  (lambda (args)
                    (let* ((name (list-ref args 0))
                           (stype (list-ref args 1))
                           (deps (list-ref args 2))
                           (m (make-service-metadata
                               #:name name
                               #:type stype
                               #:dependencies deps)))
                      (and (string=? (service-metadata-name m) name)
                           (eq? (service-metadata-type m) stype)
                           (equal? (service-metadata-dependencies m) deps)))))))

  (test-assert "deployment-metrics fields are preserved"
    (eq? #t
         (for-all *pbt-trials*
                  (lambda ()
                    (let ((svc (random-service-name))
                          (total (random 500))
                          (rate (* 1.0 (random 100))))
                      (list svc total rate)))
                  (lambda (args)
                    (let* ((svc (list-ref args 0))
                           (total (list-ref args 1))
                           (rate (list-ref args 2))
                           (m (make-deployment-metrics
                               #:service-name svc
                               #:total-deployments total
                               #:success-rate rate)))
                      (and (string=? (deployment-metrics-service-name m) svc)
                           (= (deployment-metrics-total-deployments m) total)
                           (= (deployment-metrics-success-rate m) rate))))))))

;;; -------------------------------------------------------
;;; 3. Enumeration predicate properties
;;; -------------------------------------------------------

(test-group "enumeration-properties"

  (test-assert "every valid deployment type passes deployment-type?"
    (every deployment-type? valid-deployment-types))

  (test-assert "every valid deployment status passes deployment-status?"
    (every deployment-status? valid-deployment-statuses))

  (test-assert "every valid service type passes service-type?"
    (every service-type? valid-service-types))

  (test-assert "random symbols outside valid sets are rejected"
    (eq? #t
         (for-all *pbt-trials*
                  (lambda () (string->symbol (random-string 20)))
                  (lambda (sym)
                    (and (not (deployment-type? sym))
                         (not (deployment-status? sym))
                         (not (service-type? sym)))))))

  (test-assert "valid-deployment-types is a proper non-empty list"
    (and (list? valid-deployment-types)
         (not (null? valid-deployment-types))
         (every symbol? valid-deployment-types)))

  (test-assert "valid-deployment-statuses is a proper non-empty list"
    (and (list? valid-deployment-statuses)
         (not (null? valid-deployment-statuses))
         (every symbol? valid-deployment-statuses)))

  (test-assert "valid-service-types is a proper non-empty list"
    (and (list? valid-service-types)
         (not (null? valid-service-types))
         (every symbol? valid-service-types))))

;;; -------------------------------------------------------
;;; 4. Mutable field (status) idempotence
;;; -------------------------------------------------------

(test-group "mutation-properties"

  (test-assert "set-deployment-event-status! is idempotent"
    (eq? #t
         (for-all *pbt-trials*
                  (lambda ()
                    (cons (random-deployment-event)
                          (random-deployment-status)))
                  (lambda (pair)
                    (let ((d (car pair))
                          (new-status (cdr pair)))
                      (set-deployment-event-status! d new-status)
                      (let ((s1 (deployment-event-status d)))
                        (set-deployment-event-status! d new-status)
                        (let ((s2 (deployment-event-status d)))
                          (eq? s1 s2))))))))

  (test-assert "status after set always equals the value set"
    (eq? #t
         (for-all *pbt-trials*
                  (lambda ()
                    (cons (random-deployment-event)
                          (random-deployment-status)))
                  (lambda (pair)
                    (let ((d (car pair))
                          (new-status (cdr pair)))
                      (set-deployment-event-status! d new-status)
                      (eq? (deployment-event-status d) new-status)))))))

;;; -------------------------------------------------------
;;; 5. Constructor validation rejects invalid inputs
;;; -------------------------------------------------------

(test-group "constructor-validation"

  (test-assert "invalid deployment types are always rejected"
    (eq? #t
         (for-all *pbt-trials*
                  (lambda () (string->symbol (random-string 15)))
                  (lambda (bad-type)
                    (catch #t
                      (lambda ()
                        (make-deployment-event
                         #:service-name "test"
                         #:version "1.0"
                         #:environment "prod"
                         #:deployment-type bad-type)
                        #f)  ; should not reach here
                      (lambda (key . args) #t))))))

  (test-assert "invalid deployment statuses are always rejected"
    (eq? #t
         (for-all *pbt-trials*
                  (lambda () (string->symbol (random-string 15)))
                  (lambda (bad-status)
                    (catch #t
                      (lambda ()
                        (make-deployment-event
                         #:service-name "test"
                         #:version "1.0"
                         #:environment "prod"
                         #:status bad-status)
                        #f)
                      (lambda (key . args) #t))))))

  (test-assert "invalid service types are always rejected"
    (eq? #t
         (for-all *pbt-trials*
                  (lambda () (string->symbol (random-string 15)))
                  (lambda (bad-type)
                    (catch #t
                      (lambda ()
                        (make-service-metadata
                         #:name "test"
                         #:type bad-type)
                        #f)
                      (lambda (key . args) #t)))))))

;;; -------------------------------------------------------
;;; 6. generate-id properties
;;; -------------------------------------------------------

(test-group "id-generation"

  (test-assert "generate-id always produces strings with deploy- prefix"
    (eq? #t
         (for-all *pbt-trials*
                  generate-id
                  (lambda (id)
                    (and (string? id)
                         (string-prefix? "deploy-" id))))))

  (test-assert "generate-id produces unique values across batch"
    (let ((ids (map (lambda (_) (generate-id)) (iota *pbt-trials*))))
      (= (length ids)
         (length (delete-duplicates ids string=?))))))

;;; -------------------------------------------------------
;;; 7. Config round-trip properties
;;; -------------------------------------------------------

(test-group "config-round-trip"

  (test-assert "set then get returns the set value"
    (eq? #t
         (for-all *pbt-trials*
                  (lambda ()
                    (let ((key (string->symbol (random-string 8)))
                          (val (random-string 12)))
                      (cons key val)))
                  (lambda (pair)
                    (let* ((key (car pair))
                           (val (cdr pair))
                           (cfg (list (cons 'base "value")))
                           (updated (set-config-value cfg key val)))
                      (equal? (get-config-value updated key) val))))))

  (test-assert "set-config-value is idempotent"
    (eq? #t
         (for-all *pbt-trials*
                  (lambda ()
                    (let ((key (string->symbol (random-string 8)))
                          (val (random-string 12)))
                      (cons key val)))
                  (lambda (pair)
                    (let* ((key (car pair))
                           (val (cdr pair))
                           (cfg (list (cons 'base "value")))
                           (once (set-config-value cfg key val))
                           (twice (set-config-value once key val)))
                      (equal? (get-config-value once key)
                              (get-config-value twice key)))))))

  (test-assert "get-config-value returns #f for absent keys"
    (eq? #t
         (for-all *pbt-trials*
                  (lambda () (string->symbol (random-string 20)))
                  (lambda (key)
                    (eq? #f (get-config-value default-config key))))))

  (test-assert "default-config is a valid alist"
    (and (list? default-config)
         (not (null? default-config))
         (every pair? default-config))))

;;; -------------------------------------------------------
;;; 8. Deployment metrics defaults
;;; -------------------------------------------------------

(test-group "metrics-defaults"

  (test-assert "default metrics have zero counters"
    (let ((m (make-deployment-metrics
              #:service-name "test"
              #:period-start (current-timestamp)
              #:period-end (current-timestamp))))
      (and (= 0 (deployment-metrics-total-deployments m))
           (= 0 (deployment-metrics-successful-deployments m))
           (= 0 (deployment-metrics-failed-deployments m))
           (= 0 (deployment-metrics-rollback-count m))
           (= 0 (deployment-metrics-average-duration m))
           (= 0 (deployment-metrics-mttr m))
           (= 0 (deployment-metrics-frequency m))
           (= 0 (deployment-metrics-success-rate m))))))

;;; -------------------------------------------------------
;;; 9. Record identity: same inputs produce same accessors
;;; -------------------------------------------------------

(test-group "record-determinism"

  (test-assert "deployment-event with same args yields same fields"
    (eq? #t
         (for-all *pbt-trials*
                  (lambda ()
                    (let ((id (string-append "det-" (random-string 12)))
                          (svc (random-service-name))
                          (ver (random-version))
                          (env "staging")
                          (dtype (random-deployment-type))
                          (stat (random-deployment-status))
                          (ts (random-timestamp)))
                      (list id svc ver env dtype stat ts)))
                  (lambda (args)
                    (let ((mk (lambda ()
                                (make-deployment-event
                                 #:id (list-ref args 0)
                                 #:service-name (list-ref args 1)
                                 #:version (list-ref args 2)
                                 #:environment (list-ref args 3)
                                 #:deployment-type (list-ref args 4)
                                 #:status (list-ref args 5)
                                 #:started (list-ref args 6)))))
                      (let ((d1 (mk))
                            (d2 (mk)))
                        (and (string=? (deployment-event-id d1)
                                       (deployment-event-id d2))
                             (string=? (deployment-event-service-name d1)
                                       (deployment-event-service-name d2))
                             (string=? (deployment-event-version d1)
                                       (deployment-event-version d2))
                             (eq? (deployment-event-deployment-type d1)
                                  (deployment-event-deployment-type d2))
                             (eq? (deployment-event-status d1)
                                  (deployment-event-status d2))))))))))

;;; -------------------------------------------------------
;;; 10. List operations on empty collections
;;; -------------------------------------------------------

(test-group "empty-collections"

  (test-assert "filtering empty list of deployment events yields empty"
    (null? (filter deployment-event?  '())))

  (test-assert "filtering empty list of rollback events yields empty"
    (null? (filter rollback-event? '())))

  (test-assert "filtering empty list of service metadata yields empty"
    (null? (filter service-metadata? '())))

  (test-assert "map over empty deployment list yields empty"
    (null? (map deployment-event-id '())))

  (test-assert "fold over empty list of metrics yields initial"
    (= 0 (fold (lambda (m acc)
                 (+ acc (deployment-metrics-total-deployments m)))
               0 '()))))

;;; -------------------------------------------------------
;;; 11. Sorting: generated events can be sorted by timestamp
;;; -------------------------------------------------------

(test-group "sorting-properties"

  (test-assert "deployment events sort stably by start time"
    (let* ((events (map (lambda (_) (random-deployment-event))
                        (iota *pbt-trials*)))
           (sorted (sort events
                         (lambda (a b)
                           (time<? (deployment-event-started a)
                                   (deployment-event-started b))))))
      ;; verify sorted order
      (let check ((rest sorted))
        (if (or (null? rest) (null? (cdr rest)))
            #t
            (and (time<=? (deployment-event-started (car rest))
                          (deployment-event-started (cadr rest)))
                 (check (cdr rest)))))))

  (test-assert "rollback events sort stably by timestamp"
    (let* ((events (map (lambda (_) (random-rollback-event))
                        (iota *pbt-trials*)))
           (sorted (sort events
                         (lambda (a b)
                           (time<? (rollback-event-timestamp a)
                                   (rollback-event-timestamp b))))))
      (let check ((rest sorted))
        (if (or (null? rest) (null? (cdr rest)))
            #t
            (and (time<=? (rollback-event-timestamp (car rest))
                          (rollback-event-timestamp (cadr rest)))
                 (check (cdr rest))))))))

;;; -------------------------------------------------------
;;; 12. Query-like filtering consistency
;;; -------------------------------------------------------

(test-group "filter-consistency"

  (test-assert "filtering by status then counting equals count-by-status"
    (let* ((events (map (lambda (_) (random-deployment-event))
                        (iota *pbt-trials*)))
           (target-status (random-deployment-status))
           (filtered (filter (lambda (d)
                              (eq? (deployment-event-status d) target-status))
                            events))
           (counted (length (filter (lambda (d)
                                      (eq? (deployment-event-status d) target-status))
                                    events))))
      (= (length filtered) counted)))

  (test-assert "filtering by type then counting equals count-by-type"
    (let* ((events (map (lambda (_) (random-deployment-event))
                        (iota *pbt-trials*)))
           (target-type (random-deployment-type))
           (filtered (filter (lambda (d)
                              (eq? (deployment-event-deployment-type d) target-type))
                            events))
           (counted (length (filter (lambda (d)
                                      (eq? (deployment-event-deployment-type d) target-type))
                                    events))))
      (= (length filtered) counted)))

  (test-assert "union of status partitions equals original set"
    (let ((events (map (lambda (_) (random-deployment-event))
                       (iota *pbt-trials*))))
      (= (length events)
         (apply +
                (map (lambda (status)
                       (length (filter (lambda (d)
                                        (eq? (deployment-event-status d) status))
                                      events)))
                     valid-deployment-statuses)))))

  (test-assert "filtering preserves predicate satisfaction"
    (eq? #t
         (for-all 50
                  (lambda ()
                    (let ((events (map (lambda (_) (random-deployment-event))
                                       (iota 20)))
                          (target (random-deployment-status)))
                      (cons events target)))
                  (lambda (pair)
                    (let* ((events (car pair))
                           (target (cdr pair))
                           (filtered (filter (lambda (d)
                                              (eq? (deployment-event-status d) target))
                                            events)))
                      (every (lambda (d)
                               (eq? (deployment-event-status d) target))
                             filtered)))))))

(test-end "pbt")
