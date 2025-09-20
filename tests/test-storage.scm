;;; test-storage.scm -- Tests for SQLite storage backend
;;; Copyright (C) 2024 DSP-DR

(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))

(use-modules (deploy-ledger core types)
             (deploy-ledger storage sqlite)
             (srfi srfi-64)
             (srfi srfi-19)
             (ice-9 ftw))

(define test-db-file "/tmp/test-deployments.db")

(define (cleanup-test-db)
  "Remove test database file"
  (when (file-exists? test-db-file)
    (delete-file test-db-file)))

(test-begin "storage-sqlite")

(test-group "database-operations"
  (cleanup-test-db)

  (test-assert "open and initialize database"
    (let ((db (open-deployment-db test-db-file)))
      (and db
           (begin
             (close-deployment-db db)
             #t))))

  (test-assert "store and retrieve deployment"
    (let* ((db (open-deployment-db test-db-file))
           (deployment (make-deployment-event
                       #:id "test-deploy-1"
                       #:service-name "test-service"
                       #:version "1.0.0"
                       #:environment "staging"
                       #:deployment-type 'rolling
                       #:status 'success)))
      (store-deployment! db deployment)
      (let ((retrieved (get-deployment db "test-deploy-1")))
        (close-deployment-db db)
        (and retrieved
             (string=? (deployment-event-id retrieved) "test-deploy-1")
             (string=? (deployment-event-service-name retrieved) "test-service")
             (string=? (deployment-event-version retrieved) "1.0.0")))))

  (test-assert "store and retrieve rollback"
    (let* ((db (open-deployment-db test-db-file))
           (rollback (make-rollback-event
                     #:id "test-rollback-1"
                     #:service-name "test-service"
                     #:from-version "2.0.0"
                     #:to-version "1.9.0"
                     #:reason "Test rollback")))
      (store-rollback! db rollback)
      (let ((retrieved (get-rollback db "test-rollback-1")))
        (close-deployment-db db)
        (and retrieved
             (string=? (rollback-event-id retrieved) "test-rollback-1")
             (string=? (rollback-event-reason retrieved) "Test rollback")))))

  (test-assert "list deployments with filters"
    (let ((db (open-deployment-db test-db-file)))
      ;; Add multiple deployments
      (for-each
       (lambda (i)
         (store-deployment! db
           (make-deployment-event
            #:id (format #f "deploy-~a" i)
            #:service-name (if (even? i) "service-a" "service-b")
            #:version (format #f "1.0.~a" i)
            #:environment (if (< i 5) "staging" "production")
            #:status (if (zero? (modulo i 3)) 'failure 'success))))
       (iota 10))

      ;; Test filtering by service
      (let ((service-a-deploys (list-deployments db #:service-name "service-a")))
        (test-equal "filter by service" 5 (length service-a-deploys)))

      ;; Test filtering by environment
      (let ((prod-deploys (list-deployments db #:environment "production")))
        (test-equal "filter by environment" 5 (length prod-deploys)))

      ;; Test filtering by status
      (let ((failed-deploys (list-deployments db #:status 'failure)))
        (test-equal "filter by status" 4 (length failed-deploys)))

      (close-deployment-db db)))

  (test-assert "update deployment status"
    (let* ((db (open-deployment-db test-db-file))
           (deployment (make-deployment-event
                       #:id "update-test-1"
                       #:service-name "update-service"
                       #:version "1.0.0"
                       #:environment "staging"
                       #:status 'in-progress)))
      (store-deployment! db deployment)
      (update-deployment-status! db "update-test-1" 'success (current-time time-utc))
      (let ((updated (get-deployment db "update-test-1")))
        (close-deployment-db db)
        (and updated
             (eq? (deployment-event-status updated) 'success)
             (deployment-event-completed updated)))))

  (test-assert "service metadata operations"
    (let* ((db (open-deployment-db test-db-file))
           (metadata (make-service-metadata
                     #:name "metadata-service"
                     #:type 'microservice
                     #:dependencies '("dep1" "dep2")
                     #:owners '("team-a")
                     #:repository "https://github.com/org/repo")))
      (store-service-metadata! db metadata)
      (let ((retrieved (get-service-metadata db "metadata-service")))
        (close-deployment-db db)
        (and retrieved
             (string=? (service-metadata-name retrieved) "metadata-service")
             (eq? (service-metadata-type retrieved) 'microservice)
             (equal? (service-metadata-dependencies retrieved) '("dep1" "dep2"))))))

  (test-assert "transaction support"
    (let ((db (open-deployment-db test-db-file)))
      (catch #t
        (lambda ()
          (with-transaction db
            (lambda ()
              (store-deployment! db
                (make-deployment-event
                 #:id "tx-test-1"
                 #:service-name "tx-service"
                 #:version "1.0.0"
                 #:environment "staging"))
              ;; Force an error to test rollback
              (error "Test transaction rollback"))))
        (lambda args #t))
      ;; Verify the deployment was not stored due to rollback
      (let ((result (get-deployment db "tx-test-1")))
        (close-deployment-db db)
        (not result))))

  (cleanup-test-db))

(test-end "storage-sqlite")