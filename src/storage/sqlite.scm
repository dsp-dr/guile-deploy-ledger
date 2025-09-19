;;; sqlite.scm -- SQLite storage backend for deployment ledger
;;; Copyright (C) 2024 DSP-DR
;;;
;;; This file is part of Guile Deploy Ledger.

(define-module (deploy-ledger storage sqlite)
  #:use-module (deploy-ledger core types)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (sqlite3)
  #:export (open-deployment-db
            close-deployment-db
            init-database!
            store-deployment!
            store-rollback!
            store-service-metadata!
            get-deployment
            get-rollback
            get-service-metadata
            list-deployments
            list-rollbacks
            list-services
            update-deployment-status!
            delete-deployment!
            delete-rollback!
            with-transaction
            vacuum-database!))

;;; Database connection management

(define (open-deployment-db filename)
  "Open or create a SQLite database for deployments"
  (let ((db (sqlite-open filename)))
    (sqlite-exec db "PRAGMA foreign_keys = ON;")
    (sqlite-exec db "PRAGMA journal_mode = WAL;")
    (init-database! db)
    db))

(define (close-deployment-db db)
  "Close the deployment database connection"
  (sqlite-close db))

;;; Schema initialization

(define (init-database! db)
  "Initialize database schema if not exists"
  (sqlite-exec db "
    CREATE TABLE IF NOT EXISTS deployments (
      id TEXT PRIMARY KEY,
      service_name TEXT NOT NULL,
      version TEXT NOT NULL,
      environment TEXT NOT NULL,
      deployment_type TEXT NOT NULL,
      started INTEGER NOT NULL,
      completed INTEGER,
      status TEXT NOT NULL,
      initiator TEXT NOT NULL,
      metadata TEXT,
      parent_id TEXT,
      created_at INTEGER DEFAULT (strftime('%s', 'now')),
      FOREIGN KEY (parent_id) REFERENCES deployments(id)
    );

    CREATE INDEX IF NOT EXISTS idx_deployments_service
      ON deployments(service_name);
    CREATE INDEX IF NOT EXISTS idx_deployments_environment
      ON deployments(environment);
    CREATE INDEX IF NOT EXISTS idx_deployments_started
      ON deployments(started);
    CREATE INDEX IF NOT EXISTS idx_deployments_status
      ON deployments(status);

    CREATE TABLE IF NOT EXISTS rollbacks (
      id TEXT PRIMARY KEY,
      service_name TEXT NOT NULL,
      from_version TEXT NOT NULL,
      to_version TEXT NOT NULL,
      reason TEXT NOT NULL,
      timestamp INTEGER NOT NULL,
      initiator TEXT NOT NULL,
      impact TEXT,
      deployment_id TEXT,
      created_at INTEGER DEFAULT (strftime('%s', 'now')),
      FOREIGN KEY (deployment_id) REFERENCES deployments(id)
    );

    CREATE INDEX IF NOT EXISTS idx_rollbacks_service
      ON rollbacks(service_name);
    CREATE INDEX IF NOT EXISTS idx_rollbacks_timestamp
      ON rollbacks(timestamp);

    CREATE TABLE IF NOT EXISTS service_metadata (
      name TEXT PRIMARY KEY,
      type TEXT NOT NULL,
      dependencies TEXT,
      owners TEXT,
      repository TEXT,
      health_check TEXT,
      created INTEGER NOT NULL,
      updated INTEGER NOT NULL
    );

    CREATE TABLE IF NOT EXISTS deployment_events (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      deployment_id TEXT NOT NULL,
      event_type TEXT NOT NULL,
      event_data TEXT,
      timestamp INTEGER DEFAULT (strftime('%s', 'now')),
      FOREIGN KEY (deployment_id) REFERENCES deployments(id)
    );

    CREATE INDEX IF NOT EXISTS idx_events_deployment
      ON deployment_events(deployment_id);
    CREATE INDEX IF NOT EXISTS idx_events_timestamp
      ON deployment_events(timestamp);
  "))

;;; Time conversion utilities

(define (time->unix time-obj)
  "Convert SRFI-19 time to Unix timestamp"
  (if time-obj
      (time-second time-obj)
      0))

(define (unix->time unix-timestamp)
  "Convert Unix timestamp to SRFI-19 time"
  (if (and unix-timestamp (> unix-timestamp 0))
      (make-time time-utc 0 unix-timestamp)
      #f))

;;; Serialization utilities

(define (alist->string alist)
  "Convert association list to string for storage"
  (if (null? alist)
      ""
      (format #f "~s" alist)))

(define (string->alist str)
  "Convert stored string back to association list"
  (if (or (not str) (string-null? str))
      '()
      (with-input-from-string str read)))

(define (list->string lst)
  "Convert list to string for storage"
  (if (null? lst)
      ""
      (format #f "~s" lst)))

(define (string->list str)
  "Convert stored string back to list"
  (if (or (not str) (string-null? str))
      '()
      (with-input-from-string str read)))

;;; Deployment operations

(define (store-deployment! db deployment)
  "Store a deployment event in the database"
  (let ((stmt (sqlite-prepare db "
    INSERT INTO deployments
    (id, service_name, version, environment, deployment_type,
     started, completed, status, initiator, metadata, parent_id)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")))
    (sqlite-bind stmt 1 (deployment-event-id deployment))
    (sqlite-bind stmt 2 (deployment-event-service-name deployment))
    (sqlite-bind stmt 3 (deployment-event-version deployment))
    (sqlite-bind stmt 4 (deployment-event-environment deployment))
    (sqlite-bind stmt 5 (symbol->string (deployment-event-deployment-type deployment)))
    (sqlite-bind stmt 6 (time->unix (deployment-event-started deployment)))
    (sqlite-bind stmt 7 (time->unix (deployment-event-completed deployment)))
    (sqlite-bind stmt 8 (symbol->string (deployment-event-status deployment)))
    (sqlite-bind stmt 9 (deployment-event-initiator deployment))
    (sqlite-bind stmt 10 (alist->string (deployment-event-metadata deployment)))
    (sqlite-bind stmt 11 (deployment-event-parent-id deployment))
    (sqlite-step stmt)
    (sqlite-finalize stmt)
    (deployment-event-id deployment)))

(define (get-deployment db id)
  "Retrieve a deployment by ID"
  (let ((stmt (sqlite-prepare db "
    SELECT * FROM deployments WHERE id = ?")))
    (sqlite-bind stmt 1 id)
    (let ((result (sqlite-step stmt)))
      (let ((deployment
             (if (sqlite-done? stmt)
                 #f
                 (make-deployment-event
                  #:id (sqlite-column stmt 0)
                  #:service-name (sqlite-column stmt 1)
                  #:version (sqlite-column stmt 2)
                  #:environment (sqlite-column stmt 3)
                  #:deployment-type (string->symbol (sqlite-column stmt 4))
                  #:started (unix->time (sqlite-column stmt 5))
                  #:completed (unix->time (sqlite-column stmt 6))
                  #:status (string->symbol (sqlite-column stmt 7))
                  #:initiator (sqlite-column stmt 8)
                  #:metadata (string->alist (sqlite-column stmt 9))
                  #:parent-id (sqlite-column stmt 10)))))
        (sqlite-finalize stmt)
        deployment))))

(define (update-deployment-status! db id status #:optional completed)
  "Update deployment status and optionally completion time"
  (let ((stmt (sqlite-prepare db
                (if completed
                    "UPDATE deployments SET status = ?, completed = ? WHERE id = ?"
                    "UPDATE deployments SET status = ? WHERE id = ?"))))
    (sqlite-bind stmt 1 (symbol->string status))
    (if completed
        (begin
          (sqlite-bind stmt 2 (time->unix completed))
          (sqlite-bind stmt 3 id))
        (sqlite-bind stmt 2 id))
    (sqlite-step stmt)
    (sqlite-finalize stmt)))

(define* (list-deployments db #:key service-name environment status
                           from-time to-time limit offset)
  "List deployments with optional filters"
  (let* ((conditions '())
         (params '())
         (query-base "SELECT * FROM deployments WHERE 1=1"))

    (when service-name
      (set! conditions (cons "service_name = ?" conditions))
      (set! params (cons service-name params)))

    (when environment
      (set! conditions (cons "environment = ?" conditions))
      (set! params (cons environment params)))

    (when status
      (set! conditions (cons "status = ?" conditions))
      (set! params (cons (symbol->string status) params)))

    (when from-time
      (set! conditions (cons "started >= ?" conditions))
      (set! params (cons (time->unix from-time) params)))

    (when to-time
      (set! conditions (cons "started <= ?" conditions))
      (set! params (cons (time->unix to-time) params)))

    (let* ((where-clause
            (if (null? conditions)
                ""
                (string-append " AND " (string-join (reverse conditions) " AND "))))
           (order-clause " ORDER BY started DESC")
           (limit-clause
            (if limit
                (format #f " LIMIT ~a OFFSET ~a" limit (or offset 0))
                ""))
           (query (string-append query-base where-clause order-clause limit-clause))
           (stmt (sqlite-prepare db query)))

      ;; Bind parameters in reverse order
      (let loop ((params (reverse params))
                 (index 1))
        (unless (null? params)
          (sqlite-bind stmt index (car params))
          (loop (cdr params) (+ index 1))))

      ;; Collect results
      (let collect ((deployments '()))
        (if (sqlite-done? stmt)
            (begin
              (sqlite-finalize stmt)
              (reverse deployments))
            (let ((deployment
                   (make-deployment-event
                    #:id (sqlite-column stmt 0)
                    #:service-name (sqlite-column stmt 1)
                    #:version (sqlite-column stmt 2)
                    #:environment (sqlite-column stmt 3)
                    #:deployment-type (string->symbol (sqlite-column stmt 4))
                    #:started (unix->time (sqlite-column stmt 5))
                    #:completed (unix->time (sqlite-column stmt 6))
                    #:status (string->symbol (sqlite-column stmt 7))
                    #:initiator (sqlite-column stmt 8)
                    #:metadata (string->alist (sqlite-column stmt 9))
                    #:parent-id (sqlite-column stmt 10))))
              (sqlite-step stmt)
              (collect (cons deployment deployments))))))))

;;; Rollback operations

(define (store-rollback! db rollback)
  "Store a rollback event in the database"
  (let ((stmt (sqlite-prepare db "
    INSERT INTO rollbacks
    (id, service_name, from_version, to_version, reason,
     timestamp, initiator, impact, deployment_id)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)")))
    (sqlite-bind stmt 1 (rollback-event-id rollback))
    (sqlite-bind stmt 2 (rollback-event-service-name rollback))
    (sqlite-bind stmt 3 (rollback-event-from-version rollback))
    (sqlite-bind stmt 4 (rollback-event-to-version rollback))
    (sqlite-bind stmt 5 (rollback-event-reason rollback))
    (sqlite-bind stmt 6 (time->unix (rollback-event-timestamp rollback)))
    (sqlite-bind stmt 7 (rollback-event-initiator rollback))
    (sqlite-bind stmt 8 (list->string (rollback-event-impact rollback)))
    (sqlite-bind stmt 9 (rollback-event-deployment-id rollback))
    (sqlite-step stmt)
    (sqlite-finalize stmt)
    (rollback-event-id rollback)))

(define (get-rollback db id)
  "Retrieve a rollback by ID"
  (let ((stmt (sqlite-prepare db "
    SELECT * FROM rollbacks WHERE id = ?")))
    (sqlite-bind stmt 1 id)
    (let ((result (sqlite-step stmt)))
      (let ((rollback
             (if (sqlite-done? stmt)
                 #f
                 (make-rollback-event
                  #:id (sqlite-column stmt 0)
                  #:service-name (sqlite-column stmt 1)
                  #:from-version (sqlite-column stmt 2)
                  #:to-version (sqlite-column stmt 3)
                  #:reason (sqlite-column stmt 4)
                  #:timestamp (unix->time (sqlite-column stmt 5))
                  #:initiator (sqlite-column stmt 6)
                  #:impact (string->list (sqlite-column stmt 7))
                  #:deployment-id (sqlite-column stmt 8)))))
        (sqlite-finalize stmt)
        rollback))))

(define* (list-rollbacks db #:key service-name from-time to-time limit offset)
  "List rollbacks with optional filters"
  (let* ((conditions '())
         (params '())
         (query-base "SELECT * FROM rollbacks WHERE 1=1"))

    (when service-name
      (set! conditions (cons "service_name = ?" conditions))
      (set! params (cons service-name params)))

    (when from-time
      (set! conditions (cons "timestamp >= ?" conditions))
      (set! params (cons (time->unix from-time) params)))

    (when to-time
      (set! conditions (cons "timestamp <= ?" conditions))
      (set! params (cons (time->unix to-time) params)))

    (let* ((where-clause
            (if (null? conditions)
                ""
                (string-append " AND " (string-join (reverse conditions) " AND "))))
           (order-clause " ORDER BY timestamp DESC")
           (limit-clause
            (if limit
                (format #f " LIMIT ~a OFFSET ~a" limit (or offset 0))
                ""))
           (query (string-append query-base where-clause order-clause limit-clause))
           (stmt (sqlite-prepare db query)))

      ;; Bind parameters
      (let loop ((params (reverse params))
                 (index 1))
        (unless (null? params)
          (sqlite-bind stmt index (car params))
          (loop (cdr params) (+ index 1))))

      ;; Collect results
      (let collect ((rollbacks '()))
        (if (sqlite-done? stmt)
            (begin
              (sqlite-finalize stmt)
              (reverse rollbacks))
            (let ((rollback
                   (make-rollback-event
                    #:id (sqlite-column stmt 0)
                    #:service-name (sqlite-column stmt 1)
                    #:from-version (sqlite-column stmt 2)
                    #:to-version (sqlite-column stmt 3)
                    #:reason (sqlite-column stmt 4)
                    #:timestamp (unix->time (sqlite-column stmt 5))
                    #:initiator (sqlite-column stmt 6)
                    #:impact (string->list (sqlite-column stmt 7))
                    #:deployment-id (sqlite-column stmt 8))))
              (sqlite-step stmt)
              (collect (cons rollback rollbacks))))))))

;;; Service metadata operations

(define (store-service-metadata! db metadata)
  "Store or update service metadata"
  (let ((stmt (sqlite-prepare db "
    INSERT OR REPLACE INTO service_metadata
    (name, type, dependencies, owners, repository, health_check, created, updated)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)")))
    (sqlite-bind stmt 1 (service-metadata-name metadata))
    (sqlite-bind stmt 2 (symbol->string (service-metadata-type metadata)))
    (sqlite-bind stmt 3 (list->string (service-metadata-dependencies metadata)))
    (sqlite-bind stmt 4 (list->string (service-metadata-owners metadata)))
    (sqlite-bind stmt 5 (service-metadata-repository metadata))
    (sqlite-bind stmt 6 (service-metadata-health-check metadata))
    (sqlite-bind stmt 7 (time->unix (service-metadata-created metadata)))
    (sqlite-bind stmt 8 (time->unix (service-metadata-updated metadata)))
    (sqlite-step stmt)
    (sqlite-finalize stmt)
    (service-metadata-name metadata)))

(define (get-service-metadata db name)
  "Retrieve service metadata by name"
  (let ((stmt (sqlite-prepare db "
    SELECT * FROM service_metadata WHERE name = ?")))
    (sqlite-bind stmt 1 name)
    (let ((result (sqlite-step stmt)))
      (let ((metadata
             (if (sqlite-done? stmt)
                 #f
                 (make-service-metadata
                  #:name (sqlite-column stmt 0)
                  #:type (string->symbol (sqlite-column stmt 1))
                  #:dependencies (string->list (sqlite-column stmt 2))
                  #:owners (string->list (sqlite-column stmt 3))
                  #:repository (sqlite-column stmt 4)
                  #:health-check (sqlite-column stmt 5)
                  #:created (unix->time (sqlite-column stmt 6))
                  #:updated (unix->time (sqlite-column stmt 7))))))
        (sqlite-finalize stmt)
        metadata))))

(define (list-services db)
  "List all services with metadata"
  (let ((stmt (sqlite-prepare db "SELECT * FROM service_metadata ORDER BY name")))
    (let collect ((services '()))
      (if (sqlite-done? stmt)
          (begin
            (sqlite-finalize stmt)
            (reverse services))
          (let ((metadata
                 (make-service-metadata
                  #:name (sqlite-column stmt 0)
                  #:type (string->symbol (sqlite-column stmt 1))
                  #:dependencies (string->list (sqlite-column stmt 2))
                  #:owners (string->list (sqlite-column stmt 3))
                  #:repository (sqlite-column stmt 4)
                  #:health-check (sqlite-column stmt 5)
                  #:created (unix->time (sqlite-column stmt 6))
                  #:updated (unix->time (sqlite-column stmt 7)))))
            (sqlite-step stmt)
            (collect (cons metadata services)))))))

;;; Cleanup operations

(define (delete-deployment! db id)
  "Delete a deployment and related records"
  (with-transaction db
    (lambda ()
      (sqlite-exec db (format #f "DELETE FROM deployment_events WHERE deployment_id = '~a'" id))
      (sqlite-exec db (format #f "DELETE FROM deployments WHERE id = '~a'" id)))))

(define (delete-rollback! db id)
  "Delete a rollback record"
  (sqlite-exec db (format #f "DELETE FROM rollbacks WHERE id = '~a'" id)))

;;; Transaction support

(define (with-transaction db thunk)
  "Execute thunk within a database transaction"
  (sqlite-exec db "BEGIN TRANSACTION")
  (catch #t
    (lambda ()
      (let ((result (thunk)))
        (sqlite-exec db "COMMIT")
        result))
    (lambda args
      (sqlite-exec db "ROLLBACK")
      (apply throw args))))

;;; Maintenance

(define (vacuum-database! db)
  "Vacuum the database to reclaim space"
  (sqlite-exec db "VACUUM"))