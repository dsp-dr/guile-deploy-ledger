;;; sqlite.scm -- SQLite storage backend for deployment ledger
;;; Copyright (C) 2024 DSP-DR
;;;
;;; This file is part of Guile Deploy Ledger.

(define-module (deploy-ledger storage sqlite)
  #:use-module (deploy-ledger core types)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
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

;; Try to load SQLite3 module if available
(define sqlite3-available?
  (catch 'misc-error
    (lambda ()
      (resolve-interface '(sqlite3))
      (use-modules (sqlite3))
      #t)
    (lambda args
      (format (current-error-port)
              "Note: SQLite3 module not available. Using stub implementation.~%")
      #f)))

;;; Stub implementations when SQLite3 is not available

(define (stub-warning operation)
  (format (current-error-port)
          "Warning: ~a called but SQLite3 module not available~%"
          operation)
  #f)

;;; Database connection management

(define (open-deployment-db filename)
  "Open or create a SQLite database for deployments"
  (if sqlite3-available?
      (let ((db (sqlite-open filename)))
        (sqlite-exec db "PRAGMA foreign_keys = ON;")
        (sqlite-exec db "PRAGMA journal_mode = WAL;")
        (init-database! db)
        db)
      (stub-warning "open-deployment-db")))

(define (close-deployment-db db)
  "Close the deployment database connection"
  (if (and sqlite3-available? db)
      (sqlite-close db)
      #t))

;;; Schema initialization

(define (init-database! db)
  "Initialize database schema if not exists"
  (if (and sqlite3-available? db)
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
      initiated INTEGER NOT NULL,
      completed INTEGER,
      status TEXT NOT NULL,
      deployment_id TEXT,
      created_at INTEGER DEFAULT (strftime('%s', 'now')),
      FOREIGN KEY (deployment_id) REFERENCES deployments(id)
    );

    CREATE INDEX IF NOT EXISTS idx_rollbacks_service
      ON rollbacks(service_name);

    CREATE TABLE IF NOT EXISTS service_metadata (
      service_name TEXT PRIMARY KEY,
      description TEXT,
      team TEXT,
      repository_url TEXT,
      documentation_url TEXT,
      health_check_url TEXT,
      dependencies TEXT,
      tags TEXT,
      created_at INTEGER DEFAULT (strftime('%s', 'now')),
      updated_at INTEGER DEFAULT (strftime('%s', 'now'))
    );

    CREATE TABLE IF NOT EXISTS deployment_artifacts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      deployment_id TEXT NOT NULL,
      artifact_type TEXT NOT NULL,
      artifact_path TEXT NOT NULL,
      checksum TEXT,
      size_bytes INTEGER,
      created_at INTEGER DEFAULT (strftime('%s', 'now')),
      FOREIGN KEY (deployment_id) REFERENCES deployments(id)
    );

    CREATE INDEX IF NOT EXISTS idx_artifacts_deployment
      ON deployment_artifacts(deployment_id);
    ")
      #t))

;;; Storage operations - Deployments

(define (store-deployment! db deployment)
  "Store a deployment event in the database"
  (if (and sqlite3-available? db)
      (let ((stmt (sqlite-prepare db "
        INSERT INTO deployments
        (id, service_name, version, environment, deployment_type,
         started, completed, status, initiator, metadata, parent_id)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")))
        (sqlite-bind stmt 1 (deployment-event-id deployment))
        (sqlite-bind stmt 2 (deployment-event-service-name deployment))
        (sqlite-bind stmt 3 (deployment-event-version deployment))
        (sqlite-bind stmt 4 (symbol->string (deployment-event-environment deployment)))
        (sqlite-bind stmt 5 (symbol->string (deployment-event-deployment-type deployment)))
        (sqlite-bind stmt 6 (time->seconds (deployment-event-started deployment)))
        (sqlite-bind stmt 7 (and (deployment-event-completed deployment)
                                 (time->seconds (deployment-event-completed deployment))))
        (sqlite-bind stmt 8 (symbol->string (deployment-event-status deployment)))
        (sqlite-bind stmt 9 (deployment-event-initiator deployment))
        (sqlite-bind stmt 10 (format #f "~s" (deployment-event-metadata deployment)))
        (sqlite-bind stmt 11 (deployment-event-parent-id deployment))
        (sqlite-step stmt)
        (sqlite-finalize stmt)
        #t)
      (stub-warning "store-deployment!")))

(define (get-deployment db id)
  "Retrieve a deployment by ID"
  (if (and sqlite3-available? db)
      (let* ((stmt (sqlite-prepare db "
         SELECT id, service_name, version, environment, deployment_type,
                started, completed, status, initiator, metadata, parent_id
         FROM deployments
         WHERE id = ?"))
             (_ (sqlite-bind stmt 1 id))
             (result (sqlite-step stmt)))
        (if (sqlite-done? stmt)
            #f
            (let ((deployment (parse-deployment-row result)))
              (sqlite-finalize stmt)
              deployment)))
      (stub-warning "get-deployment")))

(define (list-deployments db #:key service environment status limit offset)
  "List deployments with optional filters"
  (if (and sqlite3-available? db)
      (let* ((query-base "
         SELECT id, service_name, version, environment, deployment_type,
                started, completed, status, initiator, metadata, parent_id
         FROM deployments")

             ;; Build conditions and params
             (conditions-params
              (let ((c '()) (p '()))
                (when service
                  (set! c (cons "service_name = ?" c))
                  (set! p (cons service p)))
                (when environment
                  (set! c (cons "environment = ?" c))
                  (set! p (cons (symbol->string environment) p)))
                (when status
                  (set! c (cons "status = ?" c))
                  (set! p (cons (symbol->string status) p)))
                (cons (reverse c) (reverse p))))

             (conditions (car conditions-params))
             (params (cdr conditions-params))

             ;; Build full query
             (where-clause (if (null? conditions)
                             ""
                             (string-append " WHERE "
                                          (string-join conditions " AND "))))
             (order-clause " ORDER BY started DESC")
             (limit-clause (if limit
                             (format #f " LIMIT ~a" limit)
                             ""))
             (offset-clause (if offset
                              (format #f " OFFSET ~a" offset)
                              ""))
             (query (string-append query-base where-clause order-clause
                                 limit-clause offset-clause))
             (stmt (sqlite-prepare db query)))

        ;; Bind parameters
        (let loop ((p params)
                  (index 1))
          (unless (null? p)
            (sqlite-bind stmt index (car p))
            (loop (cdr p) (+ index 1))))

        ;; Collect results
        (let collect ((deployments '()))
          (sqlite-step stmt)
          (if (sqlite-done? stmt)
              (begin
                (sqlite-finalize stmt)
                (reverse deployments))
              (let ((row (sqlite-row stmt)))
                (collect (cons (parse-deployment-row row) deployments))))))
      '()))

(define (update-deployment-status! db id status #:optional completed)
  "Update deployment status and optionally set completion time"
  (if (and sqlite3-available? db)
      (let* ((query (if completed
                       "UPDATE deployments SET status = ?, completed = ? WHERE id = ?"
                       "UPDATE deployments SET status = ? WHERE id = ?"))
             (stmt (sqlite-prepare db query)))
        (sqlite-bind stmt 1 (symbol->string status))
        (if completed
            (begin
              (sqlite-bind stmt 2 (time->seconds completed))
              (sqlite-bind stmt 3 id))
            (sqlite-bind stmt 2 id))
        (sqlite-step stmt)
        (sqlite-finalize stmt)
        #t)
      (stub-warning "update-deployment-status!")))

(define (delete-deployment! db id)
  "Delete a deployment record"
  (if (and sqlite3-available? db)
      (let ((stmt (sqlite-prepare db "DELETE FROM deployments WHERE id = ?")))
        (sqlite-bind stmt 1 id)
        (sqlite-step stmt)
        (sqlite-finalize stmt)
        #t)
      (stub-warning "delete-deployment!")))

;;; Storage operations - Rollbacks

(define (store-rollback! db rollback)
  "Store a rollback event in the database"
  (if (and sqlite3-available? db)
      (let ((stmt (sqlite-prepare db "
        INSERT INTO rollbacks
        (id, service_name, from_version, to_version, reason,
         initiated, completed, status, deployment_id)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)")))
        (sqlite-bind stmt 1 (rollback-event-id rollback))
        (sqlite-bind stmt 2 (rollback-event-service-name rollback))
        (sqlite-bind stmt 3 (rollback-event-from-version rollback))
        (sqlite-bind stmt 4 (rollback-event-to-version rollback))
        (sqlite-bind stmt 5 (rollback-event-reason rollback))
        (sqlite-bind stmt 6 (time->seconds (rollback-event-initiated rollback)))
        (sqlite-bind stmt 7 (and (rollback-event-completed rollback)
                                 (time->seconds (rollback-event-completed rollback))))
        (sqlite-bind stmt 8 (symbol->string (rollback-event-status rollback)))
        (sqlite-bind stmt 9 (rollback-event-deployment-id rollback))
        (sqlite-step stmt)
        (sqlite-finalize stmt)
        #t)
      (stub-warning "store-rollback!")))

(define (get-rollback db id)
  "Retrieve a rollback by ID"
  (if (and sqlite3-available? db)
      (let* ((stmt (sqlite-prepare db "
         SELECT id, service_name, from_version, to_version, reason,
                initiated, completed, status, deployment_id
         FROM rollbacks
         WHERE id = ?"))
             (_ (sqlite-bind stmt 1 id))
             (result (sqlite-step stmt)))
        (if (sqlite-done? stmt)
            #f
            (let ((rollback (parse-rollback-row result)))
              (sqlite-finalize stmt)
              rollback)))
      (stub-warning "get-rollback")))

(define (list-rollbacks db #:key service limit offset)
  "List rollbacks with optional filters"
  (if (and sqlite3-available? db)
      (let* ((query (if service
                       "SELECT * FROM rollbacks WHERE service_name = ? ORDER BY initiated DESC"
                       "SELECT * FROM rollbacks ORDER BY initiated DESC"))
             (stmt (sqlite-prepare db query)))
        (when service
          (sqlite-bind stmt 1 service))

        (let collect ((rollbacks '()))
          (sqlite-step stmt)
          (if (sqlite-done? stmt)
              (begin
                (sqlite-finalize stmt)
                (reverse rollbacks))
              (let ((row (sqlite-row stmt)))
                (collect (cons (parse-rollback-row row) rollbacks))))))
      '()))

(define (delete-rollback! db id)
  "Delete a rollback record"
  (if (and sqlite3-available? db)
      (let ((stmt (sqlite-prepare db "DELETE FROM rollbacks WHERE id = ?")))
        (sqlite-bind stmt 1 id)
        (sqlite-step stmt)
        (sqlite-finalize stmt)
        #t)
      (stub-warning "delete-rollback!")))

;;; Storage operations - Service Metadata

(define (store-service-metadata! db metadata)
  "Store or update service metadata"
  (if (and sqlite3-available? db)
      (let ((stmt (sqlite-prepare db "
        INSERT OR REPLACE INTO service_metadata
        (service_name, description, team, repository_url,
         documentation_url, health_check_url, dependencies, tags, updated_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)")))
        (sqlite-bind stmt 1 (service-metadata-name metadata))
        (sqlite-bind stmt 2 (service-metadata-description metadata))
        (sqlite-bind stmt 3 (service-metadata-team metadata))
        (sqlite-bind stmt 4 (service-metadata-repository-url metadata))
        (sqlite-bind stmt 5 (service-metadata-documentation-url metadata))
        (sqlite-bind stmt 6 (service-metadata-health-check-url metadata))
        (sqlite-bind stmt 7 (format #f "~s" (service-metadata-dependencies metadata)))
        (sqlite-bind stmt 8 (format #f "~s" (service-metadata-tags metadata)))
        (sqlite-bind stmt 9 (current-time time-utc))
        (sqlite-step stmt)
        (sqlite-finalize stmt)
        #t)
      (stub-warning "store-service-metadata!")))

(define (get-service-metadata db service-name)
  "Retrieve service metadata"
  (if (and sqlite3-available? db)
      (let* ((stmt (sqlite-prepare db "
         SELECT service_name, description, team, repository_url,
                documentation_url, health_check_url, dependencies, tags
         FROM service_metadata
         WHERE service_name = ?"))
             (_ (sqlite-bind stmt 1 service-name))
             (result (sqlite-step stmt)))
        (if (sqlite-done? stmt)
            #f
            (let ((metadata (parse-metadata-row result)))
              (sqlite-finalize stmt)
              metadata)))
      (stub-warning "get-service-metadata")))

(define (list-services db)
  "List all services with metadata"
  (if (and sqlite3-available? db)
      (let ((stmt (sqlite-prepare db "
        SELECT service_name, description, team, repository_url,
               documentation_url, health_check_url, dependencies, tags
        FROM service_metadata
        ORDER BY service_name")))

        (let collect ((services '()))
          (sqlite-step stmt)
          (if (sqlite-done? stmt)
              (begin
                (sqlite-finalize stmt)
                (reverse services))
              (let ((row (sqlite-row stmt)))
                (collect (cons (parse-metadata-row row) services))))))
      '()))

;;; Transaction support

(define (with-transaction db proc)
  "Execute procedure within a database transaction"
  (if (and sqlite3-available? db)
      (begin
        (sqlite-exec db "BEGIN TRANSACTION")
        (catch #t
          (lambda ()
            (let ((result (proc)))
              (sqlite-exec db "COMMIT")
              result))
          (lambda args
            (sqlite-exec db "ROLLBACK")
            (apply throw args))))
      (proc)))

;;; Utility operations

(define (vacuum-database! db)
  "Vacuum the database to reclaim space"
  (if (and sqlite3-available? db)
      (begin
        (sqlite-exec db "VACUUM")
        #t)
      (stub-warning "vacuum-database!")))

;;; Helper functions for parsing database rows

(define (parse-deployment-row row)
  "Parse a database row into a deployment event"
  (if sqlite3-available?
      (make-deployment-event-internal
       (vector-ref row 0)  ; id
       (vector-ref row 1)  ; service-name
       (vector-ref row 2)  ; version
       (string->symbol (vector-ref row 3))  ; environment
       (string->symbol (vector-ref row 4))  ; deployment-type
       (seconds->time (vector-ref row 5) time-utc)  ; started
       (and (vector-ref row 6)
            (seconds->time (vector-ref row 6) time-utc))  ; completed
       (string->symbol (vector-ref row 7))  ; status
       (vector-ref row 8)  ; initiator
       (if (vector-ref row 9)
           (with-input-from-string (vector-ref row 9) read)
           '())  ; metadata
       (vector-ref row 10))  ; parent-id
      #f))

(define (parse-rollback-row row)
  "Parse a database row into a rollback event"
  (if sqlite3-available?
      (make-rollback-event-internal
       (vector-ref row 0)  ; id
       (vector-ref row 1)  ; service-name
       (vector-ref row 2)  ; from-version
       (vector-ref row 3)  ; to-version
       (vector-ref row 4)  ; reason
       (seconds->time (vector-ref row 5) time-utc)  ; initiated
       (and (vector-ref row 6)
            (seconds->time (vector-ref row 6) time-utc))  ; completed
       (string->symbol (vector-ref row 7))  ; status
       (vector-ref row 8))  ; deployment-id
      #f))

(define (parse-metadata-row row)
  "Parse a database row into service metadata"
  (if sqlite3-available?
      (make-service-metadata-internal
       (vector-ref row 0)  ; name
       (vector-ref row 1)  ; description
       (vector-ref row 2)  ; team
       (vector-ref row 3)  ; repository-url
       (vector-ref row 4)  ; documentation-url
       (vector-ref row 5)  ; health-check-url
       (if (vector-ref row 6)
           (with-input-from-string (vector-ref row 6) read)
           '())  ; dependencies
       (if (vector-ref row 7)
           (with-input-from-string (vector-ref row 7) read)
           '()))  ; tags
      #f))

;; Helper for time conversion
(define (seconds->time seconds time-type)
  "Convert seconds to time structure"
  (make-time time-type 0 seconds))

(define (time->seconds time)
  "Convert time structure to seconds"
  (time-second time))