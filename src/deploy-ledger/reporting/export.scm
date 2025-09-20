;;; export.scm -- Data export functionality
;;; Copyright (C) 2024 DSP-DR
;;;
;;; This file is part of Guile Deploy Ledger.

(define-module (deploy-ledger reporting export)
  #:use-module (deploy-ledger core types)
  #:use-module (deploy-ledger storage sqlite)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-19)
  #:export (export-deployments-sexp
            export-deployments-json
            export-deployments-csv
            export-deployments-org
            export-metrics-report
            import-deployments-sexp))

;;; S-expression export/import

(define (deployment->sexp deployment)
  "Convert deployment to S-expression"
  `(deployment
    (id ,(deployment-event-id deployment))
    (service ,(deployment-event-service-name deployment))
    (version ,(deployment-event-version deployment))
    (environment ,(deployment-event-environment deployment))
    (type ,(deployment-event-deployment-type deployment))
    (started ,(time->string (deployment-event-started deployment)))
    (completed ,(and (deployment-event-completed deployment)
                    (time->string (deployment-event-completed deployment))))
    (status ,(deployment-event-status deployment))
    (initiator ,(deployment-event-initiator deployment))
    (metadata ,(deployment-event-metadata deployment))
    (parent-id ,(deployment-event-parent-id deployment))))

(define (rollback->sexp rollback)
  "Convert rollback to S-expression"
  `(rollback
    (id ,(rollback-event-id rollback))
    (service ,(rollback-event-service-name rollback))
    (from-version ,(rollback-event-from-version rollback))
    (to-version ,(rollback-event-to-version rollback))
    (reason ,(rollback-event-reason rollback))
    (timestamp ,(time->string (rollback-event-timestamp rollback)))
    (initiator ,(rollback-event-initiator rollback))
    (impact ,(rollback-event-impact rollback))
    (deployment-id ,(rollback-event-deployment-id rollback))))

(define (export-deployments-sexp db output-port)
  "Export all deployments as S-expressions"
  (let ((deployments (list-deployments db))
        (rollbacks (list-rollbacks db)))
    (write '(deploy-ledger-export (version 1)) output-port)
    (newline output-port)
    (for-each (lambda (d)
               (pretty-print (deployment->sexp d) output-port)
               (newline output-port))
             deployments)
    (for-each (lambda (r)
               (pretty-print (rollback->sexp r) output-port)
               (newline output-port))
             rollbacks)))

(define (import-deployments-sexp db input-port)
  "Import deployments from S-expressions"
  (let ((header (read input-port)))
    (unless (and (pair? header)
                (eq? (car header) 'deploy-ledger-export))
      (error "Invalid export file format"))
    (let loop ()
      (let ((entry (read input-port)))
        (unless (eof-object? entry)
          (match entry
            (('deployment . fields)
             (let ((deployment (sexp->deployment fields)))
               (store-deployment! db deployment)))
            (('rollback . fields)
             (let ((rollback (sexp->rollback fields)))
               (store-rollback! db rollback)))
            (_ (error "Unknown entry type" entry)))
          (loop))))))

;;; JSON export

(define (time->json-string time)
  "Convert SRFI-19 time to JSON timestamp"
  (if time
      (date->string (time-utc->date time) "~Y-~m-~dT~H:~M:~SZ")
      "null"))

(define (deployment->json deployment)
  "Convert deployment to JSON object"
  (format #f "{
  \"id\": \"~a\",
  \"service\": \"~a\",
  \"version\": \"~a\",
  \"environment\": \"~a\",
  \"deployment_type\": \"~a\",
  \"started\": \"~a\",
  \"completed\": ~a,
  \"status\": \"~a\",
  \"initiator\": \"~a\",
  \"parent_id\": ~a
}"
          (deployment-event-id deployment)
          (deployment-event-service-name deployment)
          (deployment-event-version deployment)
          (deployment-event-environment deployment)
          (deployment-event-deployment-type deployment)
          (time->json-string (deployment-event-started deployment))
          (if (deployment-event-completed deployment)
              (format #f "\"~a\"" (time->json-string (deployment-event-completed deployment)))
              "null")
          (deployment-event-status deployment)
          (deployment-event-initiator deployment)
          (if (deployment-event-parent-id deployment)
              (format #f "\"~a\"" (deployment-event-parent-id deployment))
              "null")))

(define (export-deployments-json db output-port)
  "Export deployments as JSON"
  (let ((deployments (list-deployments db)))
    (display "[\n" output-port)
    (let loop ((deps deployments))
      (unless (null? deps)
        (display (deployment->json (car deps)) output-port)
        (unless (null? (cdr deps))
          (display "," output-port))
        (newline output-port)
        (loop (cdr deps))))
    (display "]\n" output-port)))

;;; CSV export

(define (export-deployments-csv db output-port)
  "Export deployments as CSV"
  (display "id,service,version,environment,type,started,completed,status,initiator\n"
           output-port)
  (let ((deployments (list-deployments db)))
    (for-each (lambda (d)
               (format output-port "\"~a\",\"~a\",\"~a\",\"~a\",\"~a\",\"~a\",\"~a\",\"~a\",\"~a\"~%"
                      (deployment-event-id d)
                      (deployment-event-service-name d)
                      (deployment-event-version d)
                      (deployment-event-environment d)
                      (deployment-event-deployment-type d)
                      (time->json-string (deployment-event-started d))
                      (or (and (deployment-event-completed d)
                              (time->json-string (deployment-event-completed d)))
                          "")
                      (deployment-event-status d)
                      (deployment-event-initiator d)))
             deployments)))

;;; Org-mode export

(define (export-deployments-org db output-port)
  "Export deployments as Org-mode document"
  (display "#+TITLE: Deployment Report
#+DATE: " output-port)
  (display (date->string (current-date) "~Y-~m-~d") output-port)
  (display "\n\n* Deployment Summary\n\n" output-port)

  ;; Statistics
  (let* ((deployments (list-deployments db))
         (total (length deployments))
         (successful (length (filter (lambda (d)
                                       (eq? (deployment-event-status d) 'success))
                                     deployments)))
         (failed (length (filter (lambda (d)
                                  (eq? (deployment-event-status d) 'failure))
                                 deployments))))
    (format output-port "- Total deployments: ~a~%" total)
    (format output-port "- Successful: ~a (~,1f%)~%"
           successful (if (> total 0) (* 100.0 (/ successful total)) 0))
    (format output-port "- Failed: ~a (~,1f%)~%"
           failed (if (> total 0) (* 100.0 (/ failed total)) 0)))

  ;; Recent deployments table
  (display "\n* Recent Deployments\n\n" output-port)
  (display "| Service | Version | Environment | Type | Status | Date |\n" output-port)
  (display "|---------|---------|-------------|------|--------|------|\n" output-port)

  (let ((recent (take (list-deployments db)
                     (min 20 (length (list-deployments db))))))
    (for-each (lambda (d)
               (format output-port "| ~a | ~a | ~a | ~a | ~a | ~a |~%"
                      (deployment-event-service-name d)
                      (deployment-event-version d)
                      (deployment-event-environment d)
                      (deployment-event-deployment-type d)
                      (deployment-event-status d)
                      (date->string
                       (time-utc->date (deployment-event-started d))
                       "~Y-~m-~d ~H:~M")))
             recent)))

  ;; Rollbacks section
  (display "\n* Recent Rollbacks\n\n" output-port)
  (let ((rollbacks (list-rollbacks db)))
    (if (null? rollbacks)
        (display "No rollbacks recorded.\n" output-port)
        (begin
          (display "| Service | From Version | To Version | Reason | Date |\n" output-port)
          (display "|---------|--------------|------------|--------|------|\n" output-port)
          (for-each (lambda (r)
                     (format output-port "| ~a | ~a | ~a | ~a | ~a |~%"
                            (rollback-event-service-name r)
                            (rollback-event-from-version r)
                            (rollback-event-to-version r)
                            (rollback-event-reason r)
                            (date->string
                             (time-utc->date (rollback-event-timestamp r))
                             "~Y-~m-~d ~H:~M")))
                   (take rollbacks (min 10 (length rollbacks))))))))

(define (export-metrics-report db services output-port)
  "Export comprehensive metrics report"
  (display "#+TITLE: Deployment Metrics Report
#+DATE: " output-port)
  (display (date->string (current-date) "~Y-~m-~d") output-port)
  (display "\n\n* Executive Summary\n\n" output-port)

  (for-each (lambda (service)
             (format output-port "** ~a\n\n" service)
             (format output-port "- Deployment Frequency: ~,2f per day\n"
                    (calculate-deployment-frequency db service))
             (format output-port "- Success Rate: ~,1f%\n"
                    (calculate-success-rate db service))
             (format output-port "- MTTR: ~,2f hours\n"
                    (/ (calculate-mttr db service) 3600))
             (format output-port "- Health Score: ~,1f/100\n\n"
                    (service-health-score db service)))
           services))

;;; Helper functions

(define (take lst n)
  "Take first n elements from list"
  (if (or (null? lst) (zero? n))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (sexp->deployment fields)
  "Convert S-expression to deployment (stub)"
  ;; Implementation would parse the fields and reconstruct deployment
  (error "Import not yet implemented"))

(define (sexp->rollback fields)
  "Convert S-expression to rollback (stub)"
  ;; Implementation would parse the fields and reconstruct rollback
  (error "Import not yet implemented"))