;;; visualize.scm -- Visualization generation for deployment data
;;; Copyright (C) 2024 DSP-DR
;;;
;;; This file is part of Guile Deploy Ledger.

(define-module (deploy-ledger reporting visualize)
  #:use-module (deploy-ledger core types)
  #:use-module (deploy-ledger storage sqlite)
  #:use-module (deploy-ledger query metrics)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-19)
  #:export (generate-mermaid-timeline
            generate-mermaid-gantt
            generate-dot-dependency-graph
            generate-gnuplot-metrics
            generate-d3-json))

;;; Mermaid diagram generation

(define (generate-mermaid-timeline db output-port service-name days)
  "Generate Mermaid timeline diagram"
  (display "graph TD\n" output-port)
  (let* ((timeline (get-deployment-timeline db
                     #:service-name service-name
                     #:from-time (days-ago days)))
         (events (reverse timeline)))
    (let loop ((events events)
               (prev-id #f)
               (counter 0))
      (unless (null? events)
        (let* ((event (car events))
               (type (assoc-ref event 'type))
               (service (assoc-ref event 'service))
               (id (format #f "E~a" counter)))
          (case type
            ((deployment)
             (let ((version (assoc-ref event 'version))
                   (status (assoc-ref event 'status))
                   (dtype (assoc-ref event 'deployment-type)))
               (format output-port "    ~a[\"~a<br/>v~a<br/>~a\"]~%"
                      id service version dtype)
               (case status
                 ((success) (format output-port "    style ~a fill:#9f9~%" id))
                 ((failure) (format output-port "    style ~a fill:#f99~%" id))
                 ((in-progress) (format output-port "    style ~a fill:#ff9~%" id)))))
            ((rollback)
             (let ((from-ver (assoc-ref event 'from-version))
                   (to-ver (assoc-ref event 'to-version)))
               (format output-port "    ~a[\"ROLLBACK<br/>~a<br/>~a→~a\"]~%"
                      id service from-ver to-ver)
               (format output-port "    style ~a fill:#f9f~%" id))))
          (when prev-id
            (format output-port "    ~a --> ~a~%" prev-id id))
          (loop (cdr events) id (+ counter 1)))))))

(define (generate-mermaid-gantt db output-port service-name days)
  "Generate Mermaid Gantt chart for deployments"
  (display "gantt\n" output-port)
  (display "    title Deployment Timeline\n" output-port)
  (display "    dateFormat YYYY-MM-DD HH:mm\n" output-port)
  (display "    axisFormat %m/%d\n\n" output-port)

  (let ((deployments (list-deployments db
                       #:service-name service-name
                       #:from-time (days-ago days))))
    (for-each
     (lambda (d)
       (let* ((start (deployment-event-started d))
              (completed (or (deployment-event-completed d) (current-time time-utc)))
              (duration (time-difference completed start))
              (status (deployment-event-status d)))
         (format output-port "    ~a v~a :~a, ~a, ~as~%"
                (deployment-event-service-name d)
                (deployment-event-version d)
                (if (eq? status 'success) "done" "crit")
                (date->string (time-utc->date start) "~Y-~m-~d ~H:~M")
                (time-second duration))))
     deployments)))

;;; Graphviz DOT generation

(define (generate-dot-dependency-graph db output-port)
  "Generate DOT graph showing service dependencies"
  (display "digraph ServiceDependencies {\n" output-port)
  (display "    rankdir=LR;\n" output-port)
  (display "    node [shape=box, style=rounded];\n\n" output-port)

  (let ((services (list-services db)))
    ;; Define nodes
    (for-each
     (lambda (svc)
       (format output-port "    \"~a\" [label=\"~a\\n(~a)\"];\n"
              (service-metadata-name svc)
              (service-metadata-name svc)
              (service-metadata-type svc)))
     services)

    (display "\n" output-port)

    ;; Define edges
    (for-each
     (lambda (svc)
       (let ((deps (service-metadata-dependencies svc)))
         (for-each
          (lambda (dep)
            (format output-port "    \"~a\" -> \"~a\";\n"
                   (service-metadata-name svc) dep))
          deps)))
     services))

  (display "}\n" output-port))

;;; Gnuplot script generation

(define (generate-gnuplot-metrics db output-port service-name metric days)
  "Generate Gnuplot script for metrics visualization"
  (format output-port "set title \"~a - ~a Metrics\"~%" service-name metric)
  (display "set xlabel \"Date\"\n" output-port)
  (display "set xdata time\n" output-port)
  (display "set timefmt \"%Y-%m-%d\"\n" output-port)
  (display "set format x \"%m/%d\"\n" output-port)
  (display "set grid\n" output-port)

  (case metric
    ((frequency)
     (display "set ylabel \"Deployments per Day\"\n" output-port)
     (display "plot '-' using 1:2 with lines title \"Frequency\" lw 2\n" output-port)
     (let ((trend (deployment-frequency-trend db service-name
                    #:period-days days
                    #:bucket-days 1)))
       (for-each
        (lambda (point)
          (format output-port "~a ~a~%"
                 (date->string
                  (time-utc->date (days-ago (- days (car point))))
                  "~Y-~m-~d")
                 (cdr point)))
        trend)))

    ((success-rate)
     (display "set ylabel \"Success Rate (%)\"\n" output-port)
     (display "set yrange [0:100]\n" output-port)
     (display "plot '-' using 1:2 with lines title \"Success Rate\" lw 2\n" output-port))

    ((mttr)
     (display "set ylabel \"MTTR (hours)\"\n" output-port)
     (display "plot '-' using 1:2 with lines title \"MTTR\" lw 2\n" output-port)))

  (display "e\n" output-port))

;;; D3.js JSON generation

(define (generate-d3-json db output-port service-name days)
  "Generate JSON data for D3.js visualization"
  (let* ((deployments (list-deployments db
                        #:service-name service-name
                        #:from-time (days-ago days)))
         (nodes '())
         (links '()))

    ;; Create nodes for each deployment
    (for-each
     (lambda (d)
       (set! nodes
             (cons `((id . ,(deployment-event-id d))
                    (service . ,(deployment-event-service-name d))
                    (version . ,(deployment-event-version d))
                    (status . ,(symbol->string (deployment-event-status d)))
                    (type . ,(symbol->string (deployment-event-deployment-type d)))
                    (timestamp . ,(time->string (deployment-event-started d))))
                  nodes)))
     deployments)

    ;; Create links for parent-child relationships
    (for-each
     (lambda (d)
       (when (deployment-event-parent-id d)
         (set! links
               (cons `((source . ,(deployment-event-parent-id d))
                      (target . ,(deployment-event-id d))
                      (type . "parent"))
                    links))))
     deployments)

    ;; Output as JSON
    (display "{\n  \"nodes\": [\n" output-port)
    (let loop ((n (reverse nodes)))
      (unless (null? n)
        (display "    {\n" output-port)
        (for-each
         (lambda (field)
           (format output-port "      \"~a\": \"~a\"~a~%"
                  (car field) (cdr field)
                  (if (eq? field (last (car n))) "" ",")))
         (car n))
        (display "    }" output-port)
        (unless (null? (cdr n))
          (display "," output-port))
        (newline output-port)
        (loop (cdr n))))

    (display "  ],\n  \"links\": [\n" output-port)
    (let loop ((l (reverse links)))
      (unless (null? l)
        (display "    {\n" output-port)
        (for-each
         (lambda (field)
           (format output-port "      \"~a\": \"~a\"~a~%"
                  (car field) (cdr field)
                  (if (eq? field (last (car l))) "" ",")))
         (car l))
        (display "    }" output-port)
        (unless (null? (cdr l))
          (display "," output-port))
        (newline output-port)
        (loop (cdr l))))

    (display "  ]\n}\n" output-port)))

(define (last lst)
  "Get last element of list"
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))