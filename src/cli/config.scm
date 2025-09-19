;;; config.scm -- Configuration management
;;; Copyright (C) 2024 DSP-DR
;;;
;;; This file is part of Guile Deploy Ledger.

(define-module (deploy-ledger cli config)
  #:use-module (ice-9 rdelim)
  #:export (load-config
            save-config
            get-config-value
            set-config-value
            default-config))

(define default-config
  '((database . "~/.deploy-ledger/deployments.db")
    (port . 8080)
    (prometheus-port . 9090)
    (webhook-secret . #f)
    (prometheus-enabled . #f)
    (quiet . #f)
    (verbose . #f)
    (default-environment . "production")
    (default-deployment-type . rolling)
    (retention-days . 365)))

(define (expand-path path)
  "Expand ~ in paths"
  (if (and path (string-prefix? "~" path))
      (string-append (getenv "HOME") (substring path 1))
      path))

(define (load-config filename)
  "Load configuration from file or return defaults"
  (let ((path (expand-path filename)))
    (if (and path (file-exists? path))
        (with-input-from-file path
          (lambda ()
            (let ((config (read)))
              (if (and (list? config) (every pair? config))
                  config
                  default-config))))
        default-config)))

(define (save-config config filename)
  "Save configuration to file"
  (let ((path (expand-path filename)))
    (with-output-to-file path
      (lambda ()
        (write config)
        (newline)))))

(define (get-config-value config key)
  "Get a configuration value"
  (assoc-ref config key))

(define (set-config-value config key value)
  "Set a configuration value"
  (assoc-set! config key value))