#!/usr/bin/env guile
!#
;;; run-tests.scm -- Test runner for Guile Deploy Ledger
;;; Copyright (C) 2024 DSP-DR

(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))

(use-modules (srfi srfi-64)
             (ice-9 ftw)
             (ice-9 regex))

(define (find-test-files dir)
  "Find all test files in directory"
  (let ((test-files '()))
    (ftw dir
         (lambda (filename statinfo flag)
           (when (and (eq? flag 'regular)
                      (string-match "test-.*\\.scm$" filename))
             (set! test-files (cons filename test-files)))
           #t))
    (reverse test-files)))

(define (run-test-file file)
  "Run a single test file"
  (format #t "Running ~a...~%" file)
  (primitive-load file))

(define (main args)
  (let* ((test-dir (if (> (length args) 1)
                       (cadr args)
                       (dirname (current-filename))))
         (test-files (find-test-files test-dir)))

    (format #t "Found ~a test files~%" (length test-files))
    (format #t "========================================~%")

    ;; Initialize test runner
    (test-runner-current (test-runner-simple))

    ;; Run each test file
    (for-each run-test-file test-files)

    ;; Report results
    (let ((runner (test-runner-current)))
      (format #t "~%========================================~%")
      (format #t "Test Results:~%")
      (format #t "  Passed: ~a~%" (test-runner-pass-count runner))
      (format #t "  Failed: ~a~%" (test-runner-fail-count runner))
      (format #t "  Skipped: ~a~%" (test-runner-skip-count runner))
      (format #t "========================================~%")

      ;; Exit with error code if any tests failed
      (exit (if (zero? (test-runner-fail-count runner)) 0 1)))))

(main (command-line))