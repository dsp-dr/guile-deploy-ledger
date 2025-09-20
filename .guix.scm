;;; .guix.scm -- Guix package definition for Guile Deploy Ledger
;;; Copyright (C) 2024 DSP-DR

(use-modules (guix packages)
             (guix git-download)
             (guix build-system gnu)
             (guix licenses)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages databases))

(package
  (name "guile-deploy-ledger")
  (version "1.0.0")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/dsp-dr/guile-deploy-ledger")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0000000000000000000000000000000000000000000000000000"))))
  (build-system gnu-build-system)
  (arguments
   `(#:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'bootstrap
         (lambda _
           (substitute* "scripts/guile-deploy-ledger"
             (("/usr/bin/env guile")
              (string-append #$(file-append guile-3.0 "/bin/guile"))))
           #t))
       (replace 'configure
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (setenv "PREFIX" out)
             #t)))
       (replace 'build
         (lambda _
           (invoke "make" "compile")))
       (replace 'check
         (lambda* (#:key tests? #:allow-other-keys)
           (when tests?
             (invoke "make" "test"))))
       (replace 'install
         (lambda _
           (invoke "make" "install"))))))
  (native-inputs
   `(("guile" ,guile-3.0)))
  (inputs
   `(("guile" ,guile-3.0)
     ("guile-sqlite3" ,guile-sqlite3)))
  (propagated-inputs
   `(("guile-sqlite3" ,guile-sqlite3)))
  (home-page "https://github.com/dsp-dr/guile-deploy-ledger")
  (synopsis "Comprehensive deployment tracking and change control system")
  (description
   "Guile Deploy Ledger is a deployment tracking system that serves as a
ledger for deployment events across both monolithic and microservice
architectures.  It provides detailed tracking of deployments, rollbacks,
and comprehensive metrics calculation including MTTR, deployment frequency,
and success rates.  The system supports multiple deployment strategies
including blue-green, canary, rolling, and big-bang deployments.")
  (license gpl3+))