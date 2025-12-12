(define-module (synnax packages)
  #:use-module (ice-9 match)
  #:export (%synnax-patch-path))

(define %synnax-root-directory
  ;; Absolute file name of the module hierarchy.
  (letrec-syntax ((dirname* (syntax-rules ()
                              ((_ file)
                               (dirname file))
                              ((_ file head tail ...)
                               (dirname (dirname* file tail ...)))))
                  (try      (syntax-rules ()
                              ((_ (file things ...) rest ...)
                               (match (search-path %load-path file)
                                 (#f
                                  (try rest ...))
                                 (absolute
                                  (dirname* absolute things ...))))
                              ((_)
                               #f))))
    (try ;; ("gnu/packages/base.scm" gnu/ packages/)
         ("synnax/packages.scm"      synnax/)
         ;; ("guix.scm")
         )))

(define %default-package-module-path
  ;; Default search path for package modules.
  `((,%synnax-root-directory . "synnax/packages")))

(define %synnax-patch-path
  (map (lambda (directory)
         ;; NOTE: We copied this from Guix's (gnu packages) %patch-path. But,
         ;; we use string-contains because I'm too lazy to do a proper "search"
         ;; for Synnax's %synnax-patch-path through Guile's %load-path.
         (if (string=? directory %synnax-root-directory)
             (string-append directory "/synnax/packages/patches")
             directory))
       %load-path))
