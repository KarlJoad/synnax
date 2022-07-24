(define-module (synnax packages wally)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:))

(define-public stumpwm-no-docs
  (package
    (inherit stumpwm)
    (arguments
     (substitute-keyword-arguments (package-arguments stumpwm)
       ((#:phases old-phases)
        #~(modify-phases #$old-phases
            (delete 'install-manual)))))))

(define-public stumpwm-with-patch
  (package
    (inherit stumpwm)
    (source (origin (inherit (package-source stumpwm))
                    (patches
                     (append (list (local-file "patches/stumpwm-module-dir-easy-regexp-replace.patch"))
                             ;; (search-patches "stumpwm-module-dir-easy-regexp-replace.patch")
                             (origin-patches (package-source stumpwm))))))
    (arguments
     (substitute-keyword-arguments (package-arguments stumpwm)
       ((#:phases old-phases)
        #~(modify-phases #$old-phases
            (add-before 'copy-source 'module-dir-point-to-store
              (lambda* (#:key source outputs #:allow-other-keys)
                (substitute* "module.lisp"
                  (("REPLACE-ME") (assoc-ref outputs "out")))))
            (delete 'install-manual)))))))

(define-public stumpwm-with-contrib
  (package
    (inherit stumpwm+slynk)))

stumpwm-with-contrib
