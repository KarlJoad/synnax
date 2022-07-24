(define-module (synnax packages wally)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:))

(define-public stumpwm+slynk-with-contrib
  (package
    (inherit stumpwm+slynk)
    (source (origin (inherit (package-source stumpwm+slynk))
                    (patches
                     (append (list (local-file "patches/stumpwm-module-dir-easy-regexp-replace.patch"))
                             (origin-patches (package-source stumpwm+slynk))))))
    (inputs
     (cons `("stumpwm-contrib" ,(package-source (@@ (gnu packages wm) stumpwm-contrib)))
           (package-inputs stumpwm+slynk)))
    (arguments
     (substitute-keyword-arguments (package-arguments stumpwm+slynk)
       ((#:phases old-phases)
        #~(modify-phases #$old-phases
            (add-after 'unpack 'module-dir-point-to-store
              (lambda* (#:key source inputs outputs #:allow-other-keys)
                (let ((modules-dir #$(this-package-input "stumpwm-contrib"))
                      (out (assoc-ref outputs "out")))
                  (format #t "Modifying module.lisp to point to ~s~%" modules-dir)
                  (substitute* "module.lisp"
                    (("REPLACE-ME") modules-dir)))))))))))

