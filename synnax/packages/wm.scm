(define-module (synnax packages wally)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
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

(define-public stumpwm-with-contrib
  (package
    (inherit stumpwm+slynk)))

stumpwm-with-contrib
