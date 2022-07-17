(define-module (synnax packages wally)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:))

(define-public stumpwm-with-contrib
  (package
    (inherit stumpwm+slynk)))

stumpwm-with-contrib
