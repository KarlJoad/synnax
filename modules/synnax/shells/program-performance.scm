(define-module (synnax shells program-performance)
  #:use-module (guix profiles)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages kde))

(packages->manifest
 (list valgrind
       kcachegrind))
