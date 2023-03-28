(define-module (synnax shells vcode-llvm)
  #:use-module (guix profiles)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages kde))

(packages->manifest
 (list valgrind
       kcachegrind))
