;;; Synnax --- Packages based on the GNU Guix functional package manager

;;; This file contains contributions to StumpWM that need to be packaged
;;; then attached to the StumpWM window manager.

(define-module (synnax packages stumpwm-xyz)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy))

;; Packages install to their directories.
