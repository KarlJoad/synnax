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

(define-public stumpwm-contrib-battery-portable
  (let ((git-branch "master")
        (commit "d0c05077eca5257d33083de949c10bca4aac4242")
        (revision "1"))
    (package
     (name "stumpwm-contrib-battery-portable")
     (version (git-version git-branch revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/stumpwm/stumpwm-contrib.git")
                    (commit git-branch)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zxhqh9wjfk7zas67kmwfx0a47y8rxmh8f1a5rcs300bv1083lkb"))))
     (build-system copy-build-system)
     (arguments
      '(#:install-plan
        ;; Install the contents of battery-portable to the root of #$out
        '(("modeline/battery-portable/" ""))))
     (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/battery-portable")
     (synopsis "Display battery information in StumpWM's modeline")
     (description "StumpWM-contributed module to portably be able to display the
device's battery levels in StumpWM's modeline.")
     (license license:gpl3))))

