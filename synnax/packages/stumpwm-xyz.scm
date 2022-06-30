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

(define stumpwm-contrib-git-branch "master")
(define stumpwm-contrib-git-commit "d0c05077eca5257d33083de949c10bca4aac4242")
(define stumpwm-contrib-revision "1")
(define stumpwm-contrib-version
  (git-version stumpwm-contrib-git-branch stumpwm-contrib-revision
               stumpwm-contrib-git-commit))

(define stumpwm-contrib-source
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/stumpwm/stumpwm-contrib.git")
         (commit stumpwm-contrib-git-branch)))
   (file-name (git-file-name "stumpwm-contrib" stumpwm-contrib-version))
   (sha256
    (base32
     "0zxhqh9wjfk7zas67kmwfx0a47y8rxmh8f1a5rcs300bv1083lkb"))))

(define-public stumpwm-contrib-battery-portable
  (package
   (name "stumpwm-contrib-battery-portable")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      ;; Install the contents of battery-portable to the root of #$out
      '(("modeline/battery-portable/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/battery-portable")
   (synopsis "Display battery information in StumpWM's modeline")
   (description "StumpWM-contributed module to portably be able to display the
device's battery levels in StumpWM's modeline.")
   (license license:gpl3)))

(define-public stumpwm-contrib-cpu
  (package
   (name "stumpwm-contrib-cpu")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("modeline/cpu/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/cpu")
   (synopsis "Display CPU information in StumpWM's modeline")
   (description "StumpWM-contributed module to display the device's CPU usage
in StumpWM's modeline.")
   (license license:gpl3)))

(define-public stumpwm-contrib-disk
  (package
   (name "stumpwm-contrib-disk")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("modeline/disk/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/disk")
   (synopsis "Display disk information in StumpWM's modeline")
   (description "StumpWM-contributed module to display the device's disk usage
in StumpWM's modeline.")
   (license license:gpl3)))

(define-public stumpwm-contrib-hostname
  (package
   (name "stumpwm-contrib-hostname")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("modeline/hostname/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/hostname")
   (synopsis "Display hostname information in StumpWM's modeline")
   (description "StumpWM-contributed module to display the device's hostname
usage in StumpWM's modeline.")
   (license license:bsd-2)))

