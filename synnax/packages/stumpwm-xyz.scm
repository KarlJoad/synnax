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

(define-public stumpwm-contrib-maildir
  (package
   (name "stumpwm-contrib-maildir")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("modeline/maildir/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/maildir")
   (synopsis "Display maildir information in StumpWM's modeline")
   (description "StumpWM-contributed module to display the tracked email
accounts maildir state in StumpWM's modeline.")
   (license license:gpl3)))

(define-public stumpwm-contrib-mem
  (package
   (name "stumpwm-contrib-mem")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("modeline/mem/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/mem")
   (synopsis "Display system memory information in StumpWM's modeline")
   (description "StumpWM-contributed module to display the device's memory usage
in StumpWM's modeline.")
   (license license:gpl3)))

(define-public stumpwm-contrib-net
  (package
   (name "stumpwm-contrib-net")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("modeline/net/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/net")
   (synopsis "Display information about the current network connection in
StumpWM's modeline")
   (description "StumpWM-contributed module to display the information about the
device's current network connection in StumpWM's modeline.")
   (license license:gpl3)))

(define-public stumpwm-contrib-wifi
  (package
   (name "stumpwm-contrib-wifi")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("modeline/wifi/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/wifi")
   (synopsis "Display information about WiFi in StumpWM's modeline")
   (description "StumpWM-contributed module to display the device's WiFi
connections in StumpWM's modeline.")
   (license license:gpl3)))

(define-public stumpwm-contrib-stumptray
  (package
   (name "stumpwm-contrib-stumptray")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("modeline/stumptray/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/stumptray")
   (synopsis "System tray for StumpWM's modeline")
   (description "StumpWM-contributed module to present a system tray to the user
in StumpWM's modeline.")
   (license license:gpl3)))

(define-public stumpwm-contrib-clipboard-history
  (package
   (name "stumpwm-contrib-clipboard-history")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("util/clipboard-history/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/util/clipboard-history")
   (synopsis "Simple clipboard history module for StumpWM")
   (description "Simple clipboard history module for StumpWM")
   (license license:gpl3)))

(define-public stumpwm-contrib-pass
  (package
   (name "stumpwm-contrib-pass")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("util/pass/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/util/pass")
   (synopsis "Integrate the @code{pass} program with StumpWM")
   (description "Integrate the @code{pass} program with StumpWM")
   (license license:gpl3)))

(define-public stumpwm-contrib-screenshot
  (package
   (name "stumpwm-contrib-screenshot")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("util/screenshot/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/util/screenshot")
   (synopsis "Takes screenshots and stores them as @code{.png} files")
   (description "Takes screenshots and stores them as @code{.png} files")
   (license license:gpl3)))

(define-public stumpwm-contrib-stump-backlight
  (package
   (name "stumpwm-contrib-stump-backlight")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("util/stump-backlight/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/util/stump-backlight")
   (synopsis "Native backlight control from StumpWM")
   (description "Native backlight control from StumpWM")
   (license license:gpl3)))

(define-public stumpwm-contrib-stump-nm
  (package
   (name "stumpwm-contrib-stump-nm")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("util/stump-nm/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/util/stump-nm")
   (synopsis "StumpWM integration with NetworkManager")
   (description "StumpWM integration with NetworkManager")
   (license license:gpl3)))

(define-public stumpwm-contrib-windowtags
  (package
   (name "stumpwm-contrib-windowtags")
   (version stumpwm-contrib-version)
   (source stumpwm-contrib-source)
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("util/windowtags/" ""))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/util/windowtags")
   (synopsis "Add metadata to StumpWM windows to manipulate them en mass")
   (description "Add metadata to StumpWM windows to manipulate them en mass")
   (license license:gpl3)))

