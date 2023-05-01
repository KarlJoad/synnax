(define-module (synnax packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system asdf)
  #:use-module (gnu packages)
  #:use-module (gnu packages wm))

(define-public stumpwm-contrib
  (define stumpwm-contrib-git-branch "master")
  (define stumpwm-contrib-git-commit "d0c05077eca5257d33083de949c10bca4aac4242")
  (define stumpwm-contrib-revision "1")
  (define stumpwm-contrib-version
    (git-version stumpwm-contrib-git-branch stumpwm-contrib-revision
                 stumpwm-contrib-git-commit))
  (package
    (name "stumpwm-contrib")
    (version stumpwm-contrib-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stumpwm/stumpwm-contrib.git")
             (commit stumpwm-contrib-git-commit)))
       (file-name (git-file-name "stumpwm-contrib" version))
       (sha256
        (base32 "0zxhqh9wjfk7zas67kmwfx0a47y8rxmh8f1a5rcs300bv1083lkb"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       ;; Install the entire contents of stumpwm-contrib to the root of #$out
       '(("." ""))))
    (home-page "https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/battery-portable")
    (synopsis "Display battery information in StumpWM's modeline")
    (description "StumpWM-contributed module to portably be able to display the
device's battery levels in StumpWM's modeline.")
    (license license:gpl3)))

(define-public stumpwm+slynk+contrib
  (package
    (inherit stumpwm+slynk)
    (name "stumpwm-with-slynk-contrib")
    (inputs
     (append `(("stumpwm-contrib" ,stumpwm-contrib))
             (package-inputs stumpwm+slynk)))
    ;; Add a phase after create-asdf-configuration and before build-program that
    ;; replaces stumpwm:*module-dir*'s default of (concat (getenv "HOME") "/.stumpwm.d/modules")
    ;; with #$stumpwm-contrib's output.
    ))
