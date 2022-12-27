(define-module (synnax packages scripts)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-13)
  #:use-module (gnu packages xorg))

(define-public fix-desktop-monitors
  (package
   (name "fix-desktop-monitors")
   (version "git")
   (source #f)
   (native-inputs `(("xrandr" ,xrandr) ("monitor-script" ,monitor-script)))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils) (srfi srfi-13))
      #:builder
      (begin
        (use-modules (guix build utils)
                     (srfi srfi-13))
        (let* ((bin (string-append (assoc-ref %outputs "out") "/bin"))
               (monitor-script (assoc-ref %build-inputs "monitor-script"))
               (target-script-file (string-append bin "/" "fix-desktop-monitors.scm")))
          (mkdir-p bin)
          (symlink monitor-script target-script-file)))))
   (home-page "https://github.com/KarlJoad/synnax")
   (synopsis "Script that fixes my desktop monitors")
   (description "Uses @{code} xrandr to change the display configuration, forcing
my monitors to work correctly.")
   (license #f)))

(define monitor-script
  (program-file "fix-desktop-monitors.scm"
                (with-imported-modules
                 `((srfi srfi-13))
                 #~(begin
                     (use-modules (srfi srfi-13))
                     (define* (xrandr-cmd output #:optional (extra ""))
                       (let* ((xrandr-bin (string-append #$xrandr "/bin/xrandr"))
                              (cmd-to-run (string-join `(,xrandr-bin "--output" ,output ,extra) " ")))
                           (format #t "~a~%" (string-append cmd-to-run))
                           (system cmd-to-run)))
                     (xrandr-cmd "DVI-D-1" "--mode 1280x1024")
                     (xrandr-cmd "DVI-D-1" "--mode 1600x900")
                     (xrandr-cmd "HDMI-1" "--left-of DVI-D-1")))))
