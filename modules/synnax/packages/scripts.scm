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
   (description "Uses @code{xrandr} to change the display configuration, forcing
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
                     (xrandr-cmd "HDMI-1" "--left-of DP-1")
                     (xrandr-cmd "HDMI-1" "--mode 1920x1080 --rate 144.00")
                     (xrandr-cmd "DP-1" "--mode 1920x1080 --rate 144.00")))))

(define-public fix-office-monitors
  (package
   (name "fix-office-monitors")
   (version "git")
   (source #f)
   (native-inputs `(("xrandr" ,xrandr) ("monitor-script" ,office-monitor-script)))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils) (srfi srfi-13))
      #:builder
      (begin
        (use-modules (guix build utils)
                     (srfi srfi-13))
        (let* ((bin (string-append (assoc-ref %outputs "out") "/bin"))
               (monitor-script (assoc-ref %build-inputs "monitor-script"))
               (target-script-file (string-append bin "/" "fix-office-monitors.scm")))
          (mkdir-p bin)
          (symlink monitor-script target-script-file)))))
   (home-page "https://github.com/KarlJoad/synnax")
   (synopsis "Script that fixes my office monitors")
   (description "Uses @code{xrandr} to change the display configuration, forcing
my monitors to work correctly.")
   (license #f)))

(define office-monitor-script
  (program-file "fix-office-monitors.scm"
                (with-imported-modules
                 `((srfi srfi-13))
                 #~(begin
                     (use-modules (srfi srfi-13))
                     (define* (xrandr-cmd output #:optional (extra ""))
                       (let* ((xrandr-bin (string-append #$xrandr "/bin/xrandr"))
                              (cmd-to-run (string-join `(,xrandr-bin "--output" ,output ,extra) " ")))
                           (format #t "~a~%" (string-append cmd-to-run))
                           (system cmd-to-run)))
                     (xrandr-cmd "HDMI-1" "--right-of DP-1")
                     (xrandr-cmd "HDMI-1" "--mode 1920x1080 --rate 60.00")
                     (xrandr-cmd "DP-1" "--mode 1920x1080 --rate 60.00")))))

(define-public last-reconfigure-date
  (package
   (name "last-reconfigure-date")
   (version "git")
   (source #f)
   (native-inputs `(("last-reconfigure-date-script" ,last-reconfigure-date-script)))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils) (srfi srfi-13))
      #:builder
      (begin
        (use-modules (guix build utils)
                     (srfi srfi-13))
        (let* ((bin (string-append (assoc-ref %outputs "out") "/bin"))
               (script-source (assoc-ref %build-inputs "last-reconfigure-date-script"))
               (target-script-file (string-append bin "/" "last-reconfigure-date.scm")))
          (mkdir-p bin)
          (symlink script-source target-script-file)))))
   (home-page "https://github.com/KarlJoad/synnax")
   (synopsis "Script that displays when the system and home were last reconfigured")
   (description "Read from /var/guix/profiles/ to determine the last time the
most recent system and per-user home profile were built.")
   (license #f)))

;; TODO: Make more generic.
;; Should accept a username parameter for /var/guix/profiles/per-user
;; Should accept an enum between home or system
;; home = /var/guix/profiles/per-user/<username>/guix-home
;; system = /var/guix/profiles/system
(define last-reconfigure-date-script
  (program-file "last-reconfigure-date"
                (with-imported-modules
                    `((srfi srfi-19))
                  #~(begin
                      (use-modules (srfi srfi-19))
                      (define (file-ctime file-path)
                        (let* ((file-info (stat file-path))
                               (file-ctime (stat:ctime file-info))
                               (epoch-secs (make-time time-monotonic 0 file-ctime))
                               (file-cdate (time-monotonic->date epoch-secs)))
                          (format #f "~a"
                                  (date->string file-cdate "~c"))))
                      (let ((user-name-running (passwd:name (getpwnam (getuid)))))
                        (format #t "Last reconfigure dates:~%")
                        (format #t "System:\t~a~%" (file-ctime "/var/guix/profiles/system"))
                        (format #t "Home:\t~a~%"
                                (file-ctime
                                 (format #f "/var/guix/profiles/per-user/~a/guix-home" user-name-running))))))))
