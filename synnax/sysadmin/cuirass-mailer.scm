(define-module (synnax sysadmin cuirass-mailer)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages gnupg))

;; mailutils, sendmail, and msmtp may be needed?
;; Cuirass uses the sendmail command.
;; msmtp can be used to send the email I guess?
;; I don't know if mailutils is needed at all...

;; Guix CI cuirass-mailer script @ /var/cuirass/cuirass-mailer
;; #!/bin/sh
;; torsocks msmtp --file=/var/cuirass/cuirass-mailer.rc $@
;; invoked with sendmail:///var/cuirass/cuirass-mailer by cuirass

(define (mailer-config gnupg-pkg)
  (mixed-text-file "cuirass-mailer.conf"
                   "defaults
\ttls on
\ttls_starttls on
\ttls_trust_file /run/current-system/profile/etc/ssl/certs/ca-certificates.crt

account default
\tauth login
\tfrom cuirass@hallsby.com
\tprotocol smtp
\thost smtp.gmail.com
\tport 587
\ttimeout 60
\tuser karl@hallsby.com
\tpasswordeval " gnupg-pkg "/bin/gpg " "--quiet " "--for-your-eyes-only " "--no-tty " "--decrypt " "/etc/cuirass/mailer-pass"))

(define-public cuirass-mailer-config
  (let ((version "git")
        (revision "0"))
    (package
     (name "cuirass-mailer-config")
     (version (string-append version "-" revision))
     (source #f)
     (build-system trivial-build-system)
     (native-inputs `(("config" ,(mailer-config gnupg))))
     (arguments
      `(#:modules ((guix build utils))
        #:builder
        (begin
          (use-modules (guix build utils))
          (let* ((mailer-config (assoc-ref %build-inputs "config"))
                 (install-target (assoc-ref %outputs "out")))
            (symlink mailer-config install-target)))))
     (home-page "https://github.com/KarlJoad/synnax")
     (synopsis "Configuration file for Cuirass emailer")
     (description "Configuration file for Cuirass emailer.
Connects to my personal email account and sends emails when a Cuirass evaluation
starts failing or builds start failing.")
     (license #f))))

;; "Build the email script that Cuirass calls with the CONFIG provided."
(define (mailer-script config-pkg)
  (program-file
   "cuirass-mailer-script.scm"
   (with-imported-modules
    `((ice-9 match)
      (srfi srfi-1)
      (srfi srfi-13))
    #~(begin
        (use-modules (ice-9 match) (srfi srfi-1) (srfi srfi-13))
        (define (flatten lst)
          ;; flatten from https://git.savannah.nongnu.org/cgit/grip.git/tree/grip/list.scm#n73
          (let loop ((item lst)
                     (result '()))
            (match item
              (() result)
              ((elt . rests) (loop elt (loop rests result)))
              (else (cons item result)))))
        (let* ((msmtp-bin #$(file-append msmtp "/bin/msmtp"))
               (config #$config-pkg)
               ;; cdr of command-line to remove Guile binary at front of list
               (body (cdr (command-line))))
          (format #t "~a --file=~a ~a ~a ~%" msmtp-bin config "--read-recipients" (flatten body))
          (apply system* `(,msmtp-bin ,(string-append "--file=" config) "--read-recipients" ,@(flatten body))))))))

(define-public cuirass-mailer-script
  (let ((version "git") ; FIXME: Use "git" or "synnax" for channel-only packages?
        (revision "0"))
    (package
     (name "cuirass-mailer-script")
     (version (string-append version "-" revision))
     (source #f)
     (inputs `(("msmtp" ,msmtp) ("mailer-script" ,(mailer-script cuirass-mailer-config))))
     (build-system trivial-build-system)
     (arguments
      `(#:modules ((guix build utils))
        #:builder
        (begin
          (use-modules (guix build utils))
          (let ((mailer-script (assoc-ref %build-inputs "mailer-script"))
                (install-target (assoc-ref %outputs "out")))
            (symlink mailer-script install-target)))))
     (home-page "https://github.com/KarlJoad/synnax")
     (synopsis "Script for Cuirass emailer")
     (description "Script for Cuirass emailer.
Uses the Cuirass mailer configuration to send emails when a Cuirass evaluation
starts failing or builds start failing.")
     (license #f))))
