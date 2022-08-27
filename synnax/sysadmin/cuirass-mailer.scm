(define-module (synnax sysadmin cuirass-mailer)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages mail))

;; mailutils, sendmail, and msmtp may be needed?
;; Cuirass uses the sendmail command.
;; msmtp can be used to send the email I guess?
;; I don't know if mailutils is needed at all...

;; Guix CI cuirass-mailer script @ /var/cuirass/cuirass-mailer
;; #!/bin/sh
;; torsocks msmtp --file=/var/cuirass/cuirass-mailer.rc $@
;; invoked with sendmail:///var/cuirass/cuirass-mailer by cuirass

(define mailer-config
  (plain-file "cuirass-mailer.conf"
              "account default
\tprotocol smtp
\thost gmail.com
\tport 997
\ttimeout 60
\ttls on
\ttls_trust_file /run/current-system/profile/etc/ssl/certs/ca-certificates.crt
\tfrom cuirass@hallsby.com
\tauth login
\tuser karl@hallsby.com
\tpassword something"))

(define-public cuirass-mailer-config
  (let ((version "git")
        (revision "0"))
    (package
     (name "cuirass-mailer-config")
     (version (string-append version "-" revision))
     (source #f)
     (build-system trivial-build-system)
     (native-inputs `(("config" ,mailer-config)))
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
  ;; TODO: Use scheme script instead
  ;; (program-file "cuirass-mailer-script.scm"
  ;;               (with-imported-modules
  ;;                `((srfi srfi-13))
  ;;                #~(begin
  ;;                    (use-modules (srfi srfi-13))
  ;;                    (let ((msmtp (string-append #$msmtp "/bin/msmtp"))
  ;;                          (config)
  (computed-file "cuirass-mailer-script.sh"
                 (with-imported-modules
                  `((srfi srfi-13))
                  #~(begin
                      (use-modules (srfi srfi-13))
                      (let ((msmtp-bin #$(file-append msmtp "/bin/msmtp"))
                            (config #$config-pkg))
                        (string-append
                         "#!/bin/sh\n\n"
                         (string-join '(msmtp-bin
                                        (string-append "--file=" config)
                                        "$@")
                                      " ")))))))

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
      `(#:modules ((guix build utils) (srfi srfi-13))
        #:builder
        (begin
          (use-modules (guix build utils))
          (let ((mailer-script (assoc-ref %build-inputs "mailer-script"))
                (install-target (assoc-ref %outputs "out")))
            (symlink mailer-script install-target))
          )))
     (home-page "https://github.com/KarlJoad/synnax")
     (synopsis "Script for Cuirass emailer")
     (description "Script for Cuirass emailer.
Uses the Cuirass mailer configuration to send emails when a Cuirass evaluation
starts failing or builds start failing.")
     (license #f))))
