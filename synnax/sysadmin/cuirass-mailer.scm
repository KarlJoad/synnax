(define-module (synnax sysadmin cuirass-mailer)
  #:use-module (guix gexp)
  #:use-module (guix packages)
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
  (computed-file "cuirass-mailer.conf"
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

