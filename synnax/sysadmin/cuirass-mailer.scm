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

(define-public cuirass-mailer-script)
