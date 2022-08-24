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

(define-public cuirass-mailer-config)

(define-public cuirass-mailer-script)
