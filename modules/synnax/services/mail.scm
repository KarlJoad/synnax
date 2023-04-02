(define-module (synnax services mail)
  #:use-module (guix gexp)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services mcron)
  #:use-module (gnu packages mail)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services mcron)

  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:export (home-mbsync-service-type
            home-mbsync-configuration

            home-mu-service-type
            home-mu-configuration

            home-msmtp-service-type
            home-msmtp-configuration
            home-msmtp-account-configuration))


;;;
;;; isync/mbsync
;;;
(define (serialize-mbsync-config field-name val)
  "Serialize the extra-config field of an home-mbsync-configuration item."
  (define (serialize-term term)
    "Serialize a single term."
    (match term
      ((? symbol? e) (symbol->string e))
      ((? number? e) (format #f "~a" e))
      ((? string? e) (format #f "~s" e))
      (e e)))
  (define (serialize-item entry)
    "Serialize an item (list of terms or gexp) into a single string."
    (match entry
      ((? gexp? e) e)
      ((? list lst) #~(string-join '#$(map serialize-term lst)))))

  #~(string-append #$@(interpose (map serialize-item val) "\n" 'suffix)))

(define mbsync-serialize-file-like serialize-file-like)
(define (mbsync-serialize-list field-name list)
  (serialize-mbsync-config field-name list))
(define (mbsync-serialize-boolean field-name b)
  (if b "yes" "no"))
(define (mbsync-serialize-number field-name n)
  (number->string n))
(define (mbsync-serialize-string field-name s)
  (format #f "~s" s))

(define-maybe boolean (prefix mbsync-))
(define-maybe number (prefix mbsync-))
(define-maybe string (prefix mbsync-))

(define-configuration home-mbsync-configuration
  (package
    (file-like isync)
    "@code{isync}/@code{mbsync} package to use.")
  (use-xdg?
   (boolean #t)
   "Whether to use the XDG specification for the isync/mbsync's configuration
file, @file{$XDG_CONFIG_HOME/isync/mbsyncrc}.")
  (extra-config
   (list '())
   "List-of-lists of configurations. The first element in each list must be a
configuration term for mbsync.")
  (auto-sync?
   (boolean #t)
   "Whether to create an mcron job to periodically fetch your email.")
  (interval
   (maybe-number (* 60 5))
   "Interval for which to run the automatically-syncing mbsync job, in seconds.")
  (post-sync-cmd
   (maybe-string "")
   "Command to run after mbsync completes its mail fetching.")
  (prefix mbsync-))

(define (add-mbsync-package config)
  "Adds the isync/mbsync package to the profile, installing it, and making it
available for use."
  (list (home-mbsync-configuration-package config)))

(define (get-mbsync-configuration mbsync-config)
  "Get the serialized MBSYNC-CONFIGURATION file."
  `((,(if (home-mbsync-configuration-use-xdg? mbsync-config)
          "isync/mbsyncrc"
          ".mbsyncrc")
     ,(mixed-text-file
       "mbsyncrc"
       (serialize-mbsync-config #f (home-mbsync-configuration-extra-config mbsync-config))))))

(define (add-mbsync-dot-configuration mbsync-config)
  "Link the built mbsync configuration to the user's home directory, naming
the resulting file $HOME/.mbsyncrc."
  (if (home-mbsync-configuration-use-xdg? mbsync-config)
      '()
      (get-mbsync-configuration mbsync-config)))

(define (add-mbsync-xdg-configuration mbsync-config)
  "Link the built mbsync configuration to the user's $XDG_CONFIG_HOME directory,
naming the resulting file $XDG_CONFIG_HOME/isync/mbsyncrc."
  (if (home-mbsync-configuration-use-xdg? mbsync-config)
      (get-mbsync-configuration mbsync-config)
      '()))

(define (home-mbsync-extensions cfg extensions)
  (home-mbsync-configuration
   (inherit cfg)
   (extra-config (append (home-mbsync-configuration-extra-config cfg) extensions))))

(define (home-mbsync-periodic-fetch-job mbsync-configuration)
  (let ((mbsync-package (home-mbsync-configuration-package mbsync-configuration)))
    #~(job (lambda (current-time)
             (+ current-time #$(home-mbsync-configuration-interval mbsync-configuration)))
           #$(file-append mbsync-package "/bin/mbsync"))))

(define home-mbsync-service-type
  (service-type (name 'home-mbsync)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        add-mbsync-package)
                       ;; Only one of add-mbsync-dot-configuration or add-mbsync-xdg-configuration
                       ;; will ever be used, depending on the value of the use-xdg? field.
                       (service-extension
                        home-files-service-type
                        add-mbsync-dot-configuration)
                       (service-extension
                        home-xdg-configuration-files-service-type
                        add-mbsync-xdg-configuration)
                       ;; TODO: Get periodic fetching working.
                       ;; (service-extension
                       ;;  home-mcron-service-type
                       ;;  home-mbsync-periodic-fetch-job)
                       ;; Extend home-mcron-service-type's job list with this job
                       ;; See (gnu home services mcron)'s home-mcron-extend procedure?
                       ))
                (compose concatenate)
                (extend home-mbsync-extensions)
                (default-value (home-mbsync-configuration))
                (description "Install and configure isync/mbsync, along with an
optional periodic task")))

(define (generate-home-mbsync-documentation)
  (generate-documentation `((home-mbsync-configuration ,home-mbsync-configuration-fields))
                          'home-mbsync-configuration))


;;;
;;; mu
;;;
;; TODO: An activation script that is run on the first time mu is used, but we
;; could also skip that and print a message to the user about it.
(define mu-serialize-file-like serialize-file-like)

(define-configuration home-mu-configuration
  (package
    (file-like mu)
    "@code{mu} package to use.")
  (prefix mu-))

(define (add-mu-package config)
  "Adds the mu package to the profile, installing it, and making it available for
use."
  (list (home-mu-configuration-package config)))

(define home-mu-service-type
  (service-type (name 'home-mu)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        add-mu-package)))
                (compose concatenate)
                (extend #f) ;; TODO: Use other extension function?
                (default-value (home-mu-configuration))
                (description "Install and configure mu, a mail indexing software")))

(define (generate-home-mu-documentation)
  (generate-documentation `((home-mu-configuration ,home-mu-configuration-fields))
                          'home-mu-configuration))


;;;
;;; msmtp
;;;
(define (msmtp-serialize-string field-name val)
  (format #f "~s ~s" field-name val))
(define (msmtp-serialize-number field-name val)
  (format #f "~s ~s" field-name (number->string val)))
(define (msmtp-serialize-boolean field-name val)
  (format #f "~s ~s" field-name (if val "on" "off")))

(define-configuration home-msmtp-account-configuration
  (account
   (string "")
   "Name this accound should use.")
  (authorization
   (string "")
   "Authorization")
  (from-email
   (string "")
   "Email address that should be used as from.")
  (host
   (string "")
   "SMTP host to synchronize with. Hostname or IP address works.")
  (user
   (string "")
   "User to log in as")
  (pass-cmd
   (string "") ;; #$(file-append coreutils /bin/cat) "/home/karljoad/<pw-file>"
   "Command to use to get the password for this account.")
  (port
   (number 587)
   "Port number to use with SMTP.")
  (protocol
   (string "smtp")
   "Protocol to use when synchronizing mail.")
  (tls?
   (boolean #t)
   "Should TLS be used?")
  (starttls?
   (boolean #t)
   "Should Start TLS be used?")
  (tls-trust-file
   (string "/etc/ssl/certs/ca-certificates.crt") ;; Find package /etc/ssl/certs/ca-certificates.crt comes from
   "Certificate Authority certificates file.")
  ;; We want to use the same serialization procedures as msmtp.
  (prefix msmtp-))

;; TODO: Serialize the msmtprc config file
(define msmtp-serialize-file-like serialize-file-like)

(define-configuration home-msmtp-configuration
  (package
    (file-like msmtp)
    "@code{msmtp} package to use.")
  (use-defaults?
   (boolean #t)
   "Should sensible defaults be used for all accounts?")
  (prefix msmtp-))

(define (add-msmtp-package config)
  "Adds the msmtp package to the profile, installing it, and making it available for
use."
  (list (home-msmtp-configuration-package config)))

(define (home-msmtp-extensions cfg extensions)
  (home-msmtp-configuration
   (inherit cfg)
   (accounts (append (home-msmtp-configuration-accounts cfg) extensions))))

(define home-msmtp-service-type
  (service-type (name 'home-msmtp)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        add-msmtp-package)
                       ;; (service-extension
                       ;;  home-xdg-configuration-files-service-type
                       ;;  add-msmtp-xdg-configuration)
                       ))
                (compose concatenate)
                (extend home-msmtp-extensions)
                (default-value #f) ;; We require users to provide a configuration themselves
                (description "Install and configure msmtp, a SMTP client to send
mail")))

(define (generate-home-msmtp-documentation)
  (generate-documentation `((home-msmtp-configuration ,home-msmtp-configuration-fields))
                          'home-msmtp-configuration))
