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
            home-mbsync-channel-configuration
            home-mbsync-group-configuration
            home-mbsync-maildir-store-configuration
            home-mbsync-imap-store-configuration

            home-mu-service-type
            home-mu-configuration

            home-msmtp-service-type
            home-msmtp-configuration
            home-msmtp-account-configuration))


;;;
;;; isync/mbsync
;;;
(define mbsync-serialize-file-like serialize-file-like)
(define (mbsync-serialize-list field-name list)
  (serialize-mbsync-config field-name list))
(define (mbsync-serialize-boolean field-name b)
  (if b "yes" "no"))
(define (mbsync-serialize-number field-name n)
  (format #f "~a ~a" field-name (number->string n)))
(define (mbsync-serialize-string field-name val)
    (if (string-any char-whitespace? val)
        (format #f "~a ~s" field-name val)
        (format #f "~a ~a" field-name val)))

;; The channel is the simplest thing that isync/mbsync deals with. A channel
;; maps a remote/far mail server's directory to a local/near directory.
;; mbsync fetches channels, but users should not fetch channels themselves.
;; Instead, users should go through groups, see home-mbsync-group-configuration.
(define-configuration/no-serialization home-mbsync-channel-configuration
  ;; Most of the actual configuration options for channels are omitted here, as
  ;; I do not need it, though they could be added easily enough. See
  ;; https://isync.sourceforge.io/mbsync.html (under Channels) for all possible
  ;; options. Many of them are used to override global defaults that I prefer to
  ;; keep.
  (name
   string
   "Name for this channel.")
  (far
   string
   "Pattern on @emph{remote/far} server for this channel.")
  (near
   string
   "Pattern for @emph{local/near} mail store for storing this channel's mail."))

(define (serialize-home-mbsync-channel-configuration config group-name
                                                     far-store-name near-store-name)
  "Both far-store-name and near-store-name must be the full name of the store.
For example, for an account with a group named \"account\", group with name
\"group\" and a remote IMAP store named \"remote\", far-store-name must be \"account-remote\"."
  (define (build-fq-channel-target store dir-name) (format #f ":~a:~a" store dir-name))
  (let* ((channel-name (home-mbsync-channel-configuration-name config))
        ;; fully-qualified-far-name
        (far-name (home-mbsync-channel-configuration-far config))
        (near-name (home-mbsync-channel-configuration-near config))
        (fq-far-name (build-fq-channel-target far-store-name far-name))
        (fq-near-name (build-fq-channel-target near-store-name near-name)))
    (string-join
     (list
      (mbsync-serialize-string "Channel" (string-append group-name "-" channel-name))
      (mbsync-serialize-string "Far" fq-far-name)
      (mbsync-serialize-string "Near" fq-near-name))
      "\n"
      'infix)))

;; The group is the lowest-level (finest-granularity) a user has for synchronization
;; with isync/mbsync. A group is just a named collection of channels.
(define (list-of-home-mbsync-channel-configurations? lst)
  (every home-mbsync-channel-configuration? lst))

(define-configuration/no-serialization home-mbsync-group-configuration
  (name
   string
   "Name for this grouping of channels.
The group is the finest granularity of synchronization that is supported by
@code{mbsync}. You can synchronize groups, but not single channels.")
  (channels
   (list-of-home-mbsync-channel-configurations '())
   "List of @code{home-mbsync-channel-configuration}s to put in this group.
NOTE: You @emph{can} have the same channel be in multiple groups!"))

(define (serialize-home-mbsync-group-configuration config far-store-name near-store-name)
  ;; FIXME: Serialization should produce "" if channels list is empty.
  (let ((group-name (home-mbsync-group-configuration-name config)))
    (define (make-channel-group-member channel-name) (format #f "~a-~a" group-name channel-name))
    (string-join `(,@(map (lambda (channel-config)
                            (serialize-home-mbsync-channel-configuration channel-config
                                                                         group-name far-store-name near-store-name))
                          (home-mbsync-group-configuration-channels config))
                   ""
                   ,(mbsync-serialize-string "Group" group-name)
                   ,@(map (lambda (channel-config)
                            (mbsync-serialize-string "Channel" (make-channel-group-member
                                                                (home-mbsync-channel-configuration-name channel-config))))
                          (home-mbsync-group-configuration-channels config)))
                 "\n" 'suffix)))

(define-configuration/no-serialization home-mbsync-maildir-store-configuration
  (name
   (string "local")
   "Name of this maildir store.")
  (inbox
   string
   "Path to inbox for this account. No trailing @code{/} is allowed.")
  (path
   string
   "Path to maildir directory for this account. Trailing @code{/} is required.")
  (subfolders
   (string "Verbatim") ;; TODO: Should be enum: verbatim, maildir++, or legacy
   "How to name folders on-disk for hierarchical mailboxes."))

(define (serialize-home-mbsync-maildir-store md-store-config account-name)
  (let ((md-store-name (home-mbsync-maildir-store-configuration-name md-store-config))
        (md-store-inbox (home-mbsync-maildir-store-configuration-inbox md-store-config))
        (md-store-path (home-mbsync-maildir-store-configuration-path md-store-config)))
    (when (not (equal? "/" (string-take-right md-store-path 1)))
      (error-out (format #f "Maildir store path for ~a must end in a /" md-store-name)))
    (when (equal? "/" (string-take-right md-store-inbox 1))
      (error-out (format #f "Maildir Inbox path for ~a cannot end with a /" md-store-name)))
    (string-append
     (mbsync-serialize-string "MaildirStore" (string-append account-name "-" md-store-name))
     "\n"
     (mbsync-serialize-string "Inbox" md-store-inbox) "\n"
     (mbsync-serialize-string "Path" md-store-path) "\n"
     (mbsync-serialize-string "SubFolders" (home-mbsync-maildir-store-configuration-subfolders md-store-config)))))

(define-configuration/no-serialization home-mbsync-imap-store-configuration
  (name
   (string "remote")
    "Name of this IMAP store, after being appended to the account name (see
@code{home-mbsync-account-configuration}."))

(define (serialize-home-mbsync-imap-store imap-store-config account-name)
  "Serialize an mbsync IMAP (remote) mail store configuration for a given
ACCOUNT-NAME."
  (let ((imap-store-name (home-mbsync-imap-store-configuration-name imap-store-config)))
    (string-append
     (mbsync-serialize-string "IMAPStore" (string-append account-name "-" imap-store-name))
     "\n" ;; TODO: Use something better than in-line "\n"?
     (mbsync-serialize-string "Account" account-name))))

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
  (format #f "~a ~a" field-name val))
(define (msmtp-serialize-number field-name val)
  (format #f "~a ~a" field-name (number->string val)))
(define (msmtp-serialize-boolean field-name val)
  (format #f "~a ~a" field-name (if val "on" "off")))

(define (serialize-home-msmtp-account-configuration-pass-cmd field-name val)
  (format #f "passwordeval ~a" val))
(define (home-msmtp-serialize-tls field-name val)
  (msmtp-serialize-boolean "tls" val))
(define (home-msmtp-serialize-starttls field-name val)
  (msmtp-serialize-boolean "tls_starttls" val))
(define (home-msmtp-serialize-tls-trust-file field-name val)
  (msmtp-serialize-string "tls_trust_file" val))

(define-configuration home-msmtp-account-configuration
  (account
   (string "")
   "Name this accound should use.")
  (auth
   (string "")
   "Authentication mechanism to use.")
  (from
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
   "Command to use to get the password for this account."
   serialize-home-msmtp-account-configuration-pass-cmd)
  (port
   (number 587)
   "Port number to use with SMTP.")
  (protocol
   (string "smtp")
   "Protocol to use when synchronizing mail.")
  (tls?
   (boolean #t)
   "Should TLS be used?"
   home-msmtp-serialize-tls)
  (starttls?
   (boolean #t)
   "Should Start TLS be used?"
   home-msmtp-serialize-starttls)
  (tls-trust-file
   (string "/etc/ssl/certs/ca-certificates.crt") ;; Find package /etc/ssl/certs/ca-certificates.crt comes from
   "Certificate Authority certificates file."
   home-msmtp-serialize-tls-trust-file)
  ;; We want to use the same serialization procedures as msmtp.
  (prefix msmtp-))

;; TODO: Serialize the msmtprc config file
(define msmtp-serialize-file-like serialize-file-like)

(define (list-of-msmtp-accounts? lst)
  (every home-msmtp-account-configuration? lst))
(define (msmtp-serialize-list-of-msmtp-accounts field-name val)
  (format #f "test-list ~s => ~a" field-name val))
(define (msmtp-serialize-default-account field-name val)
  (if (not (equal? val %unset-value))
      (format #f "account default : ~a" val)
      ""))

(define-configuration home-msmtp-configuration
  (package
    (file-like msmtp)
    "@code{msmtp} package to use.")
  (queue-dir
   (string (string-append (getenv "HOME") "/.msmtpqueue"))
   ;; An alternative $XDG_DATA_HOME/msmtpqueue
   "Directory where emails queued to send (with msmtpqueue) will be stored before
being sent later.")
  (use-defaults?
   (boolean #t)
   "Should sensible defaults be used for all accounts?")
  (tls?
   (boolean #t)
   "Should TLS be used for all MSMTP accounts?"
   home-msmtp-serialize-tls)
  (starttls?
   (boolean #t)
   "Should Start TLS be used for all MSMTP accounts?"
   home-msmtp-serialize-starttls)
  (tls-trust-file
   (string "/etc/ssl/certs/ca-certificates.crt") ;; Find package /etc/ssl/certs/ca-certificates.crt comes from
   "Certificate Authority certificates file."
   home-msmtp-serialize-tls-trust-file)
  (accounts
   (list-of-msmtp-accounts '())
   "List of MSMTP accounts to send with.")
  (default-account
    maybe-string
    "Account name that should be used as the default by MSMTP. This must match
one of the @code{account} fields for a @code{home-msmtp-account-configuration}
instance."
    msmtp-serialize-default-account)
  (prefix msmtp-))

(define (add-msmtp-package config)
  "Adds the msmtp package to the profile, installing it, and making it available for
use."
  (list (home-msmtp-configuration-package config)))

(define (serialize-home-msmtp-account-configuration account-config)
  "Serialize the Scheme configuration of an MSMTP account to a configuration
file, in MSMTP's format."
  ;; (serialize-configuration account-config home-msmtp-account-configuration-fields)
  ;; Requires that each serialization function define a newline at the end
  (string-join (map (lambda (field)
                      ((configuration-field-serializer field)
                       (configuration-field-name field)
                       ((configuration-field-getter field) account-config)))
                    home-msmtp-account-configuration-fields)
               "\n" 'suffix))

(define (serialize-home-msmtp-configuration config)
  "Serialize the Scheme configuration of MSMTP to a configuration file, in
MSMTP's format."
  #~(string-append
     "# Generated by Guix Home.\n\n"
     "# Global Settings\n"
     #$(if (home-msmtp-configuration-use-defaults? config) "defaults\n" "")
     #$(msmtp-serialize-boolean "tls" (home-msmtp-configuration-tls? config)) "\n"
     #$(msmtp-serialize-boolean "tls_starttls" (home-msmtp-configuration-starttls? config)) "\n"
     #$(msmtp-serialize-string "tls_trust_file" (home-msmtp-configuration-tls-trust-file config)) "\n"
     "\n"
     #$(string-join (map serialize-home-msmtp-account-configuration
                         (home-msmtp-configuration-accounts config))
                    "\n" 'suffix)
     #$(msmtp-serialize-default-account #f (home-msmtp-configuration-default-account config))))

(define (add-msmtp-xdg-configuration config)
  "Link the built msmtp configuration to the $XDG_CONFIG_HOME directory,
naming the resuling file @file{$XDG_CONFIG_HOME/msmtp/config}."
  `(("msmtp/config"
     ,(mixed-text-file
       "msmtp-config"
       (serialize-home-msmtp-configuration config)))))

(define (add-msmtp-queue-dir-env-var config)
  "Set MSMTP's $QUEUEDIR for delayed sending of emails."
  `(("QUEUEDIR" . ,(home-msmtp-configuration-queue-dir config))))

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
                       (service-extension
                        home-xdg-configuration-files-service-type
                        add-msmtp-xdg-configuration)
                       (service-extension
                        home-environment-variables-service-type
                        add-msmtp-queue-dir-env-var)))
                (compose concatenate)
                (extend home-msmtp-extensions)
                (default-value #f) ;; We require users to provide a configuration themselves
                (description "Install and configure msmtp, a SMTP client to send
mail")))

(define (generate-home-msmtp-documentation)
  (generate-documentation `((home-msmtp-configuration ,home-msmtp-configuration-fields))
                          'home-msmtp-configuration))
