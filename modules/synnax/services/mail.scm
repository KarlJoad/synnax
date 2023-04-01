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
            home-mbsync-configuration))


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
