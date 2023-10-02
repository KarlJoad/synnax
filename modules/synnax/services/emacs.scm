(define-module (synnax services emacs)
  #:use-module (guix gexp)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (synnax services home utils)
  #:export (home-emacs-server-service-type
            home-emacs-server-configuration))

(define package? file-like?)

(define-configuration/no-serialization home-emacs-server-configuration
  (package
   (package emacs)
   "Emacs package to use in service.")
  (server-name
   (string "")
   "Name for Emacs server to use."))

(define (home-emacs-server-service config)
  (define emacs-package (home-emacs-server-configuration-package config))
  (define server-name (home-emacs-server-configuration-server-name config))

  (define (server-name->symbol server-name)
    (let ((emacs-server-symbol 'emacs-server))
      (if (string-null? server-name)
          emacs-server-symbol
          (symbol-append emacs-server-symbol (string->symbol "-")
                         (string->symbol server-name)))))
  (define (server-name->server-socket-name server-name)
    (if (string-null? server-name) "server" server-name))

  (list
   (shepherd-service
    (provision (list
                (server-name->symbol server-name)));; (server-name-suffix server-name))))
    (requirement '())
    (respawn? #t)
    (start #~(make-forkexec-constructor
              (list #$(file-append emacs-package "/bin/emacs")
                    #$(format #f "--fg-daemon=~a"
                              (server-name->server-socket-name server-name)))
              #:log-file #$(home-service-log-file-path
                            (format #f "emacs-~a"
                                    (server-name->server-socket-name server-name)))))
    (stop ;; #~(make-kill-destructor)
     #~(make-system-destructor
        #$(file-append emacs-package "/bin/emacsclient" " "
                       (format #f "--socket-name=~a"
                               (server-name->server-socket-name server-name))
                       " " "--eval '(kill-emacs)'")))
    (documentation (string-append "Emacs server"
                                  (if (string-null? server-name)
                                      ""
                                      (string-append " for " server-name)))))))

(define home-emacs-server-service-type
  (service-type (name 'home-emacs)
                (extensions
                 (list (service-extension
                        home-shepherd-service-type
                        home-emacs-server-service)))
                (default-value #f)
                (description "Run an Emacs server")))
