(define-module (synnax systems home))

(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu home services shepherd)
             (gnu home services mcron)
             (gnu home services ssh)
             (gnu services)
             (guix gexp)
             (synnax systems packages))

(use-package-modules
 emacs
 linux
 music
 vnc
 vpn
 fontutils)

(define (emacs-server server-name)
  (define (server-name->symbol server-name)
    (let ((emacs-server-symbol 'emacs-server))
      (if (string-null? server-name)
          emacs-server-symbol
          (symbol-append emacs-server-symbol (string->symbol "-")
                         (string->symbol server-name)))))
  (define (server-name->server-socket-name server-name)
    (if (string-null? server-name) "server" server-name))

  (shepherd-service
   (provision (list
               (server-name->symbol server-name)));; (server-name-suffix server-name))))
   (requirement '())
   (respawn? #t)
   (start #~(make-forkexec-constructor
             (list #$(file-append emacs "/bin/emacs")
                   #$(format #f "--fg-daemon=~a"
                             (server-name->server-socket-name server-name)))
             #:log-file (string-append
				                 (or (getenv "XDG_LOG_HOME")
				                     (format #f "~a/.local/var/log"
					                           (getenv "HOME")))
				                 #$(format #f "/emacs-~a.log"
                                   (server-name->server-socket-name server-name)))))
   (stop ;; #~(make-kill-destructor)
         #~(make-system-destructor
            #$(file-append emacs "/bin/emacsclient" " "
                           (format #f "--socket-name=~a"
                                   (server-name->server-socket-name server-name))
                                 " " "--eval '(kill-emacs)'"))
         )
   (documentation (string-append "Emacs server"
                                 (if (string-null? server-name)
                                     ""
                                     (string-append " for " server-name))))))

(define fontconfig-update-mcron-job
  #~(job '(next-hour '(10))
         (lambda ()
           (string-append #$fontconfig "/bin/fc-cache -fv"))
         "fontconfig-daily-update"))

(home-environment
 (packages
  (append
   (list ;; slack discord element-desktop
    )
   %nonguix-packages
   %desktop-home-packages
   %home-packages))
 (services
  (list
   (simple-service 'force-xdg-env-vars-service
                   home-environment-variables-service-type
                   `(("SQLITE_HISTORY" . "$XDG_CACHE_HOME/sqlite_history")))
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (aliases
              '(("grep='grep --color'" . "auto")
                  ("ll" . "'ls -la'")
                  ("ls='ls -p --color'" . "auto")))
             (bashrc
              (list (local-file "bashrc" "bashrc")))
             (bash-profile
              (list (local-file "bash_profile" "bash_profile")))))
   (service home-openssh-service-type
            (home-openssh-configuration
             (hosts
              (list (openssh-host (name "ci")
                                  (host-name "192.168.1.6")
                                  (user "root")
                                  (identity-file "~/.ssh/ci_rsa"))
                    (openssh-host (name "*github.com")
                                  (user "git")
                                  (identity-file "~/.ssh/github"))
                    (openssh-host (name "*.cs.northwestern.edu")
                                  (user "kgh0080")
                                  (identity-file "~/.ssh/nu"))))))
   (simple-service 'ssh-config
                   home-files-service-type
                   (list `(".config/git/config"
                           ,(plain-file "config"
                                        "[user]
\tname = Karl Hallsby
\temail = karl@hallsby.com"))
                         `(".config/git/ignore"
                           ,(plain-file "ignore"
                                        "*~
*.swp"))))
   (service home-shepherd-service-type
            (home-shepherd-configuration
             (auto-start? #t)
             (services
              (list
               (emacs-server "")))))
   (service home-mcron-service-type
            (home-mcron-configuration
             (jobs
              (list
               fontconfig-update-mcron-job))))
   )))