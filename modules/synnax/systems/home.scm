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
             (synnax systems packages)
             (synnax services dbus)
             (synnax services pipewire)
             (synnax services mail))

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
           #$(file-append fontconfig "/bin/fc-cache -fv"))
         "fontconfig-daily-update"))

(home-environment
 (packages
  (append
   (list ;; slack discord
    )
   %nonguix-packages
   %desktop-home-packages
   %home-packages))
 (services
  (list
   (service home-dbus-service-type)
   (service home-pipewire-service-type)
   (simple-service 'force-xdg-env-vars-service
                   home-environment-variables-service-type
                   `(("SQLITE_HISTORY" . "$XDG_CACHE_HOME/sqlite_history")))
   (simple-service 'language-env-vars-service
                   home-environment-variables-service-type
                   '(("LANGUAGE" . "en_US.utf8")
                     ("LC_ALL" . "en_US.utf8")))
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (environment-variables
              ;; See (bash) Bash Variables info for documentation.
              `(("HISTSIZE" . "10000")
                ("HISTCONTROL" . "ignoreboth:erasedups")))
             (aliases
              '(("grep='grep --color'" . "auto")
                ("ll" . "'ls -la'")
                ("ls='ls -p --color'" . "auto")))
             (bashrc
              (list (local-file "bashrc" "bashrc")
                    ;; NOTE: Adding direnv support should come last!
                    (plain-file "bashrc-add-direnv" "eval \"$(direnv hook bash)\"")))
             (bash-profile
              (list (local-file "bash_profile" "bash_profile")))))
   (service home-openssh-service-type
            (home-openssh-configuration
             (hosts
              (list (openssh-host (name "robin")
                                  (host-name "robin.raven")
                                  (user "root")
                                  (identity-file "~/.ssh/ci_rsa"))
                    (openssh-host (name "redhood")
                                  (host-name "redhood.raven")
                                  (user "root")
                                  (identity-file "~/.ssh/ci_rsa"))
                    (openssh-host (name "*github.com")
                                  (user "git")
                                  (identity-file "~/.ssh/github"))
                    (openssh-host (name "*.cs.northwestern.edu")
                                  (user "kgh0080")
                                  (identity-file "~/.ssh/nu"))))))
   (simple-service 'git-config-files
                   home-xdg-configuration-files-service-type
                   (list `("git/config"
                           ,(plain-file "config"
                                        "[user]
\tname = Karl Hallsby
\temail = karl@hallsby.com"))
                         `("git/ignore"
                           ,(plain-file "ignore"
                                        "*~
*.swp"))))
   (simple-service 'sbcl-config-files-service
                   home-files-service-type
                   (list `(".sbclrc"
                           ,(plain-file "sbclrc"
                                        ";; Generated by Guix Home.
;; To disable SBCL's loading of this file, pass --no-userinit on the command line.
(sb-ext:restrict-compiler-policy 'debug 3 3)
(sb-ext:restrict-compiler-policy 'safety 3 3)"))))
   (simple-service 'nix-config-files-service
                   home-files-service-type
                   (list `(".nix-channels"
                           ,(plain-file "nix-channels"
                                        "https://nixos.org/channels/nixos-22.11 nixpkgs"))
                         `(".config/nixpkgs/config.nix"
                           ,(plain-file "config.nix"
                                        "{
  allowUnfree = true;
}"))))
   (simple-service 'nix-config-env-vars-service
                   home-environment-variables-service-type
                   `(("NIX_PROFILE" . "$HOME/.nix-profile")
                     ;; See info (bash) Shell Parameter Expansion for why :+ is used
                     ("PATH" . "$NIX_PROFILE/bin${PATH:+:}$PATH")))
   (service home-mbsync-service-type
            (home-mbsync-configuration
             (use-xdg? #f)
             (global-config '((Sync All)
                              (Create Both)
                              (Remove None)
                              (Expunge Both)
                              (CopyArrivalDate yes)
                              (SyncState *)))
             (accounts
              (list (home-mbsync-account-configuration
                     (name "personal")
                     (auth-mechs "LOGIN")
                     (certificate-file "/etc/ssl/certs/ca-certificates.crt")
                     (host "imap.gmail.com")
                     (user "karl@hallsby.com")
                     (pass-cmd (string-join (list "cat" (string-append (getenv "HOME") "/personal"))
                                            " " 'infix))
                     (pipeline-depth 50)
                     (port 993)
                     (ssl-type "IMAPS")
                     (ssl-versions "TLSv1.3")
                     (remote-mail-store
                      (home-mbsync-imap-store-configuration
                       (name "remote")))
                     (local-mail-store
                      (home-mbsync-maildir-store-configuration
                       (name "local")
                       (path (string-append (getenv "HOME") "/Mail/Personal/"))
                       (inbox (string-append (getenv "HOME") "/Mail/Personal/Inbox"))
                       (subfolders "Verbatim")))
                     (groups
                      (list (home-mbsync-group-configuration
                             (name "personal")
                             (channels (list (home-mbsync-channel-configuration
                                              (name "inbox"))
                                             (home-mbsync-channel-configuration
                                              (name "drafts")
                                              (far "[Gmail]/Drafts")
                                              (near "Drafts"))
                                             (home-mbsync-channel-configuration
                                              (name "sent")
                                              (far "[Gmail]/Sent Mail")
                                              (near "Sent"))
                                             (home-mbsync-channel-configuration
                                              (name "starred")
                                              (far "[Gmail]/Starred")
                                              (near "Starred"))
                                             (home-mbsync-channel-configuration
                                              (name "trash")
                                              (far "[Gmail]/Trash")
                                              (near "Trash"))
                                             (home-mbsync-channel-configuration
                                              (name "acm")
                                              (far "Academic_Orgs/ACM")
                                              (near "ACM"))
                                             (home-mbsync-channel-configuration
                                              (name "github")
                                              (far "GitHub")
                                              (near "GitHub"))
                                             (home-mbsync-channel-configuration
                                              (name "ieee")
                                              (far "Academic_Orgs/IEEE")
                                              (near "IEEE"))
                                             (home-mbsync-channel-configuration
                                              (name "phd")
                                              (far "PhD")
                                              (near "PhD"))))))))
                    (home-mbsync-account-configuration
                     (name "iit")
                     (auth-mechs "LOGIN")
                     (certificate-file "/etc/ssl/certs/ca-certificates.crt")
                     (host "imap.gmail.com")
                     (user "khallsby@hawk.iit.edu")
                     (pass-cmd (string-join (list "cat" (string-append (getenv "HOME") "/iit"))
                                            " " 'infix))
                     (pipeline-depth 50)
                     (port 993)
                     (ssl-type "IMAPS")
                     (ssl-versions "TLSv1.3")
                     (remote-mail-store
                      (home-mbsync-imap-store-configuration
                       (name "remote")))
                     (local-mail-store
                      (home-mbsync-maildir-store-configuration
                       (name "local")
                       (path (string-append (getenv "HOME") "/Mail/IIT/"))
                       (inbox (string-append (getenv "HOME") "/Mail/IIT/Inbox"))
                       (subfolders "Verbatim")))
                     (groups
                      (list (home-mbsync-group-configuration
                             (name "iit")
                             (channels (list (home-mbsync-channel-configuration
                                              (name "inbox"))
                                             (home-mbsync-channel-configuration
                                              (name "drafts")
                                              (far "[Gmail]/Drafts")
                                              (near "Drafts"))
                                             (home-mbsync-channel-configuration
                                              (name "sent")
                                              (far "[Gmail]/Sent Mail")
                                              (near "Sent"))
                                             (home-mbsync-channel-configuration
                                              (name "starred")
                                              (far "[Gmail]/Starred")
                                              (near "Starred"))
                                             (home-mbsync-channel-configuration
                                              (name "trash")
                                              (far "[Gmail]/Trash")
                                              (near "Trash"))
                                             (home-mbsync-channel-configuration
                                              (name "blackboard")
                                              (far "BlackBoard Submissions")
                                              (near "BlackBoard_Submissions"))
                                             (home-mbsync-channel-configuration
                                              (name "camras")
                                              (far "Camras")
                                              (near "Camras"))
                                             (home-mbsync-channel-configuration
                                              (name "coterminal")
                                              (far "Co-Terminal")
                                              (near "Co-Terminal"))
                                             (home-mbsync-channel-configuration
                                              (name "cyberhawks")
                                              (far "CyberHawks")
                                              (near "CyberHawks"))
                                             (home-mbsync-channel-configuration
                                              (name "financialAid")
                                              (far "Office of Financial Aid")
                                              (near "FinAid"))
                                             (home-mbsync-channel-configuration
                                              (name "github")
                                              (far "GitHub")
                                              (near "GitHub"))
                                             (home-mbsync-channel-configuration
                                              (name "googleForms")
                                              (far "Google Forms")
                                              (near "Google_Forms"))
                                             (home-mbsync-channel-configuration
                                              (name "martialArtsClub")
                                              (far "Martial Arts Club")
                                              (near "MartialArtsClub"))
                                             (home-mbsync-channel-configuration
                                              (name "nsf")
                                              (far "NSF")
                                              (near "NSF"))
                                             (home-mbsync-channel-configuration
                                              (name "outOfSight")
                                              (far "Keep but Out of Sight")
                                              (near "Keep_but_Out_of_Sight"))
                                             (home-mbsync-channel-configuration
                                              (name "phd")
                                              (far "PhD")
                                              (near "PhD"))
                                             (home-mbsync-channel-configuration
                                              (name "studyAbroad")
                                              (far "Study Abroad")
                                              (near "Study_Abroad"))
                                             (home-mbsync-channel-configuration
                                              (name "tauBetaPi")
                                              (far "Tau Beta Pi")
                                              (near "TauBetaPi"))
                                             (home-mbsync-channel-configuration
                                              (name "triangle")
                                              (far "Triangle")
                                              (near "Triangle"))))))))))
             (post-sync-cmd "mu index")))
   (service home-mu-service-type
            (home-mu-configuration
             (addresses (list "karl@hallsby.com" "khallsby@hawk.iit.edu"))))
   (service home-msmtp-service-type
            (home-msmtp-configuration
             (accounts (list
                        (home-msmtp-account-configuration
                         (account "personal")
                         (auth "on")
                         (from "karl@hallsby.com")
                         (host "smtp.gmail.com")
                         (port 587)
                         (user "karl@hallsby.com")
                         (pass-cmd (string-join (list "cat"
                                                      (string-append (getenv "HOME") "/personal"))
                                                " " 'infix))
                         (protocol "smtp")
                         (tls? #t)
                         (starttls? #t)
                         (tls-trust-file "/etc/ssl/certs/ca-certificates.crt"))
                        (home-msmtp-account-configuration
                         (account "iit")
                         (auth "on")
                         (from "khallsby@hawk.iit.edu")
                         (host "smtp.gmail.com")
                         (port 587)
                         (user "khallsby@hawk.iit.edu")
                         (pass-cmd (string-join (list "cat"
                                                      (string-append (getenv "HOME") "/iit"))
                                                " " 'infix))
                         (protocol "smtp")
                         (tls? #t)
                         (starttls? #t)
                         (tls-trust-file "/etc/ssl/certs/ca-certificates.crt"))))
             (default-account "personal")))
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
