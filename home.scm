(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu home services shepherd)
             (gnu services)
             (guix gexp))

(use-package-modules base
                     admin
                     password-utils
                     emacs
                     emacs-xyz
                     engineering
                     maths
                     ebook
                     gnucash
                     music
                     video
                     vnc
                     vpn
                     package-management
                     text-editors
                     cryptsetup
                     samba
                     aspell
                     tex
                     kde)

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

(home-environment
 (packages
  (list binutils ;; TODO: Only install the binutils info manuals to global home path?
        password-store ;; pass
        yt-dlp
        lepton-eda
        gnucash
        lilypond
        texmacs
        texlive ;; Include ALL of TeXLive, because I am lazy and disks are large
        okular
        ispell
        emacs
        emacs-guix
        remmina
        stow
        octave
        ;; slack discord element-desktop
        calibre
        cryptsetup
        cifs-utils
        openconnect ;; IIT VPN
        ))
 (services
  (list
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
   (simple-service 'ssh-config
                   home-files-service-type
                   (list `("ssh/config"
                           ,(plain-file "config"
                                        "Host 192.168.1.6
\tUser root
\tIdentityFile ~/.ssh/ci_rsa

Host *github.com
\tUser git
\tIdentityFile ~/.ssh/github
"))
                         `(".gitconfig"
                           ,(plain-file "gitconfig"
                                        "[user]
\tname = Karl Hallsby
\temail = karl@hallsby.com"))))
   (simple-service 'xsession-file
                   home-files-service-type
                   (list `(".xsession"
                           ,(plain-file "xsession"
                                        "export GDK_CORE_DEVICE_EVENTS=1"))))
   (service home-shepherd-service-type
            (home-shepherd-configuration
             (auto-start? #t)
             (services
              (list
               (emacs-server "")
               (emacs-server "debug")))))
   )))
