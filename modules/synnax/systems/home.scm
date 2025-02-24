(define-module (synnax systems home))

(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu home services desktop)
             (gnu home services shells)
             (gnu home services shepherd)
             (gnu home services mcron)
             (gnu home services sound)
             (gnu home services ssh)
             (gnu packages containers)
             (gnu packages mail)
             (gnu services)
             (guix gexp)
             (synnax systems packages)
             (synnax services keyboard)
             (synnax services podman)
             (synnax services home mail)
             (synnax services home emacs)
             (synnax services home gdb)
             (synnax services home keyboard)
             (synnax services home podman)
             (synnax services home tmux)
             (synnax services home vim)
             (synnax services home zathura))

(use-package-modules
 emacs
 linux
 music
 vnc
 vpn
 fontutils)

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
   (service home-keyboard-x11-service-type
            (keyboard-repeat-configuration
             (repeat-delay 200)
             (repeat-rate 40)))
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (environment-variables
              ;; See (bash) Bash Variables info for documentation.
              `(("HISTSIZE" . "10000")
                ("HISTCONTROL" . "ignoreboth:erasedups")))
             (aliases
              `(("grep" . ,(literal-string "grep --color=auto"))
                ("ll" . ,(literal-string "ls -l"))
                ("ls" . ,(literal-string "ls -p --color=auto"))
                ("ip" . ,(literal-string "ip -color=auto"))
                ("vim" . ,(literal-string "nvim"))
                ;; Many remote machines do not understand Alacritty's alacritty
                ;; TERM environment variable, as the entry is not present in
                ;; terminfo. So the remote machine falls back to some VT100-ish
                ;; behavior, which is just plain bad. So we always send a TERM
                ;; value that the remote system _should_ know about. This value
                ;; is old enough (it's freaking xterm) that every machine should
                ;; have it.
                ("ssh" . ,(literal-string "TERM=xterm-256color ssh"))))
             (bashrc
              ;; Each entry is added to final bashrc and string-join with newline
              (list
               ;; NOTE: A single \ in the output requires \\ in the string here!
               (plain-file "bashrc-vterm-printf"
                           "function vterm_printf() {
    if [ -n \"$TMUX\" ] && ([ \"${TERM%%-*}\" = \"tmux\" ] || [ \"${TERM%%-*}\" = \"screen\" ]); then
        # Tell tmux to pass the escape sequences through
        printf \"\\ePtmux;\\e\\e]%s\\007\\e\\\\\" \"$1\"
    elif [ \"${TERM%%-*}\" = \"screen\" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf \"\\eP\\e]%s\\007\\e\\\\\" \"$1\"
    else
        printf \"\\e]%s\\e\\\\\" \"$1\"
    fi
}")
               (plain-file "bashrc-print-osc7-dir-tracking"
                           "function osc7-dir-tracking {
    # Track current directory using OSC 7's URI-style notation.
    # Emacs 28+ natively understands this information for directory tracking!
    # NOTE: The URI _is_ actually printed, but uses a terminal escape sequence to
    # make it invisible!
    printf \"\\e]7;file://%s%s\\e\\\\\" \"$HOSTNAME\" \"$PWD\"
    vterm_printf \"51;A$USER@$HOSTNAME:$PWD\"
}

# Bash community has settled on ; to be the delimiter in PROMPT_COMMAND
PROMPT_COMMAND=\"osc7-dir-tracking${PROMPT_COMMAND:+;$PROMPT_COMMAND}\"")
               (plain-file "bashrc-color-ps1-prompt-command"
                           "function color_prompt_command {
    # PROMPT_COMMAND can contain ordinary Bash statements whereas the PS1 variable
    # can also contain the special characters, such as '\\h' for hostname, in the variable.
    # \\e is Bash's name for \\033 (escape)
    # Non-printable sequences should be enclosed in \\[ and \\]!
    # https://unix.stackexchange.com/a/105974
    if [[ -n \"$GUIX_ENVIRONMENT\" ]]; then
      PS1='\\n\\[\\e[1;34m\\]\\u@\\h \\w [env] \\t \\$\\[\\e[0m\\] '
    elif [[ -n \"$IN_NIX_SHELL\" ]]; then
      PS1='\\n\\[\\033[1;32m\\][nix-shell:\\w] \\t \\$\\[\\033[0m\\] '
    else
      PS1='\\n\\[\\e[1;35m\\]\\u@\\h \\w\\n\\t \\$\\[\\e[0m\\] '
    fi
}
# Bash community has settled on ; to be the delimiter in PROMPT_COMMAND
PROMPT_COMMAND=\"color_prompt_command${PROMPT_COMMAND:+;$PROMPT_COMMAND}\"")
               ;; All modifications to $PROMPT_COMMAND MUST come before this export!
               (plain-file "export-final-prompt-command" "export PROMPT_COMMAND")
               ;; NOTE: Adding direnv support should come last!
               (plain-file "bashrc-add-direnv" "eval \"$(direnv hook bash)\"")))))
   (service home-openssh-service-type
            (home-openssh-configuration
             (hosts
              (list (openssh-host (name "karl.hallsby.com")
                                  (identity-file "~/.ssh/website_rsa"))
                    (openssh-host (name "*github.com")
                                  (user "git")
                                  (identity-file "~/.ssh/github"))
                    (openssh-host (name "*gitlab.com")
                                  (user "git")
                                  (identity-file "~/.ssh/gitlab"))
                    (openssh-host (name "*.cs.northwestern.edu")
                                  (user "karl")
                                  (identity-file "~/.ssh/nu"))
                    (openssh-host (name "moore")
                                  (host-name "moore.wot.eecs.northwestern.edu")
                                  (identity-file "~/.ssh/nu"))
                    (openssh-host (name "*.eecs.northwestern.edu")
                                  (identity-file "~/.ssh/nu"))
                    (openssh-host (name "*.ece.northwestern.edu")
                                  (identity-file "~/.ssh/nu"))
                    (openssh-host (name "107.191.51.74")
                                  (user "root")
                                  (identity-file "~/.ssh/website_rsa"))
                    (openssh-host (name "roquefort")
                                  (host-name "roquefort.cs.northwestern.edu")
                                  (user "karl")
                                  (identity-file "~/.ssh/nu"))
                    (openssh-host (name "dubliner")
                                  (host-name "dubliner.cs.northwestern.edu")
                                  (user "karl")
                                  (identity-file "~/.ssh/nu"))
                    (openssh-host (name "peroni")
                                  (host-name "peroni.cs.northwestern.edu")
                                  (user "kgh0080")
                                  (identity-file "~/.ssh/nu"))
                    (openssh-host (name "cuirass")
                                  (host-name "cuirass.raven")
                                  (user "root")
                                  (identity-file "~/.ssh/cuirass_rsa"))
                    (openssh-host (name "tvbox")
                                  (host-name "tvbox.raven")
                                  (user "ravenjoad")
                                  (identity-file "~/.ssh/home_ed25519"))))))
   (simple-service 'git-config-files
                   home-xdg-configuration-files-service-type
                   (list `("git/attributes"
                           ,(plain-file "gitattributes"
                                        "# Generated by Guix Home. DO NOT EDIT!
*.c     diff=cpp
*.h     diff=cpp
*.c++   diff=cpp
*.h++   diff=cpp
*.cpp   diff=cpp
*.hpp   diff=cpp
*.cc    diff=cpp
*.hh    diff=cpp
*.css   diff=css
*.html  diff=html
*.xhtml diff=html
*.pl    diff=perl
*.py    diff=python
*.md    diff=markdown
*.rs    diff=rust
*.scm   diff=scheme
*.tex   diff=tex
*.bib   diff=bibtex
*.el    diff=elisp
*.lisp  diff=lisp"))
                         `("git/config"
                           ,(plain-file "gitconfig"
                                        "[core]
\tautocrlf = false
\tattributesfile = ~/.config/git/gitattributes

[user]
\tname = Karl Hallsby
\temail = karl@hallsby.com

[transfer]
\tfsckobjects = true

[fetch]
\tfsckobjects = true

[receive]
\tfsckobjects = true

[diff \"lisp\"]
\txfuncname = ^(\\\\((def[^[:space:]]*|let)[[:space:]]+[^[:space:]]+)

[diff \"elisp\"]
\txfuncname = ^\\\\([^[:space:]]*def[^[:space:]]+[[:space:]]+([^()[:space:]]+)"))
                         `("git/ignore"
                           ,(plain-file "gitignore"
                                        "*~
*.swp
.\\#*"))))
   (simple-service 'cl-source-registry-file-service
                   home-xdg-configuration-files-service-type
                   (list `("common-lisp/source-registry.conf"
                           ,(plain-file "cl-source-registry.conf"
                                        ";; -*- mode: common-lisp -*-
;; Generated by Guix Home.

(:source-registry
 (:tree (:home \"Repos\")) ;; Expands to \"$HOME/Repos/\"
 :inherit-configuration)"))))
   (simple-service 'sbcl-config-files-service
                   home-files-service-type
                   (list `(".sbclrc"
                           ,(plain-file "sbclrc"
                                        ";; Generated by Guix Home.
;; To disable SBCL's loading of this file, pass --no-userinit on the command line.
(sb-ext:restrict-compiler-policy 'debug 3 3)
(sb-ext:restrict-compiler-policy 'safety 3 3)"))))
   (simple-service 'bloop-config-files-service
                   home-files-service-type
                   (list `(".bloop/bloop.json"
                           ;; FIXME: Turn into service which uses guile-json
                           ;; library to generate the JSON?
                           ;; XXX: Can this even be done? There is a "javaHome"
                           ;; field in the default configuration that I generated
                           ;; with Chipyard that points to an openjdk install in
                           ;; the Nix store.
                           ,(plain-file "bloop.json"
                                        "{
  \"javaOptions\": [\"Xmx6G\"]
}"))))
   (simple-service 'nix-config-files-service
                   home-files-service-type
                   (list `(".nix-channels"
                           ,(plain-file "nix-channels"
                                        "https://nixos.org/channels/nixos-24.11 nixpkgs"))
                         `(".config/nixpkgs/config.nix"
                           ,(plain-file "config.nix"
                                        "{
  allowUnfree = true;
}"))))
   (simple-service 'nix-config-env-vars-service
                   home-environment-variables-service-type
                   `(("NIX_PROFILE" . "$HOME/.nix-profile")
                     ;; See info (bash) Shell Parameter Expansion for why :+ is used
                     ("PATH" . "$NIX_PROFILE/bin${PATH:+:}$PATH")
                     ("XDG_DATA_DIRS" . "$NIX_PROFILE/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS")))
   (simple-service 'sqlite3-config-file-service
                   home-files-service-type
                   ;; home-xdg-configuration-files-service-type after sqlite3 3.41
                   ;; .config/sqlite/.sqliterc (or similar)
                   ;; Might be best to keep the config file in both locations.
                   (list `(".sqliterc"
                           ,(plain-file "sqliterc"
                                        "-- -*- mode: sql -*-
-- Generated by Guix home.
.headers on
.mode column"))))
   (simple-service 'discord-config-files-service
                   home-xdg-configuration-files-service-type
                   (list `("discord/settings.json"
                           ,(plain-file "discord-settings.json"
                                        "{
  \"SKIP_HOST_UPDATE\": true
}"))))
   (service home-mbsync-service-type
            (home-mbsync-configuration
             (use-xdg? #t)
             ;; This is what I have set, but it is really gross.
             (global-config '((Sync All)
                              (Create Both)
                              (Remove None)
                              (Expunge Both)
                              (CopyArrivalDate yes)
                              (SyncState *)))
             (post-sync-cmd
              ;; I am not a fan of using a list here, but the problem is "index"
              ;; must be an argument to the mu binary, not a separate binary.
              ;; This means that we need a separate entry for the underlying
              ;; system* function.
              #~(list #$(file-append mu "/bin/mu") "index"))
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
                     (tls-type "IMAPS")
                     (tls-versions
                      (home-mbsync-tls-versions-configuration
                       (enable '("TLSv1.3" "TLSv1.2"))
                       (disable '("TLSv1.0" "TLSv1.1"))))
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
                                              (name "github")
                                              (far "GitHub/Waterly")
                                              (near "Waterly"))
                                             (home-mbsync-channel-configuration
                                              (name "ieee")
                                              (far "Academic_Orgs/IEEE")
                                              (near "IEEE"))
                                             (home-mbsync-channel-configuration
                                              (name "phd")
                                              (far "PhD")
                                              (near "PhD"))))))))
                    (home-mbsync-account-configuration
                     (name "northwestern")
                     (auth-mechs "LOGIN")
                     (certificate-file "/etc/ssl/certs/ca-certificates.crt")
                     (host "imap.gmail.com")
                     (user "karlhallsby2027@u.northwestern.edu")
                     (pass-cmd (string-join (list "cat" (string-append (getenv "HOME") "/northwestern"))
                                            " " 'infix))
                     (pipeline-depth 50)
                     (port 993)
                     (tls-type "IMAPS")
                     (tls-versions
                      (home-mbsync-tls-versions-configuration
                       (enable '("TLSv1.3" "TLSv1.2"))
                       (disable '("TLSv1.0" "TLSv1.1"))))
                     (remote-mail-store
                      (home-mbsync-imap-store-configuration
                       (name "remote")))
                     (local-mail-store
                      (home-mbsync-maildir-store-configuration
                       (name "local")
                       (path (string-append (getenv "HOME") "/Mail/Northwestern/"))
                       (inbox (string-append (getenv "HOME") "/Mail/Northwestern/Inbox"))
                       (subfolders "Verbatim")))
                     (groups
                      (list (home-mbsync-group-configuration
                             (name "northwestern")
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
                                              (near "Trash"))))))))))))
   (service home-mu-service-type
            (home-mu-configuration
             (addresses (list "karl@hallsby.com"
                              "karlhallsby2027@u.northwestern.edu"))))
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
                         (account "northwestern")
                         (auth "on")
                         (from "karlhallsby2027@u.northwestern.edu")
                         (host "smtp.gmail.com")
                         (port 587)
                         (user "karlhallsby2027@u.northwestern.edu")
                         (pass-cmd (string-join (list "cat"
                                                      (string-append (getenv "HOME") "/northwestern"))
                                                " " 'infix))
                         (protocol "smtp")
                         (tls? #t)
                         (starttls? #t)
                         (tls-trust-file "/etc/ssl/certs/ca-certificates.crt"))))
             (default-account "personal")))
   (simple-service 'wayland-workarounds-env-vars-service
            home-environment-variables-service-type
            `(("MOZ_ENABLE_WAYLAND" . "1")
              ("NIXOS_OZONE_WL" . "1")))
   (service home-emacs-server-service-type
            (home-emacs-server-configuration
             (package emacs-pgtk)))
   (service home-vim-service-type
            (home-vim-configuration
             (config '((number . #t)
                       (ruler . #t)
                       (noswapfile . #t)
                       (smarttab . #t)
                       (hlsearch . #t)
                       (incsearch . #t)
                       (smartcase . #t)
                       (ttyfast . #t)
                       (showmatch . #t)
                       (noerrorbells . #t)
                       (novisualbell . #t)
                       (encoding . "utf8")
                       (undolevels . 1000)
                       (backspace . "indent,eol,start")
                       (confirm . #t)))
             (extra-config
              (list (plain-file "vim-enable-syntax-highlight"
                                "syntax on")
                    (plain-file "vim-autodetect-filetypes"
                                "filetype plugin on")))))
   (service home-zathura-service-type
            (home-zathura-configuration
             (config
              '((recolor . #t)
                (selection-clipboard . "clipboard")))
             (key-mapping
              '(("q") ; Unbind q from quitting, I hit it too easily
                ("<C-g>" abort)
                ("<C-i>" zoom in)
                ("<C-o>" zoom out)
                ("<C-p>" scroll up)
                ("<C-n>" scroll down)
                ("<C-f>" scroll right)
                ("<C-b>" scroll left)
                ("<C-a>" scroll full-left)
                ("<C-e>" scroll full-right)
                ("<M-v>" scroll half-up)
                ("<C-v>" scroll half-down)
                ("<A-p>" scroll full-up)
                ("<A-n>" scroll full-down)
                ("<A-<>" goto top)
                ("<A->>" goto bottom)
                ("<C-0>" adjust_window width)
                ("<C-)>" adjust_window width)
                ;; Goto uses Vim-style motion: Page# <A-g>
                ("<A-g>" goto)))))
   (service home-tmux-service-type)
   (service home-podman-service-type
            (home-podman-configuration
             (packages (list podman podman-compose))
             (config-files
              (container-configuration-files
               (unqualified-search-registries
                (list "docker.io" "registry.fedoraproject.org"
                      "registry.access.redhat.com" "registry.centos.org"))))))
   (service home-gdb-service-type
            (home-gdb-configuration
             (configs
              (list
               (plain-file "gdb-set-history"
                           "set history save on
set history size unlimited

guile
(use-modules (gdb))
(let ((history-dir (string-append (or (getenv \"XDG_CACHE_HOME\") \"~/.cache\")
                                  \"/gdb\")))
  (execute (string-append \"set history filename \"
                           history-dir \"/history\"))
  (catch 'system-error
    (lambda () (mkdir history-dir))
    (lambda exn-args
      (if (= EEXIST (system-error-errno exn-args))
          (format #t \"~s already made!~%\" history-dir)
          (apply throw exn-args)))))
end")))))
   (simple-service 'alacritty-config-files
                   home-xdg-configuration-files-service-type
                   (list `("alacritty/alacritty.toml"
                           ,(plain-file "alacritty-config.toml"
                                        "# Generated by Guix Home. DO NOT EDIT!
[font]
size = 9

# Colors (Modus Vivendi) from https://github.com/anhsirk0/alacritty-themes
[colors.bright]
black = \"#535353\"
red = \"#ef8b50\"
green = \"#70b900\"
yellow = \"#c0c530\"
blue = \"#79a8ff\"
magenta = \"#fe00f6\"
cyan = \"#4ae2f0\"
white = \"#989898\"

[colors.cursor]
cursor = \"#f4f4f4\"
text = \"#323232\"

[colors.normal]
black = \"#323232\"
red = \"#ff8059\"
green = \"#44bc44\"
yellow = \"#d0bc00\"
blue = \"#2fafff\"
magenta = \"#f78fe7\"
cyan = \"#00d3d0\"
white = \"#ffffff\"

[colors.primary]
background = \"#000000\"
foreground = \"#ffffff\"

[colors.selection]
background = \"#3c3c3c\"
text = \"#ffffff\""))))
   (service home-mcron-service-type
            (home-mcron-configuration
             (jobs
              (list
               fontconfig-update-mcron-job)))))))
