(define-module (synnax systems home))

(use-modules (gnu)
             (gnu packages mail)
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
             (synnax services home mail)
             (synnax services home emacs)
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
                ("vim" . ,(literal-string "nvim"))))
             (bashrc
              ;; Each entry is added to final bashrc and string-join with newline
              (list
               ;; NOTE: A single \ in the output requires \\ in the string here!
               (plain-file "bashrc-print-osc7-dir-tracking"
                           "function osc7-dir-tracking {
    # Track current directory using OSC 7's URI-style notation.
    # Emacs 28+ natively understands this information for directory tracking!
    # NOTE: The URI _is_ actually printed, but uses a terminal escape sequence to
    # make it invisible!
    printf \"\\e]7;file://%s%s\\e\\\\\" \"$HOSTNAME\" \"$PWD\"
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
              (list (openssh-host (name "robin")
                                  (host-name "robin.raven")
                                  (user "root")
                                  (identity-file "~/.ssh/ci_rsa"))
                    (openssh-host (name "redhood")
                                  (host-name "redhood.raven")
                                  (user "root")
                                  (identity-file "~/.ssh/ci_rsa"))
                    (openssh-host (name "karl.hallsby.com")
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
                    (openssh-host (name "45.63.75.121") ;; Old website
                                  (user "root")
                                  (identity-file "~/.ssh/website_rsa"))
                    (openssh-host (name "107.191.51.74") ;; New website
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
                                  (identity-file "~/.ssh/nu"))))))
   (simple-service 'git-config-files
                   home-xdg-configuration-files-service-type
                   (list `("git/config"
                           ,(plain-file "gitconfig"
                                        "[user]
\tname = Karl Hallsby
\temail = karl@hallsby.com

[transfer]
\tfsckobjects = true

[fetch]
\tfsckobjects = true

[receive]
\tfsckobjects = true

[core]
\tautocrlf = false"))
                         `("git/ignore"
                           ,(plain-file "gitignore"
                                        "*~
*.swp
.\\#*"))))
   (simple-service 'tmux-config-file-service
                   home-xdg-configuration-files-service-type
                   (list `("tmux/tmux.conf"
                           ,(mixed-text-file "tmux.conf"
                                             "# Generated by Guix Home. DO NOT EDIT!
# Make Tmux use the same prefix as Emacs
unbind C-b
set -g prefix C-x
bind-key C-x send-prefix

# Use Emacs-style bindings everywhere
set -g mode-keys emacs
setw -g mode-keys emacs
set -g status-keys emacs
setw -g status-keys emacs
setw -g xterm-keys on

# Default to using UTF-8
# set -g status-utf8 on
# setw -g utf8 on

# Use this to set CWD of new pane when splitting.
bind c new-window -c \"#{pane_current_path}\"

# Move across panes in directions.
# These match what I do in my Emacs config too.
bind -N 'Select pane left'  C-b select-pane -L
bind -N 'Select pane down'  C-n select-pane -D
bind -N 'Select pane up'    C-p select-pane -U
bind -N 'Select pane right' C-f select-pane -R

# Switch Tmux windows by name the same way as Emacs
# This is taken directly from Tmux's default config (key-bindings.c)
# NOTE: Older Tmux-s do not support -T!
bind-key -N 'Select window by name/index' b { command-prompt -T window-target -p\\\"name/index\\\" { select-window -t ':%%' } }

# When renaming a window, do not fill in the old name automatically
bind-key -N 'Rename current window' , { command-prompt { rename-window -- '%%' } }

# Use 1-indexing for windows
set -g base-index 1 #
setw -g pane-base-index 1 #

# Configure status bar
set -g status on
set -g status-position bottom # Put status bar at bottom of terminal
set -g status-interval 5 # Update status bar every 5 seconds
set -g status-justify centre # Put window list on in center
set-option -g status-style fg=colour136,bg=colour235

set -g status-left-length 30
set -g status-left '[#S]#[fg=white]@#[fg=red]#H ' # [Session]@hostname

# loadavg DoW HH:MM YYYY-MM(month)-DD
set -g status-right '#[fg=green]#(" coreutils "/bin/cat" " /proc/loadavg | " coreutils "/bin/cut" " -f 1-3 -d \" \")'
set -ag status-right ' #[fg=white,bg=default]%a %H:%M #[fg=blue,bg=default]%Y-%m(%b)-%d'

# Set window title using xterm codes if terminal is xterm or similar
set -g set-titles on
# Allow current tmux window to be renamed by programs (SSH for example)
set -g allow-rename on

# Set $TERM inside tmux sessions
set -g default-terminal 'tmux-256color'

# Change time required for things to happen
set-option -g repeat-time 0
set -sg escape-time 0

# Allow the mouse to control things
set -g mouse on

# Raise the history limit significantly
set -g history-limit 25000

# Tmux automatically repacks (StumpWM terminology) window numbers
set -g renumber-windows on

# Tmux resizes window to size of smallest session
setw -g aggressive-resize on"))))
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
   (simple-service 'nix-config-files-service
                   home-files-service-type
                   (list `(".nix-channels"
                           ,(plain-file "nix-channels"
                                        "https://nixos.org/channels/nixos-24.05 nixpkgs"))
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
             (use-xdg? #f)
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
                                              (near "Triangle"))))))))
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
                     (ssl-type "IMAPS")
                     (ssl-versions "TLSv1.3")
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
             (addresses (list "karl@hallsby.com" "khallsby@hawk.iit.edu"
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
            `(("MOZ_ENABLE_WAYLAND" . "1")))
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
