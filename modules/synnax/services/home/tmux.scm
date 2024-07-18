(define-module (synnax services home tmux)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages tmux)
  #:use-module (gnu services configuration)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:export (home-tmux-configuration
            home-tmux-service-type))

(define-configuration/no-serialization home-tmux-configuration
  (package
    (package tmux)
    "Tmux package to provide in profile.")
  (use-xdg?
   (boolean #t)
   "Whether to place the generated Tmux configuration in XDG-specified
directory."))

(define (get-tmux-configuration config)
  `((,(if (home-tmux-configuration-use-xdg? config)
          "tmux/tmux.conf"
          ".tmux.conf")
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

(define (home-tmux-xdg-config-service config)
  "Link the built tmux configuration to the user's $XDG_CONFIG_HOME directory,
naming the resulting file $XDG_CONFIG_HOME/tmux/tmux.conf."
  (if (home-tmux-configuration-use-xdg? config)
      (get-tmux-configuration config)
      '()))

(define (home-tmux-dot-config-service config)
  "Link the built mbsync configuration to the user's $HOME directory,
naming the resulting file $HOME/.tmux.conf."
  (if (home-tmux-configuration-use-xdg? config)
      '()
      (get-tmux-configuration config)))

(define (home-tmux-profile-service config)
  "Return a list of <package>s to add to the user's profile."
  (list
   (home-tmux-configuration-package config)))

(define home-tmux-service-type
  (service-type (name 'home-tmux)
                (extensions
                 (list
                  ;; Only one of add-mbsync-dot-configuration or add-mbsync-xdg-configuration
                  ;; will ever be used, depending on the value of the use-xdg? field.
                  (service-extension
                   home-xdg-configuration-files-service-type
                   home-tmux-xdg-config-service)
                  (service-extension
                   home-files-service-type
                   home-tmux-dot-config-service)
                  (service-extension
                   home-profile-service-type
                   home-tmux-profile-service)))
                (default-value (home-tmux-configuration))
                (description "Create Tmux configuration and provide
@command{tmux} in the profile's PATH.")))
