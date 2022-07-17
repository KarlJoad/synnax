(define-module (systems packages)
  :#use-module (gnu)
  :#use-module (guix)
  :#export '(package-list))

(use-package-modules gnuzilla ; Icecat
                     web-browsers ; Nyxt
                     emacs emacs-xyz
                     vim
                     mail
                     wget curl
                     version-control
                     wm ; Stumpwm
                     xorg
                     certs
                     moreutils pciutils lsof
                     xdisorg
                     password-utils
                     freedesktop ; XDG stuff
                     linux pulseaudio ; ALSA/PulseAudio
                     compression
                     terminals
                     admin)

(define-public package-list
  (list coreutils moreutils
        ;; sawfish
        (specification->package "stumpwm-with-slynk") xsetroot
        icecat
        nyxt xclip xsel
        pwgen
        xdg-utils
        alsa-utils pavucontrol
        vim
        emacs emacs-guix
        mu
        git
        wget curl
        zip unzip
        rxvt-unicode alacritty
        pciutils lsof hwdata
        squashfs-tools
        tree
        nss-certs))
