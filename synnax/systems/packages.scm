  :#export '(package-list))

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
(define-module (synnax systems packages)
  #:use-module (gnu)
  #:use-module (guix)

(use-package-modules
 gnuzilla ; Icecat
 web-browsers ; Nyxt
 emacs emacs-xyz
 vim
 admin
 password-utils ; passwd
 mail ; mbsync & mu
 xorg
 xdisorg          ; X11 helper programs
 wget curl
 version-control
 wm ; Stumpwm
 dunst ; X11 Notification manager
 certs
 moreutils pciutils lsof
 disk             ; dosfstools (mkfs.fat)
 cryptsetup samba
 freedesktop      ; XDG stuff
 linux pulseaudio ; ALSA/PulseAudio
 compression
 terminals      ; alacritty, urxvt
 virtualization ; virt-manager
 code           ; global
 engineering    ; lepton-eda
 text-editors   ; texmacs
 tex            ; texlive, biber
 python-xyz     ; python-pygments
 aspell         ; ispell
 kde            ; okular
 libreoffice
 fonts)

