(define-module (synnax systems packages)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (nongnu packages mozilla)
  #:export (%nonguix-packages
            %web-browser-packages
            %virtualization-packages))

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

(define %nonguix-packages (list firefox))

(define %web-browser-packages (list icecat nyxt xclip xsel))

(define %virtualization-packages
  (list virt-manager))
