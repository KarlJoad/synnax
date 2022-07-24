(define-module (synnax systems packages)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (nongnu packages mozilla)
  #:export (%nonguix-packages
            %web-browser-packages
            %virtualization-packages
            %filesystems-packages
            %wm-packages
            %utils-packages
            %system-packages
            %base-home-packages
            %eda-packages
            %document-prep-packages
            %development-packages
            %font-packages
            %home-packages))

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

(define %filesystems-packages
  (list squashfs-tools dosfstools
        cryptsetup
        cifs-utils
        usbutils))

(define %wm-packages
  (list stumpwm+slynk xsetroot dunst))

(define %utils-packages
  (list coreutils moreutils pciutils lsof hwdata binutils))

(define %system-packages
  (append
   (list pwgen
         xdg-utils
         xautolock ; Run command for user after some time has passed with no input
         alsa-utils pavucontrol
         vim
         emacs emacs-guix
         wget curl
         zip unzip
         rxvt-unicode alacritty
         nss-certs)
   %utils-packages
   %wm-packages
   %web-browser-packages
   %virtualization-packages))


(define %base-home-packages
  (list vim  ;; While this may be heresy, having vim is useful sometimes
        emacs
        emacs-guix
        password-store ;; pass
        ))

(define %eda-packages (list lepton-eda kicad))

(define %document-prep-packages
  (list texmacs
        texlive ;; Include ALL of TeXLive, because I am lazy and disks are large
        python-pygments ;; To make texlive-minted work
        biber ;; I prefer biber over old-school bibtex
        okular
        ispell
        libreoffice))

(define %development-packages
  (list binutils ;; TODO: Only install the binutils info manuals to global home path?
        gnu-make ;; Include the make command by default
        global   ;; Global & gtags
        cscope))

(define %font-packages
  (list font-iosevka font-iosevka-slab font-iosevka-term font-iosevka-term-slab
        font-fira-mono font-fira-code))

(define %home-packages
  (append
   (list hello)
   %development-packages
   %document-prep-packages
   %eda-packages
   %font-packages
   %fs-tools-packages
   %base-home-packages))
