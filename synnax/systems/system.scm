(define-module (synnax systems system))

(use-modules (gnu))

(use-package-modules gnuzilla web-browsers ; Icecat & Nyxt
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
(use-service-modules desktop networking ssh xorg)

(operating-system
 (locale "en_US.utf8")
 (timezone "America/Chicago")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "guix-iso-vm")
 (users (cons* (user-account
                (name "karl")
                (comment "Karl Hallsby")
                (group "users")
                (home-directory "/home/karl")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video" "kvm")))
               %base-user-accounts))
 (packages
  (append
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
	       nss-certs)
   %base-packages))
 (services
  (append
   (list (service xfce-desktop-service-type)
         (service openssh-service-type)
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout))))
   %desktop-services))
 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (targets (list "/dev/sda"))
   (keyboard-layout keyboard-layout)))
 (swap-devices
  (list (swap-space
         (target (uuid "749b8ace-aab5-4c24-a89f-f04f71323774")))))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "c67b093f-2390-43df-8744-2cefb9fc4b67"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
