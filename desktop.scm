(use-modules (gnu)
             (gnu packages linux)
             (guix download)
             (guix packages)
             ;; Two modules below require nonguix be a pulled channel
             (nongnu packages linux)
             (nongnu system linux-initrd))

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

(use-service-modules
 cups
 desktop
 networking
 ssh
 xorg
 virtualization)

(define %ipmi-linux-options
  `(("CONFIG_ACPI_IPMI" . #t)
    ("CONFIG_IPMI_HANDLER" . #t)
    ("CONFIG_IPMI_DMI_DECODE" . #t)
    ("CONFIG_IPMI_PLAT_DATA" . #t)
    ("CONFIG_IPMI_PANIC_EVENT" . #t)
    ("CONFIG_IPMI_DEVICE_INTERFACE" . #t)
    ("CONFIG_IPMI_SI" . #t)
    ("CONFIG_IPMI_SSIF" . #t)
    ("CONFIG_IPMI_WATCHDOG" . #t)
    ("CONFIG_IPMI_POWEROFF" . #t)))

(define %sound-card-options
  `(("CONFIG_SND_EMU10K1" . #t)))

(define* (add-linux-libre-config-options #:rest options)
  "Prepend provided list of OPTIONS, which itself is a list of pairs."
  (append
   (apply append options)
   (@@ (gnu packages linux) %default-extra-linux-options)))

(define-public linux-libre/desktop
  ((@@ (gnu packages linux) make-linux-libre*)
   linux-libre-version
   linux-libre-gnu-revision
   linux-libre-source
   '("x86_64-linux")
   #:configuration-file (@@ (gnu packages linux) kernel-config)
   ;; extra-options must take ALL config flags to be passed to the kernel!
   #:extra-options (add-linux-libre-config-options
                    %ipmi-linux-options
                    %sound-card-options)))

;; Will only work if nonguix channel is present.
(define-public linux-corrupted/desktop
  (corrupt-linux linux-libre/desktop linux-libre-version
                 "035i9i0gg3fxi5ighjrya97592sk0i4xagra6a8m8nxyh21z3k34"
                 #:name "linux-ipmi"))

(operating-system
 (locale "en_US.utf8")
 (kernel linux-corrupted/desktop)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))

 (timezone "America/Chicago")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "Karl-Desktop")
 (users (cons* (user-account
                (name "karljoad")
                (comment "Karl Hallsby")
                (group "users")
                (home-directory "/home/karljoad")
                (supplementary-groups
                 `("wheel" "netdev" "audio" "video"
                   "kvm" ;; ,(user-group (name "libvirt-users") (system? #t))
                   "dialout")))
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
         (service cups-service-type)
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout)))
         (service libvirt-service-type
                  (libvirt-configuration
                   (unix-sock-group "libvirt-users"))))
   %desktop-services))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets (list "/boot/efi"))
   (keyboard-layout keyboard-layout)))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "362b972e-bd89-48f7-8a61-95b298b23cb4"
                 'ext4))
          (type "ext4"))
         (file-system
          (mount-point "/boot/efi")
          (device (uuid "C9B7-4E39" 'fat32))
          (type "vfat"))
         %base-file-systems)))
