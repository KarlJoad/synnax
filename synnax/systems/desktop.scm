(define-module (synnax systems desktop))

(use-modules (gnu)
             (gnu packages linux)
             (guix download)
             (guix packages)
             ;; Modules below require nonguix be a pulled channel
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (nongnu packages mozilla))

(use-package-modules gnuzilla ; Icecat
                     web-browsers ; Nyxt
                     dunst
                     bash
                     emacs emacs-xyz
                     vim
                     mail
                     wget curl
                     version-control
                     wm ; Stumpwm
                     xorg
                     certs
                     moreutils pciutils lsof
                     disk ; dosfstools (mkfs.fat)
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
 syncthing
 xorg
 virtualization docker)

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
                 "0yr8xfds5l1s3lk8qk67mgy0l4yh2jfvjc7xwrfws3ci020ss9a2"
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
                   "kvm" "libvirt" "docker"
                   "dialout")))
               %base-user-accounts))
 (packages
  (append
   (list coreutils moreutils
         ;; sawfish
         (specification->package "stumpwm-with-slynk") xsetroot
         icecat firefox
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
         dosfstools
         dunst
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
                   (unix-sock-group "libvirt")))
         (service virtlog-service-type)
         (service docker-service-type)
         (service syncthing-service-type
                  (syncthing-configuration
                   (user "karljoad"))) ;; TODO: Refactor `user' field to use variable.
         (extra-special-file "/bin/bash" (file-append bash "/bin/bash"))
         (extra-special-file "/usr/bin/env" (file-append coreutils "/bin/env")))
   %desktop-services))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-removable-bootloader)
   (targets (list "/boot/efi"))
   (keyboard-layout keyboard-layout)))
 (swap-devices
  (list (swap-space
         (target
          (uuid "58db59f7-b2e6-489f-89ac-911734ac8da2")))))
 (file-systems
  (cons* (file-system
          (mount-point "/boot/efi")
          (device (uuid "2089-9100" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device
           (uuid "db00e330-f375-4a28-9f68-90eca0d2db17"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
