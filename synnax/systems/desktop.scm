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
                     virtualization
                     admin freeipmi)

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
                    %ipmi-linux-options)))

;; Will only work if nonguix channel is present.
(define-public linux-corrupted/desktop
  (corrupt-linux linux-libre/desktop linux-libre-version
                 "0a5n1lb43nhnhwjwclkk3dqp2nxsx5ny7zfl8idvzshf94m9472a"
                 #:name "linux-ipmi"))

(define zsa-moonlander-udev-rule
  (udev-rule
   "50-wally.rules"
   (string-append
    "# Teensy rules for the Ergodox EZ\n"
    "ATTRS{idVendor}==\"16c0\", ATTRS{idProduct}==\"04[789B]?\", ENV{ID_MM_DEVICE_IGNORE}=\"1\"\n"
    "ATTRS{idVendor}==\"16c0\", ATTRS{idProduct}==\"04[789A]?\", ENV{MTP_NO_PROBE}=\"1\"\n"
    "SUBSYSTEMS==\"usb\", ATTRS{idVendor}==\"16c0\", ATTRS{idProduct}==\"04[789ABCD]?\", MODE:=\"0666\"\n"
    "KERNEL==\"ttyACM*\", ATTRS{idVendor}==\"16c0\", ATTRS{idProduct}==\"04[789B]?\", MODE:=\"0666\"\n"
    "# STM32 rules for the Moonlander and Planck EZ\n"
    "SUBSYSTEMS==\"usb\", ATTRS{idVendor}==\"0483\", ATTRS{idProduct}==\"df11\", \\\n"
    "MODE:=\"0666\", \\\n"
    "SYMLINK+=\"stm32_dfu\"\n")))

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
         freeipmi
         icecat firefox
         nyxt xclip xsel
         pwgen
         xdg-utils
         xautolock ; Run command for user after some time has passed with no input
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
         virt-manager
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
         ;; Add GNU Hurd VM that is small, but always exists.
         (service hurd-vm-service-type
                  (hurd-vm-configuration
                   (disk-size (* 3 (expt 2 30))) ;3GiB Volatile disk
                   (memory-size 1024)))          ;1024MiB vRAM
         (service syncthing-service-type
                  (syncthing-configuration
                   (user "karljoad"))) ;; TODO: Refactor `user' field to use variable.
         (udev-rules-service 'zsa-moonlander zsa-moonlander-udev-rule)
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
