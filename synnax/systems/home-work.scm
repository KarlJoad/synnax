(define-module (synnax systems home-work))

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
                     wm
                     dunst
                     bash
                     emacs emacs-xyz
                     vim
                     mail
                     wget curl
                     version-control
                     wm ; Stumpwm
                     xorg
                     xdisorg ; X11 helper programs (xautolock)
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
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))

 (timezone "America/Chicago")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "Guix-HomeWork")
 (users (cons* (user-account
                (name "raven")
                (comment "Raven Hallsby")
                (group "users")
                (home-directory "/home/raven")
                (supplementary-groups
                 `("wheel" "netdev" "audio" "video"
                   "kvm" "libvirt" "docker"
                   "dialout" "plugdev")))
               %base-user-accounts))
 (groups (cons* (user-group (name "plugdev")
                            (system? #t))
                %base-groups))
 (packages
  (append
   (list coreutils moreutils
         ;; sawfish
         stumpwm+slynk xsetroot
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
   (list (service openssh-service-type)
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
                   (user "raven"))) ;; TODO: Refactor `user' field to use variable.
         (udev-rules-service 'zsa-moonlander zsa-moonlander-udev-rule)
         (extra-special-file "/bin/bash" (file-append bash "/bin/bash"))
         (extra-special-file "/usr/bin/env" (file-append coreutils "/bin/env")))
   %desktop-services))
 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (targets (list "/dev/sda"))
   (keyboard-layout keyboard-layout)))
 (swap-devices
  (list (swap-space
         (target
          (uuid "8d6e8722-4052-4312-8d47-a57d85948614")))))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "81271a9c-a7d8-4418-b320-c2ab6b97545c"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
