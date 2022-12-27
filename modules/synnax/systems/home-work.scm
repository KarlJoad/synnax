(define-module (synnax systems home-work))

(use-modules (gnu)
             (gnu packages linux)
             (guix download)
             (guix packages)
             ;; Modules below require nonguix be a pulled channel
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (nongnu packages mozilla)
             (synnax systems packages))

(use-package-modules bash)

(use-service-modules
 cups
 desktop
 networking
 ssh
 syncthing
 xorg
 virtualization docker)

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
   (list )
   ;; %nonguix-packages
   %system-packages
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
