(define-module (synnax systems vm-desktop))

(use-modules (gnu)
             (synnax systems packages))

(use-package-modules bash)
(use-service-modules desktop networking ssh xorg)

(operating-system
 (locale "en_US.utf8")
 (timezone "America/Chicago")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "guix-iso-vm")
 (users (cons* (user-account
                (name "raven")
                (comment "Raven Hallsby")
                (group "users")
                (home-directory "/home/raven")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video" "kvm")))
               %base-user-accounts))
 (packages
  (append
   (list )
   %system-packages
   %base-packages))
 (services
  (append
   (list (service openssh-service-type)
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout)))
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
         (target (uuid "749b8ace-aab5-4c24-a89f-f04f71323774")))))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "c67b093f-2390-43df-8744-2cefb9fc4b67"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
