(define-module (synnax systems laptop))

(use-modules (gnu)
             (gnu packages linux)
             (guix download)
             (guix packages)
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (synnax systems packages))

(use-service-modules
 cups
 desktop
 networking
 ssh
 syncthing
 xorg
 nix
 virtualization docker)

(define-public laptop
  (operating-system
   (locale "en_US.utf8")
   (kernel linux)
   (initrd microcode-initrd)
   (firmware (list linux-firmware))

   (timezone "America/Chicago")
   (keyboard-layout (keyboard-layout "us"))
   (host-name "Guix-Laptop")
   (users (cons* (user-account
                  (name "karljoad")
                  (comment "Karl Hallsby")
                  (group "users")
                  (home-directory "/home/karljoad")
                  (supplementary-groups
                   `("wheel" "netdev" "audio" "video"
                     "lp" "input" "tty"
                     "kvm" ;; ,(user-group (name "libvirt-users") (system? #t))
                     "dialout")))
                 %base-user-accounts))
   (packages
    (append
     (list )
     %system-packages
     %base-packages))

   (services
    (append
     (list (service xfce-desktop-service-type)
           (service openssh-service-type)
           (service cups-service-type)
           (set-xorg-configuration
            (xorg-configuration
             (keyboard-layout keyboard-layout)))
           (service nix-service-type)
           (service syncthing-service-type
                    (syncthing-configuration
                     (user "karljoad") ;; TODO: Refactor `user' field to use variable.
                     (arguments '("-gui-address=127.0.0.1:8384"))))
           (service libvirt-service-type
                    (libvirt-configuration
                     (unix-sock-group "libvirt-users")))
           (service virtlog-service-type)
           (service docker-service-type))
     %desktop-services))

   (bootloader
    (bootloader-configuration
     (bootloader grub-efi-bootloader)
     (targets (list "/boot/efi"))
     (keyboard-layout keyboard-layout)))
   (mapped-devices
    (list (mapped-device
           (source
            (uuid "34627d93-4e91-42e8-8eb8-b64c8fa4729f"))
           (target "cryptroot")
           (type luks-device-mapping))))
   (file-systems
    (cons* (file-system
            (mount-point "/boot/efi")
            (device (uuid "C899-DB8A" 'fat32))
            (type "vfat"))
           (file-system
            (mount-point "/")
            (device "/dev/mapper/cryptroot")
            (type "ext4")
            (dependencies mapped-devices))
           %base-file-systems))))
