(use-modules (gnu))
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
   (list (specification->package "icecat")
         (specification->package "nyxt")
         (specification->package "emacs")
         (specification->package "emacs-guix")
         (specification->package "vim")
         (specification->package "mu")
	       (specification->package "git")
	       (specification->package "nss-certs"))
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
   (target "/dev/sda")
   (keyboard-layout keyboard-layout)))
 (swap-devices
  (list (uuid "749b8ace-aab5-4c24-a89f-f04f71323774")))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "c67b093f-2390-43df-8744-2cefb9fc4b67"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
