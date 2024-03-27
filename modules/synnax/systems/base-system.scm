(define-module (synnax systems base-system)
  #:export (%base-system))

(use-modules (gnu)
             (gnu system)
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (nongnu packages mozilla)
             (synnax systems packages)
             (synnax services fstrim))

(use-package-modules
 bash)

(use-service-modules
 desktop
 networking
 ssh
 cups
 syncthing
 xorg
 virtualization docker
 nix)

(define-public %base-system
  (operating-system
   (locale "en_US.utf8")
   (kernel linux)
   (initrd microcode-initrd)
   (firmware (list linux-firmware))
   (timezone "America/Chicago")
   (keyboard-layout
    (keyboard-layout "us" #:options '("ctrl:nocaps")))
   (host-name "synnax-base-system")
   (users (cons* (user-account
                   (name "karljoad")
                   (comment "Karl Hallsby")
                   (group "users")
                   (home-directory "/home/karljoad")
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
      %system-packages
      %base-packages))
    (services
     (append
      (list (service plasma-desktop-service-type)
            (service openssh-service-type)
            (service cups-service-type
                     (cups-configuration
                      (web-interface? #t)))
            (service fstrim-service-type)
            (set-xorg-configuration
             (xorg-configuration
              (keyboard-layout keyboard-layout)))
            (service libvirt-service-type
                     (libvirt-configuration
                      (unix-sock-group "libvirt")))
            (service virtlog-service-type)
            (service docker-service-type)
            (service nix-service-type
                     (nix-configuration
                      (extra-config
                       (list "experimental-features = nix-command flakes\n"
                             ;; When using flakes, "nix shell" will not change $PS1.
                             ;; This configuration should make "nix shell" behave
                             ;; like "nix-shell" in setting $PS1.
                             "bash-prompt = \\n\\[\\033[1;32m\\][nix-develop:\\w]\\$\\[\\033[0m\\]\\040\n"))))
            (service syncthing-service-type
                     (syncthing-configuration
                      (user "karljoad") ;; TODO: Refactor `user' field to use variable.
                      (arguments '("-gui-address=127.0.0.1:8384"))))
            ;; 4-core VM with RTC set to Jan 2020, using Skylake for building
            ;; Does NOT run by default. Must explicitly turn on for offload
            ;; building to work!
            (service virtual-build-machine-service-type)
            ;; /bin/sh and /usr/bin/env are already made extra-special-files
            ;; by %base-services, which %desktop-services extends
            (extra-special-file "/bin/bash" (file-append bash "/bin/bash"))
            ;; TODO: Add this?
            ;; (name-service-switch %mdns-host-lookup-nss) ;; Resolve .local with mDNS
            )
      (modify-services %desktop-services
                       (gdm-service-type config =>
                                         (gdm-configuration
                                          (inherit config)
                                          (wayland? #t))))))
    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot/efi"))
      (keyboard-layout keyboard-layout)))
    (file-systems
     (append
      (list (file-system
              (device (file-system-label "fake-root"))
              (mount-point "/")
              (type "ext4")))
      %base-file-systems))))

%base-system
