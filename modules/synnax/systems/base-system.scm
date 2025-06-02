(define-module (synnax systems base-system)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu services)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services nix)
  #:use-module (gnu services ssh)
  #:use-module (gnu services syncthing)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services xorg)
  #:use-module (gnu system)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages mozilla)
  #:use-module (nongnu system linux-initrd)
  #:use-module (synnax services fstrim)
  #:use-module (synnax services keyboard)
  #:use-module (synnax services podman)
  #:use-module ((synnax systems archive-keys) #:prefix keys/)
  #:use-module (synnax systems packages)
  #:export (%base-system))

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
            (service containerd-service-type)
            (service docker-service-type)
            (service podman-service-type
                     (podman-configuration
                      (user-name "karljoad")
                      (config-files
                       (container-configuration-files
                        (unqualified-search-registries
                         (list "docker.io" "registry.fedoraproject.org"
                               "registry.access.redhat.com" "registry.centos.org"))))))
            (service nix-service-type
                     (nix-configuration
                      (extra-config
                       (list "experimental-features = nix-command flakes\n"
                             "trusted-users = root @wheel karljoad\n"
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
            ;; Allow for native compilation of foreign architectures. This is
            ;; like cross-compilation, but instead of using a compiler for a
            ;; TARGET architecture, my computer "fakes" being the building SYSTEM
            ;; architecture.
            ;; This must be manually enabled/disabled. You can control this with
            ;; herd start/stop qemu-binfmt
            (service qemu-binfmt-service-type
                     (qemu-binfmt-configuration
                      (platforms
                       (lookup-qemu-platforms "arm" "aarch64" "riscv64"))))
            ;; Enable stem darkening to make thin fonts look crisper at small resolutions.
            ;; This is added to /etc/environment through session-environment-service-type
            ;; which comes from (gnu system).
            (simple-service 'font-stem-darkening-env-var session-environment-service-type
                            '(("FREETYPE_PROPERTIES" . "cff:no-stem-darkening=0 autofitter:no-step-darkening=0")))
            ;; Set the keyboard repeat rate in the CONSOLE. Setting it for X11
            ;; must be done for X specifically. Setting the rate for Wayland
            ;; is Wayland compositor-specific.
            (service keyboard-console-service-type
                     (keyboard-repeat-configuration
                      (repeat-delay 200)
                      (repeat-rate 40)))
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
                                          (wayland? #t)))
                       (guix-service-type config =>
                                          (guix-configuration
                                           (inherit config)
                                           (substitute-urls
                                            (cons* "https://substitutes.nonguix.org"
                                                   "https://nonguix-proxy.ditigal.xyz"
                                                   (guix-configuration-substitute-urls config)))
                                           (authorized-keys
                                            (cons* keys/%nonguix
                                                   (guix-configuration-authorized-keys config))))))))
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
