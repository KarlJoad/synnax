(use-modules (gnu)
             (gnu packages linux)
             (guix download)
             (guix packages))
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

(operating-system
 (locale "en_US.utf8")
 (kernel linux-libre/desktop)

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
   (list (specification->package "emacs")
         (specification->package "emacs-guix")
         (specification->package "vim")
         (specification->package "git")
         ;; Only for making Emacs work nicely right now.
         (specification->package "mu")
         (specification->package "isync")
         (specification->package "stumpwm-with-slynk")
         (specification->package "xsetroot") ;; To change X11 stuff in StumpWM
         ;; Web browser (Minimal enough, but extensible)
         (specification->package "nyxt")
         ;; Virtualization
         (specification->package "libvirt")
         (specification->package "virt-manager")
         ;; Certs required for HTTPS
         (specification->package "nss-certs"))
   %base-packages))
 (services
  (append
   (list (service gnome-desktop-service-type)
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
