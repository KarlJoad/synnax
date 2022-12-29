(define-module (synnax systems desktop))

(use-modules (gnu)
             (gnu packages linux)
             (guix download)
             (guix packages)
             ;; Modules below require nonguix be a pulled channel
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (nongnu packages mozilla)
             (nongnu packages nvidia)
             (nongnu services nvidia)
             ;; Modules below are from my own Synnax channel
             (synnax packages scripts)
             (synnax systems packages))

(use-package-modules bash freeipmi)

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

(define %container-linux-options
  `(("CONFIG_DEVPTS_MULTIPLE_INSTANCES" . #t)))

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
                 "08389890gq4b9vkvrb22lzkr4blkn3a5ma074ns19gl89wyyp16l"
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
                   "dialout" "plugdev")))
               %base-user-accounts))
 (groups (cons* (user-group (name "plugdev")
                            (system? #t))
                %base-groups))
 (packages
  (append
   (list freeipmi
         fix-desktop-monitors)
   %system-packages
   %base-packages))
 (services
  (append
   (list (service xfce-desktop-service-type)
         (service openssh-service-type)
         (service cups-service-type)
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout)
           (modules (cons* nvidia-driver %default-xorg-modules))
           (drivers '("nvidia"))))
         (service nvidia-service-type)
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
          (uuid "375cdcd3-21c9-4130-b507-68767ebc3a90")))))
 (file-systems
  (cons* (file-system
          (mount-point "/boot/efi")
          (device (uuid "2089-9100" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device
           (uuid "0c469309-3ce2-4780-bf8c-279f7f02e155"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
