(define-module (synnax systems desktop)
  #:export (desktop))

(use-modules (gnu)
             (gnu packages linux)
             (gnu system)
             ;; Modules below require nonguix be a pulled channel
             (nongnu packages linux) ;; Needed for corrupt-linux
             (nongnu packages nvidia)
             (nongnu services nvidia)
             ;; Modules below are from my own Synnax channel
             (synnax packages scripts)
             (synnax systems base-system))

(use-package-modules bash freeipmi)

(use-service-modules
 networking
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

(define %container-linux-options
  `(("CONFIG_DEVPTS_MULTIPLE_INSTANCES" . #t)))

(define (linux-config-alist->linux-config-string-list configs)
  "Given an alist of Linux kernel config options, convert to a list of strings
with the form show below.
  '((\"CONFIG_TO_SET\" . #t)
    (\"CONFIG_TO_CLEAR\" . #f)
    (\"CONFIG_AS_MODULE\" . m))
is turned into
  '(\"CONFIG_TO_SET=y\" \"CONFIG_TO_CLEAR=n\" \"CONFIG_AS_MODULE=m\")"
  (define (config->string config)
    (let* ((config-name (car config))
           (config-set (cdr config))
           (config-str (cond ((eq? config-set #t) "y")
                             ((eq? config-set #f) "n")
                             ((eq? config-set 'm) "m"))))
      (string-append config-name "=" config-str)))
  (map config->string configs))

(define* (extra-linux-config-options #:rest options)
  "Turn alist of Linux kernel options into a list of strings."
  (let ((configs (apply append options)))
    (linux-config-alist->linux-config-string-list configs)))

(define-public linux-libre/desktop
  (customize-linux #:name "linux-ipmi-uncorrupted"
                   #:configs (extra-linux-config-options %ipmi-linux-options
                                                         %container-linux-options)))

;; Will only work if nonguix channel is present.
(define-public linux-corrupted/desktop
  (corrupt-linux linux-libre/desktop #:name "linux-ipmi"))

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

(define desktop
  (operating-system
    (inherit %base-system)
    (kernel linux-corrupted/desktop)
    (host-name "Karl-Desktop")
    (packages
     (append
      (list freeipmi
            fix-desktop-monitors)
      (operating-system-packages %base-system)))
    (services
     (append
      (list
       ;; Add GNU Hurd VM that is small, but always exists.
       (service hurd-vm-service-type
                (hurd-vm-configuration
                 (disk-size (* 3 (expt 2 30))) ;3GiB Volatile disk
                 (memory-size 1024)))          ;1024MiB vRAM
       (udev-rules-service 'zsa-moonlander zsa-moonlander-udev-rule))
      (operating-system-user-services %base-system)))

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
            (file-system
              (mount-point "/mnt/VM_Disks")
              (device
               (uuid "892ca4b1-5ffa-45ef-ba27-c6e6a55097c5"
                     'ext4))
              (type "ext4"))
            (file-system
              (type "cifs")
              (mount-point "/mnt/store")
              (device "//karl-nas.raven/store")
              (options "noauto,vers=default,iocharset=utf8,uid=1000,gid=998,rw,file_mode=0644,dir_mode=0755,nobrl,nounix,credentials=/root/.personalSMBcredentials")
              (mount? #f)
              (create-mount-point? #t)
              (mount-may-fail? #t))
            %base-file-systems))))

desktop
