(define-module (synnax systems desktop)
  #:use-module (gnu)
  #:use-module (gnu packages freeipmi)
  #:use-module (gnu packages linux)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu services networking)
  #:use-module (gnu services virtualization)
  #:use-module (srfi srfi-1)
  ;; Modules below require nonguix be a pulled channel
  #:use-module (nongnu packages linux) ;; Needed for corrupt-linux
  #:use-module (nongnu packages nvidia)
  #:use-module (nongnu services nvidia)
  ;; Modules below are from my own Synnax channel
  #:use-module (synnax packages scripts)
  #:use-module (synnax services udev-rules)
  #:use-module (synnax systems base-system)
  #:export (desktop))

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

;; NOTE: Currently Guix catches the fact that the default value for this
;; configure variable is #f/"n", but I am setting it to #t/"y". I do not
;; actively use containers on Guix right now, so this is not truly needed for me.
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
                   #:configs (extra-linux-config-options %ipmi-linux-options)))

;; Will only work if nonguix channel is present.
(define-public linux-corrupted/desktop
  (corrupt-linux linux-libre/desktop #:name "linux-ipmi"))

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
       (udev-rules-service 'zsa-moonlander zsa-udev-rule))
      (operating-system-user-services %base-system)))

    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-removable-bootloader)
      (targets (list "/boot/efi"))
      (keyboard-layout (operating-system-keyboard-layout %base-system))))
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
            ;; (file-system
            ;;   (mount-point "/mnt/VM_Disks")
            ;;   (device
            ;;    (uuid "892ca4b1-5ffa-45ef-ba27-c6e6a55097c5"
            ;;          'ext4))
            ;;   (type "ext4"))
            %base-file-systems))))

desktop
