(define-module (synnax systems lilcato)
  #:use-module (gnu)
  #:use-module (gnu packages linux) ; brightnessctl
  #:use-module (gnu packages qt)
  #:use-module (gnu packages wm)
  #:use-module (gnu services desktop)
  #:use-module (gnu services pm)
  #:use-module (synnax systems base-system)
  #:export (lilcato))

(define-public lilcato
  (operating-system
   (inherit %base-system)
   (host-name "lilcato")

   (packages
    (append
     (list sway
           swaylock
           ;; Swaylock with extra goodies.
           ;; swaylock-effects
           quickshell
           ;; Adds a QT_PLATFORM_PLUGIN for wayland
           qtwayland
           brightnessctl
           bluez)
     (operating-system-packages %base-system)))

   (services
    (append
     (list (service bluetooth-service-type
                    (bluetooth-configuration
                     (auto-enable? #t)))
           (service power-profiles-daemon-service-type))
     (operating-system-user-services %base-system)))

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

lilcato
