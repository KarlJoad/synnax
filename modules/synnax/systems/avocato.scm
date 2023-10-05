(define-module (synnax systems avocato)
  #:export (avocato))

(use-modules (gnu)
             (synnax systems base-system))

(use-package-modules
 linux ;; brightnessctl
 )

(use-service-modules
 desktop)

(define avocato
  (operating-system
   (inherit %base-system)
   (host-name "avocato")

   (packages
    (append
     (list brightnessctl
           bluez)
     (operating-system-packages %base-system)))

   (services
    (append
     (list (service bluetooth-service-type))
     (operating-system-user-services %base-system)))

   (mapped-devices (list (mapped-device
                          (source (uuid
                                   "463ae7a7-5765-4d1d-9cf9-c86682c9f284"))
                          (target "cryptroot")
                          (type luks-device-mapping))))

   (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "21B0-F945"
                                       'fat32))
                         (type "vfat"))
			(file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "ext4")
                         (dependencies mapped-devices)) %base-file-systems))))

avocato
