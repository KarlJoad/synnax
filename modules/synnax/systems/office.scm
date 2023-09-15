(define-module (synnax systems office)
  #:export (office))

(use-modules (gnu)
             (synnax systems base-system))

(use-package-modules
 linux
 )

(use-service-modules
 networking
 desktop)

(define-public office
  (operating-system
   (inherit %base-system)
   (host-name "Karl-Office")

   (packages
    (append
     (list bluez)
     (operating-system-packages %base-system)))

   (services
    (append
     (list (service bluetooth-service-type))
     (operating-system-user-services %base-system)))

   (mapped-devices
    (list (mapped-device
                          (source (uuid
                                   "d02d555d-3929-4de4-b0d2-f5ad65395d29"))
                          (target "cryptroot")
                          (type luks-device-mapping))))
     (file-systems
      (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "6879-6F61"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "ext4")
                         (dependencies mapped-devices))
		       %base-file-systems))))

office
