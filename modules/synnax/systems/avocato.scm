(define-module (synnax systems avocato)
  #:use-module (gnu)
  #:use-module (gnu packages linux) ; brightnessctl
  #:use-module (gnu packages xorg)
  #:use-module (gnu services desktop)
  #:use-module (nongnu packages linux)
  #:use-module (synnax services udev-rules)
  #:use-module (synnax systems base-system)
  #:export (avocato))

;; Allow members of the "video" group to change the screen brightness.
(define backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define avocato
  (operating-system
   (inherit %base-system)
   (host-name "avocato")
   (firmware
    (append (list sof-firmware)
            (operating-system-firmware %base-system)))

   (packages
    (append
     (list xorg-server-xwayland
           brightnessctl
           bluez)
     (operating-system-packages %base-system)))

   (services
    (append
     (list (service bluetooth-service-type
                    (bluetooth-configuration
                     (auto-enable? #t)))
           (udev-rules-service 'change-brightness-service-type backlight-udev-rule)
           (udev-rules-service 'zsa-moonlander zsa-udev-rule))
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
                         (dependencies mapped-devices)) %base-file-systems))
   (swap-devices
    (list
     ;; /swapfile
     (swap-space
       (target "/swapfile")
       ;; uuid targets only work for swap partitions!
       ;; (target (uuid "ec711927-a230-40c0-baa0-6ca7895abb5c"))
       (dependencies (filter (file-system-mount-point-predicate "/")
                             file-systems)))))))

avocato
