;; This module is taken from https://git.sr.ht/~zoglesby/zpak/tree/main/item/zpak/home/services
;; and may not reflect the best way to do things.

(define-module (synnax services dbus)
  #:use-module (gnu packages glib)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp))

(define (home-dbus-environment-variables-service _)
  '(("DBUS_SESSION_BUS_ADDRESS" . "unix:path=$XDG_RUNTIME_DIR/dbus.sock")))

(define (home-dbus-shepherd-service _)
  (list
   (shepherd-service
    (provision '(dbus-home))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append dbus
                                   "/bin/dbus-daemon")
                    "--nofork"
                    "--session"
                    (string-append
                     "--address=" "unix:path="
                     (getenv "XDG_RUNTIME_DIR") "/dbus.sock"))))
    (documentation "User instance of DBUS"))))

(define-public home-dbus-service-type
  (service-type
   (name 'home-dbus)
   (extensions
    (list (service-extension
           home-environment-variables-service-type
           home-dbus-environment-variables-service)
          (service-extension
           home-shepherd-service-type
           home-dbus-shepherd-service)))
   (default-value #f)
   (description "Run user-level dbus instance.")))
