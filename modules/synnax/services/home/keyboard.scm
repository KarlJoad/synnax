(define-module (synnax services home keyboard)
  #:use-module (guix gexp)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services shepherd)
  #:use-module (synnax services keyboard)
  #:export (home-keyboard-x11-service-type
            ;; keyboard-wayland-service-type
            ))


;; X11
;; Setting it for X11 must be done for X specifically.

(define (home-keyboard-x11-service config)
  "Return a <shepherd-service> that runs @file{xset} as a one-shot service."
  (let ((repeat-delay (keyboard-repeat-configuration-repeat-delay config))
        (repeat-rate (keyboard-repeat-configuration-repeat-rate config)))
    (list
     (shepherd-service
      (provision '(x11-keyboard-repeat))
      (requirement '())
      (one-shot? #t)
      (start #~(make-forkexec-constructor
                (list #$(file-append xset "/bin/xset")
                      "r"
                      "rate"
                      #$(number->string repeat-delay)
                      #$(number->string repeat-rate))))
      ;; Stopping this service means resetting the keybaord repeat/delay rates
      ;; to their default values.
      (stop #~(make-forkexec-constructor
                (list #$(file-append xset "/bin/xset")
                      "r"
                      "rate")))
      (documentation "Service to set keyboard repeat rate in X11.")))))

(define home-keyboard-x11-service-type
  (service-type
   (name 'keyboard-x11-repeat-rate)
   (extensions
    (list (service-extension
           home-shepherd-service-type
           home-keyboard-x11-service)))
   ;; (compose ???)
   ;; (extend ???)
   (default-value (keyboard-repeat-configuration
                   (repeat-delay 660)
                   (repeat-rate 25)))
   (description "Set keyboard repeat rate for X11.")))


;; Wayland
;; Setting the rate for Wayland is compositor-specific.
