(define-module (synnax services keyboard)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages linux)
  #:export (keyboard-repeat-configuration
            keyboard-repeat-configuration-repeat-delay
            keyboard-repeat-configuration-repeat-rate

            keyboard-console-service-type))

(define-configuration/no-serialization keyboard-repeat-configuration
  ;; The default values are from the Arch Wiki, and are the defaults that kbd uses
  ;; under the hood.
  ;; https://wiki.archlinux.org/title/Linux_console/Keyboard_configuration#Adjusting_typematic_delay_and_rate
  (repeat-delay
   (integer 660)
   "Delay before keyboard input starts repeating, in milliseconds.")
  (repeat-rate
   (integer 25)
   "Rate at which the key should be repeated per second."))


;; Console
(define (kbd-repeat-service config)
  "Return a <shepherd-service> that runs @file{kbdrate} as a one-shot service."
  (let ((repeat-delay (keyboard-repeat-configuration-repeat-delay config))
        (repeat-rate (keyboard-repeat-configuration-repeat-rate config)))
    (list
     (shepherd-service
      (provision '(keyboard-console-repeat-rate))
      (requirement '(user-processes virtual-terminal))
      (one-shot? #t)
      ;; (stop #~(const #f))
      (start #~(make-forkexec-constructor
                (list #$(file-append kbd "/bin/kbdrate")
                      "--silent"
                      "--delay" #$(number->string repeat-delay)
                      "--rate" #$(number->string repeat-rate))))
      (documentation "One-shot service to set keyboard repeat rate in TTY.")))))

(define keyboard-console-service-type
  (service-type
   (name 'keyboard-console-repeat-rate)
   (extensions
    (list (service-extension
           shepherd-root-service-type
           kbd-repeat-service)))
   ;; (compose ???)
   ;; (extend ???)
   (default-value (keyboard-repeat-configuration
                   (repeat-delay 250)
                   (repeat-rate 11)))
   (description "Set keyboard repeat rate in TTY.")))

(define (generate-keyboard-repeat-documentation)
  (generate-documentation
   `((keyboard-repeat-configuration ,keyboard-repeat-configuration-fields))
   'keyboard-repeat-configuration))
