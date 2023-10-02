;; This module is taken from https://git.sr.ht/~zoglesby/zpak/tree/main/item/zpak/home/services
;; and may not reflect the best way to do things.

(define-module (synnax services pipewire)
  #:use-module (guix gexp)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (synnax services home utils))

(define (home-pipewire-files-service _)
  `(("alsa/asoundrc"
     ,(mixed-text-file
       "asoundrc"
       #~(string-append
          "<"
          #$(file-append pipewire
             "/share/alsa/alsa.conf.d/50-pipewire.conf")
          ">\n<"
          #$(file-append pipewire
             "/share/alsa/alsa.conf.d/99-pipewire-default.conf")
          ">\n"
          "
pcm_type.pipewire {
  lib " #$(file-append pipewire
           "/lib/alsa-lib/libasound_module_pcm_pipewire.so") "
}

ctl_type.pipewire {
  lib " #$(file-append pipewire
           "/lib/alsa-lib/libasound_module_ctl_pipewire.so") "
}
")))))

(define (home-pipewire-shepherd-service _)
  (list
   (shepherd-service
    (requirement '(dbus-home))
    (provision '(pipewire))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append pipewire "/bin/pipewire"))
              #:log-file #$(home-service-log-file-path "pipewire")))
    (documentation "Pipewire service"))

   (shepherd-service
    (requirement '(pipewire))
    (provision '(wireplumber))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append wireplumber "/bin/wireplumber"))
              #:log-file #$(home-service-log-file-path "wireplumber")))
    (documentation "Wireplumber service for Pipewire"))

   (shepherd-service
    (requirement '(pipewire))
    (provision '(pipewire-pulse))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append pipewire "/bin/pipewire-pulse"))
              #:log-file #$(home-service-log-file-path "pipewire-pulse")))
    (documentation "Pulseaudio integration for Pipewire"))))

(define-public home-pipewire-service-type
  (service-type
   (name 'home-pipewire)
   (extensions
    (list (service-extension
           home-xdg-configuration-files-service-type
           home-pipewire-files-service)
          (service-extension
           home-shepherd-service-type
           home-pipewire-shepherd-service)
          (service-extension
           home-profile-service-type
           (const (list pipewire pulseaudio)))))
   (default-value #f)
   (description "Run Pipewire while also providing pipewire-pulse for backwards
compatibility.")))
