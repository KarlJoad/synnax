(define-module (synnax services home podman)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages linux)
  #:use-module (synnax services podman)
  #:use-module (srfi srfi-1)
  #:export (home-podman-configuration
            home-podman-service-type))

;; NOTE: I want to have podman and podman-compose as my packages list.

(define (list-of-package? ps) (every package? ps))

(define-configuration/no-serialization home-podman-configuration
  (packages
   (list-of-package (list podman))
   "List of packages to provide with this Podman configuration.")
  (config-files
   (container-configuration-files (container-configuration-files))
   "Configuration files.")
  (bash-alias
   (boolean #f)
   "Alias @code{podman} to @code{docker} in Bash.")
  (zsh-alias
   (boolean #f)
   "Alias @code{podman} to @code{docker} in Zsh.")
  (fish-alias
   (boolean #f)
   "Alias @code{podman} to @code{docker} in Fish."))

(define (generate-home-podman-documentation)
  (generate-documentation
   `((home-podman-configuration ,home-podman-configuration-fields))
   'home-podman-configuration))

(define (home-podman-packages-service config)
  "List of packages the user requested to add to the profile for Podman."
  (home-podman-configuration-packages config))

(define (home-podman-configuration-files-service config)
  (generate-container-configuration-files
   (home-podman-configuration-config-files config)))

(define (home-podman-bash-alias-service config)
  "Extend home-bash-configuration with alias from docker to podman if requested."
  (if (home-podman-configuration-bash-alias config)
      (home-bash-extension
       (aliases
        `(("docker" . ,(literal-string "podman")))))
      '()))

(define home-podman-service-type
  (service-type
   (name 'podman)
   (extensions
    (list (service-extension
           home-profile-service-type
           home-podman-packages-service)
          (service-extension
           home-xdg-configuration-files-service-type
           home-podman-configuration-files-service)
          ;; (service-extension
          ;;  home-bash-service-type
          ;;  home-podman-bash-alias-service)
          ))
   ;; (compose ...)
   ;; (extend ...)
   (default-value (home-podman-configuration))
   (description "Set up and configure Podman container engine for root-less
use.")))
