(define-module (synnax services podman)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages linux)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (container-configuration-files
            container-configuration-files?
            generate-container-configuration-files))


;;;
;;; General tools for Podman
;;;

(define (list-of-file-like? fs) (every file-like? fs))
(define (list-of-string? strs) (every string? strs))

(define-configuration/no-serialization container-configuration-files
  (unqualified-search-registries
   (list-of-string (list))
   "List of URLs podman can search for \"unqualified\" container images.")
  (image-trust-policy
   (string "insecureAcceptAnything")
   "Where to allow container images to be downloaded from.")
  (storage-driver
   (string "overlay")
   "Driver to use for Storage.")
  (extra-files
   (list-of-file-like (list))
   "Extra file-like configuration files to install."))

(define (generate-container-configuration-files-documentation)
  (generate-documentation
   `((container-configuration-files-configuration ,container-configuration-files-configuration-fields))
   'container-configuration-files-configuration))

(define (generate-container-configuration-files configs)
  `(("containers/registries.conf"
     ;; Where can we search for and resolve shorter names to fully-qualified
     ;; names?
     ,(mixed-text-file
       "podman-registries.conf"
       "unqualified-search-registries = ["
       (string-join
        (map (lambda (url) (string-append "'" url "'"))
             (container-configuration-files-unqualified-search-registries configs))
        ", ")
       "]"))

    ("containers/policy.json"
     ;; Allow pulling from any remote repository
     ;; Policy for obtaining images
     ;; TODO: Use configuration option
     ,(mixed-text-file
       "podman-policy.json"
       "{\"default\": [{\"type\": \""
       (container-configuration-files-image-trust-policy configs)
       "\"}]}"))

    ("containers/storage.conf"
     ;; How should the container map its storage back to the actual device?
     ;; Use a faster storage driver
     ,(mixed-text-file
       "podman-storage.conf"
       "[storage]\ndriver = \""
       (container-configuration-files-storage-driver configs)
       "\""))

    ;; FIXME: Actually use extra-files!
    ))
