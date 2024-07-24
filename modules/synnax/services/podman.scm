(define-module (synnax services podman)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages linux)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (container-configuration-files container-configuration-files?
            generate-container-configuration-files

            podman-configuration
            podman-service-type))


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


;;;
;;; System-specific container utilities
;;;

(define-record-type <sub-id-map>
  (sub-id-map base range)
  sub-id-map?
  (base  sub-id-map-base)
  (range sub-id-map-range))


;;;
;;; Podman system-wide configuration
;;;

(define-configuration/no-serialization podman-configuration
  ;; TODO: Need an extension function for user-name, group-name, subuid, subgid,
  ;; and id-range match up correctly. It would be nice to do validation on those
  ;; too. You should NOT allow one id mapping to "cross into" another one. Prevent
  ;; the case of user1:100:50 and user2:110:50; UIDs collide, potentially causing
  ;; problems.
  (user-name
   (string "") ;; FIXME: Default value should match syncthing's.
   "Username that can use podman.")
  (subuid
   (integer 100000)
   "Sub-UID mapping.")
  (subgid
   (integer 100000)
   "Sub-GID mapping.")
  (id-range
   (integer 65536)
   "The maximum range for UID and GID, starting from subuid & subgid,
respectively.")
  (config-files
   (container-configuration-files (container-configuration-files))
   "Configuration files."))

(define (generate-podman-documentation)
  (generate-documentation
   `((podman-configuration ,podman-configuration-fields))
   'podman-configuration))

(define (podman-configuration-files-service config)
  (let ((podman-user-name (podman-configuration-user-name config))
        (podman-subuid (number->string
                        (podman-configuration-subuid config)))
        (podman-subgid (number->string
                        (podman-configuration-subgid config))))
  `(("subuid"
     ,(plain-file
       "podman-subuid"
       (string-append podman-user-name ":" podman-subuid ":65536\n")))

    ("subgid"
     ,(plain-file
       "podman-subgid"
       (string-append podman-user-name ":" podman-subgid ":65536\n")))

    ,@(generate-container-configuration-files
       (podman-configuration-config-files config)))))

;; TODO: Implement compose & extend to allow multiple users to have
;; podman-service-type defined for them.

(define podman-service-type
  (service-type
   (name 'podman)
   (extensions
    (list (service-extension
           etc-service-type
           podman-configuration-files-service)))
   ;; (compose concatenate)
   ;; (extend podman-extensions)
   (default-value #f)
   (description "Set up and configure Podman container engine at the system-level
for root-less use.")))
