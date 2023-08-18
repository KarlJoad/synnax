(define-module (synnax services web-deploy)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (website-deploy-service-type
            website-deploy-configuration website-deploy-configuration?
            site-configuration site-configuration?))

(define-configuration/no-serialization site-configuration
  (name
   (string "")
   "Name for this site.")
  (website-package
   (file-like)
   "Package output that this site deployment should copy from.")
  (target
   (string "/srv/http/site.com")
   "Target path for this site."))

(define (list-of-site-configurations? lst)
  (every site-configuration? lst))

(define-configuration/no-serialization website-deploy-configuration
  (sites
   (list-of-site-configurations '())
   "List of sites to deploy."))

(define (website-deploy-service config)
  (map (lambda (site-config)
         (let ((website-in-store (site-configuration-website-package site-config))
               (website-dir (site-configuration-target site-config)))
           (shepherd-service
            (requirement '(file-systems))
            (provision (list (string->symbol
                              (string-append "website-deploy-"
                                             (site-configuration-name site-config)))))
            (documentation (format #f "Copy ~a website out of store to @file{~a/}"
                                   (site-configuration-name site-config)
                                   website-dir))
            (start #~(lambda _
                       (mkdir-p #$website-dir)
                       ;; (guix build utils) already in scope for start by modules (guix) Shepherd Services
                       (copy-recursively #$website-in-store #$website-dir)
                       #t))
            (stop #~(lambda _
                      (with-exception-handler (lambda (e) (pk 'caught e))
                        (lambda () (delete-file-recursively #$website-dir))
                        #:unwind? #t)
                      #f)))))
       (website-deploy-configuration-sites config)))

(define website-deploy-service-type
  (service-type
   (name 'website-deploy)
   (description
    "Deploy the specified websites from the store to the specified locations.
Eacy site passed in the sites field of the website-deploy-configuration will
have its own shepherd service created.")
   (extensions (list (service-extension shepherd-root-service-type
                                        website-deploy-service)))
   (compose concatenate)
   (default-value #f)))

(define (generate-website-deploy-documentation)
  (generate-documentation `((website-deploy-configuration ,website-deploy-configuration-fields))
                          'website-deploy-configuration))
