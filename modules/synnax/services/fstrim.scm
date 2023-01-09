(define-module (synnax services fstrim)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services mcron)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages linux)
  #:use-module (srfi srfi-1)
  #:export (fstrim-service-type
            fstrim-configuration fstrim-configuration?))

(define-record-type* <fstrim-configuration>
  fstrim-configuration make-fstrim-configuration
  fstrim-configuration?
  (package  fstrim-configuration-package  ;file-like
            (default util-linux))
  (interval fstrim-configuration-interval ;integer (seconds)
            (default (* 60 60 24 7))))

(define (fstrim-job configuration)
  ;; The time to run the job must be set by calculating the offset from now to
  ;; 7 days from now using math. This is needed because despite having
  ;; documentation that next-week and next-week-from exists, there are no procedures
  ;; defined in mcron for them.
  ;; Patch https://lists.gnu.org/archive/html/bug-mcron/2022-09/msg00000.html would
  ;; add this functionality, but remains to be merged into upstream mcron.
  (let ((fstrim-package (fstrim-configuration-package configuration)))
    (list
     #~(job (lambda (current-time)
              (+ current-time #$(fstrim-configuration-interval configuration)))
            #$(file-append fstrim-package
                           "/bin/fstrim --listed-in /etc/fstab:/proc/self/mountinfo --verbose --quiet-unsupported")))))

(define fstrim-service-type
  (service-type
   (name 'fstrim)
   (description
    "Periodically run TRIM command on all SSDs using fstrim.
By default, fstrim runs weekly.")
   (extensions (list (service-extension mcron-service-type
                                        fstrim-job)))
   (compose concatenate)
   (default-value (fstrim-configuration))))
