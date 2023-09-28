(define-module (synnax systems ci-test)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services cuirass)
  #:use-module (gnu services mcron)
  #:export (ci-test))

(use-package-modules
 emacs
 certs
 ssh)

(define %ci-specifications
  #~(list
     (specification
      (name "nanopass")
      (build '(channels nanopass))
      (channels
       (append (list
                (channel
                 (name 'nanopass)
                 (url "https://github.com/KarlJoad/nanopass-cl.git")
                 (branch "master")))
               %default-channels)))
     (specification
      (name "nanopass-docs")
      (build '(channels nanopass))
      (channels
       (append (list
                (channel
                 (name 'nanopass)
                 (url "https://github.com/KarlJoad/nanopass-cl.git")
                 (branch "docs")))
               %default-channels)))
     (specification
      (name "tcl2cl")
      (build '(channels tcl2cl))
      (channels
       (append (list
                (channel
                 (name 'tcl2cl)
                 (url "https://github.com/KarlJoad/tcl2cl.git")
                 (branch "master")))
               %default-channels)))
     (specification
      (name "chil")
      (build '(channels chil))
      (channels
       (append (list
                (channel
                 (name 'chil)
                 (url "https://github.com/KarlJoad/chil.git")
                 (branch "main")))
               %default-channels)))
     (specification
      (name "gloa")
      (build '(channels gloa))
      (channels
       (append (list
                (channel
                 (name 'gloa)
                 (url "https://github.com/KarlJoad/gloa.git")
                 (branch "master")))
               %default-channels)))
     (specification
      (name "glorri")
      (build '(channels glorri))
      (channels
       (append (list
                (channel
                 (name 'glorri)
                 (url "https://github.com/KarlJoad/glorri.git")
                 (branch "master")))
               %default-channels)))
     (specification
      (name "website")
      (build '(channels website))
      (channels
       (append (list
                (channel
                 (name 'website)
                 (url "git://karl.hallsby.com/website.git")
                 (branch "master")))
               %default-channels)))
     (specification
      (name "synnax")
      (build '(channels synnax))
      (channels
       (append (list (channel
                            (name 'synnax)
                            (url "https://github.com/KarlJoad/synnax.git"))
                           (channel
                            (name 'nonguix)
                            (url "https://gitlab.com/nonguix/nonguix"))
                           (channel (inherit %default-guix-channel)))
               %default-channels)))))

(define guix-gc-job
  #~(job (lambda (current-time)
           (+ current-time (* 60 60 24 5)))
         "guix gc"
         "Run Guix GC on regular interval"))

(define ci-test
  (operating-system
   (locale "en_US.utf8")
   (timezone "America/Chicago")
   (keyboard-layout (keyboard-layout "us"))
   (host-name "cuirass")
   (packages
    (append
     (list ;; emacs
           nss-certs)
     %base-packages))
   (services
    (append
     (list (service openssh-service-type
                    (openssh-configuration
                     (openssh openssh-sans-x)
                     (password-authentication? #f)
                     (permit-root-login 'prohibit-password)
                     (authorized-keys
                      `(("root" ,(local-file (string-append (getenv "HOME") "/.ssh/cuirass_rsa.pub")))))))
           (service dhcp-client-service-type)
           (service cuirass-service-type
                    (cuirass-configuration
                     (host "0.0.0.0")
                     (specifications %ci-specifications)
                     ;; Only poll upstreams once every x seconds
                     (interval (* 60 15))))
           (service mcron-service-type
                    (mcron-configuration
                     (jobs (list guix-gc-job)))))
     (modify-services %base-services
       (guix-service-type config =>
                          (guix-configuration
                           (inherit config)
                           (authorized-keys
                            (append (list (local-file
                                           (string-append (getenv "HOME")
                                                          "/Repos/synnax/deploys/desktop-guix-signing-key.pub")))
                                    %default-authorized-guix-keys)))))))
   (bootloader
    (bootloader-configuration
     (bootloader grub-bootloader)
     (targets '("/dev/vda"))
     (keyboard-layout keyboard-layout)))
   (swap-devices (list (swap-space
                        (target "/dev/vda2"))))
   (file-systems
    (append
     (list (file-system
            (device "/dev/vda3")
            (mount-point "/")
            (type "ext4")))
     %base-file-systems))
   ;; (bootloader
   ;;  (bootloader-configuration
   ;;   (bootloader grub-bootloader)
   ;;   (targets '("/dev/xvda"))
   ;;   (keyboard-layout keyboard-layout)))
   ;; (swap-devices (list (swap-space
   ;;                      (target "/dev/xvda2"))))
   ;; (file-systems
   ;;  (append
   ;;   (list (file-system
   ;;          (device "/dev/xvda3")
   ;;          (mount-point "/")
   ;;          (type "ext4")))
   ;;   %base-file-systems))
   ))

ci-test
