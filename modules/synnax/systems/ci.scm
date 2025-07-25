(define-module (synnax systems ci)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages vim)
  #:use-module (gnu services)
  #:use-module (gnu services avahi)
  #:use-module (gnu services cuirass)
  #:use-module (gnu services databases)
  #:use-module (gnu services mcron)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services virtualization)
  #:export (ci-system))

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
     ;; FIXME: A specification to serve ONLY the built output of nanopass-cl's
     ;; documentation. It does NOT serve any nanopass programs/files!
     ;; (specification
     ;;  (name "nanopass-docs")
     ;;  (build '(channels nanopass))
     ;;  (channels
     ;;   (append (list
     ;;            (channel
     ;;             (name 'nanopass)
     ;;             (url "https://github.com/KarlJoad/nanopass-cl.git")
     ;;             (branch "docs")))
     ;;           %default-channels)))
     (specification
      (name "chilir")
      (build '(channels chilir))
      (channels
       (append (list
                (channel
                 (name 'chilir)
                 (url "https://github.com/Chil-HW/chilir.git")
                 (branch "main")))
               %default-channels)))
     (specification
      (name "tcl2cl")
      (build '(channels tcl2cl))
      (channels
       (append (list
                (channel
                 (name 'tcl2cl)
                 (url "https://github.com/Chil-HW/tcl2cl.git")
                 (branch "master")))
               %default-channels)))
     (specification
      (name "chil")
      (build '(channels chil))
      (channels
       (append (list
                (channel
                 (name 'chil)
                 (url "https://github.com/Chil-HW/chil.git")
                 (branch "main")))
               %default-channels)))
     (specification
      (name "shwatt")
      (build '(channels shwatt))
      (channels
       (append (list
                (channel
                 (name 'shwatt)
                 (url "https://github.com/Chil-HW/shwatt.git")
                 (branch "master")))
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
      (name "lispy-key")
      (build '(channels lispy-key))
      (channels
       (append (list
                (channel
                 (name 'lispy-key)
                 (url "https://github.com/KarlJoad/lispy-key.git")
                 (branch "master")))
               %default-channels)))
     (specification
      (name "guile-yaml")
      (build '(channels guile-yaml))
      (channels
       (append (list
                (channel
                 (name 'guile-yaml)
                 (url "https://github.com/KarlJoad/guile-yaml.git")
                 (branch "master")))
               %default-channels)))
     (specification
      (name "website")
      (build '(channels website))
      (channels
       (append (list
                (channel
                 (name 'website)
                 (url "https://cgit.karl.hallsby.com/website.git")
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
                      (url "https://gitlab.com/nonguix/nonguix")))
               %default-channels)))))

(define guix-gc-job
  #~(job (lambda (current-time)
           ;; Run every 5 days, give or take.
           (+ current-time (* 60 60 24 5)))
         "guix gc"
         "Run Guix GC on regular interval"))

(define ci-system
  (operating-system
   (locale "en_US.utf8")
   (timezone "America/Chicago")
   (keyboard-layout (keyboard-layout "us"))
   (host-name "cuirass")

   (packages
    (append
     (list tmux vim)
     %base-packages))

   (services
    (append
     (list
      (service xe-guest-utilities-service-type)
      (service openssh-service-type
               (openssh-configuration
                (openssh openssh-sans-x)
                (password-authentication? #f)
                (permit-root-login 'prohibit-password)
                (authorized-keys
                 `(("root" ,(local-file (string-append (getenv "HOME") "/.ssh/cuirass_rsa.pub")))))))
      (service dhcpcd-service-type)
      (service ntp-service-type)
      (service postgresql-service-type
               (postgresql-configuration
                (postgresql postgresql)))
      (service postgresql-role-service-type)
      (service cuirass-service-type
               (cuirass-configuration
                (host "0.0.0.0")
                (specifications %ci-specifications)
                ;; Only poll upstreams once every x seconds
                (interval (* 60 30))
                (remote-server
                 (cuirass-remote-server-configuration))
                ;; (parameters
                ;;  (plain-file "cuirass-parameters"
                ;;              "((%zabbix-url \"http://localhost:15412/api_jsonrpc.php\")
                ;;                  (%zabbix-user \"zabbix\")
                ;;                  (%zabbix-password \"zabbix-test\"))"))
                ))
      (service cuirass-remote-worker-service-type
               (cuirass-remote-worker-configuration
                (workers 2)))
      ;; (service zabbix-server-service-type
      ;;          (zabbix-server-configuration
      ;;           (db-password "zabbix-test")
      ;;           (log-type "file")))
      ;; (service zabbix-agent-service-type
      ;;          (zabbix-agent-configuration
      ;;           (log-type "file")))
      ;; (service zabbix-front-end-service-type
      ;;          (zabbix-front-end-configuration
      ;;           (db-password "zabbix-test")))
      ;; Cuirass is not working because of the .raven TLD I have set for
      ;; myself!
      ;; NOTE: The problem here was that I had 2 cuirass remote-servers
      ;; running simultaneously, and the multicasting was getting mixed
      ;; up.
      ;; https://forum.manjaro.org/t/multicast-not-working-avahi-shows-no-services/117891/7
      ;; (guix) Networking Services
      ;; https://0pointer.de/lennart/projects/nss-mdns/
      ;; (guix) Name Service Switch
      (service avahi-service-type
               (avahi-configuration
                (debug? #t)))
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
     (targets '("/dev/xvda"))
     (keyboard-layout keyboard-layout)))
   (swap-devices
    (list
     (swap-space
      (target (uuid "bb7db1f3-7fe2-4c32-b101-fed7eca95f07")))))
   (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "11d6afaa-e2ff-47da-8d22-9968b1509751"
                                  'ext4))
                         (type "ext4"))
                        %base-file-systems))))

ci-system
