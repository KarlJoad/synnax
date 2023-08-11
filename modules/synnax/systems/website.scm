(define-module (synnax systems website)
  #:export (%website-system))

(use-modules (gnu)
             (gnu system)
             (synnax packages personal-website)
             (synnax services fstrim))

(use-package-modules
 vim
 certs
 pretty-print
 web
 version-control
 ssh
 admin)

(use-service-modules
 networking
 ssh
 version-control
 cgit
 web
 certbot
 security)

(define %certbot-nginx-deploy-hook
  (program-file
   "certbot-nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))

(define-public %website-system
  (operating-system
   (locale "en_US.utf8")
   (timezone "America/Chicago")
   (keyboard-layout (keyboard-layout "us"))
   (host-name "website")
   (packages
    (append
     (list vim
           git ;; FIXME: Remove git pakcage
           nss-certs)
     %base-packages))
   (services
    (append
     (list (service openssh-service-type
                    (openssh-configuration
                     (openssh openssh-sans-x)))
           (service fstrim-service-type)
           (service dhcp-client-service-type)
           (service git-daemon-service-type) ;; Allow cloning repos with git://
           (service nginx-service-type
                    (nginx-configuration
                     ;; (global-directives
                     ;;  `((pcre_jit . on)
                     ;; TODO: ssl_protocols TLSv1 TLSv1.1 TLSv1.2 TLSv1.3;
                     ;; TODO: ssl_prefer_server_ciphers on;
                     ;;    ))
                     (server-blocks
                      (list
                       ;; Personal website virtual server
                       (nginx-server-configuration
                        (listen '("80";; "443 ssl"
                                  ))
                        (server-name '("karl.hallsby.com"))
                        ;; (ssl-certificate
                        ;;  "/etc/letsencrypt/live/karl.hallsby.com/fullchain.pem")
                        ;; (ssl-certificate-key
                        ;;  "/etc/letsencrypt/live/karl.hallsby.com/privkey.pem")
                        (root personal-website)
                        (locations
                         (list
                          ;; NOTE: git-http is for cloning using HTTP, not browsing!
                          ;; If you browse, you will always get a black webpage
                          (git-http-nginx-location-configuration
                           (git-http-configuration)))))))))
           (service fcgiwrap-service-type) ;; Needed for git-http
           ;; TODO: Debug and fix certbot once we go live
           ;; Cannot refresh certs for karl.hallsby.com without running on that host.
           ;; (service certbot-service-type
           ;;          (certbot-configuration
           ;;           (email "karl@hallsby.com")
           ;;           (certificates
           ;;            (list
           ;;             ;; Just one domain. The git page is under /git now.
           ;;             (certificate-configuration
           ;;              (domains '("karl.hallsby.com" "www.karl.hallsby.com"))
           ;;              (deploy-hook %certbot-nginx-deploy-hook))))))
           (service fail2ban-service-type
                    (fail2ban-configuration
                     (extra-jails
                      (list
                       (fail2ban-jail-configuration
                        (name "sshd")
                        (enabled? #t)))))))
     %base-services))
   (bootloader
    (bootloader-configuration
     (bootloader grub-bootloader)
     (targets '("/dev/vda"))
     (keyboard-layout keyboard-layout)))
   (file-systems
    (append
     (list (file-system
            (device "/dev/vda1")
            (mount-point "/")
            (type "ext4")))
     %base-file-systems))))

%website-system
