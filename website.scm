(use-modules (gnu)
             (synnax packages personal-website))
(use-service-modules networking ssh web
                     certbot
                     version-control
                     cgit
                     )
(use-package-modules bootloaders ssh version-control certs
                     tls ;; certbot
                     )

(define (cert-path host file)
  (format #f "/etc/letsencrypt/live/~a/~a.pem" host (symbol->string file)))

(define %certbot-nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))

(define %website-host
  (operating-system
   (host-name "Guix-VPS")
   (timezone "America/Chicago")
   (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (target "/dev/vda")
                (terminal-outputs '(console))))
   (swap-devices
    (list (swap-space
           (target
            (uuid "e97e64be-c6e5-4d12-87cc-09c6aaf8d154")))))
   (file-systems (cons (file-system
                        (mount-point "/")
                        (device
                         (uuid "70fd75bf-dc83-4d68-bd3e-5fad85f84276"
                               'ext4))
                        (type "ext4"))
                       %base-file-systems))
   (packages
    (append (list certbot
                  nss-certs)
            %base-packages))
   (services
    (append (list (service dhcp-client-service-type)
                  (service openssh-service-type
                           (openssh-configuration
                            (openssh openssh-sans-x)
                            (password-authentication? #false)
                            (permit-root-login #t)
                            (authorized-keys
                             ;; Authorise our SSH key.
                             ;; SSH access must be able to access/elevate to user in config list at bottom
                             `(("root" ,(local-file "/home/karl/.ssh/id_rsa.pub"))))))
                  ;; (service certbot-service-type
                  ;;          (certbot-configuration
                  ;;           (email "karl@hallsby.com")
                  ;;           (certificates
                  ;;            (list
                  ;;             (certificate-configuration
                  ;;              (name "Website")
                  ;;              (domains '("karl.hallsby.com" "www.karl.hallsby.com"))
                  ;;              (deploy-hook
                  ;;               %certbot-nginx-deploy-hook))
                  ;;             (certificate-configuration
                  ;;              (name "Git Viewer")
                  ;;              (domains '("git.karl.hallsby.com" "www.git.karl.hallsby.com"))
                  ;;              (deploy-hook
                  ;;               %certbot-nginx-deploy-hook))))))
                  (service nginx-service-type
                           (nginx-configuration
                            (server-blocks
                             (list (nginx-server-configuration
                                    (server-name '("karl.hallsby.com" "www.karl.hallsby.com"))
                                    (root personal-website)
                                    (listen '("80")) ;Stop 443 SSL port to allow nginx to start
                                    ;; (ssl-certificate (cert-path "karl.hallsby.com" 'fullchain))
                                    ;; (ssl-certificate-key (cert-path "karl.hallsby.com" 'privkey))
                                    )
                                   ))))
                  ;; (service fcgiwrap-service-type)
                  ;; (service git-daemon-service)
                  ;; (service cgit-service-type
                  ;;          (cgit-configuration
                  ;;           (nginx
                  ;;            (list (nginx-server-configuration
                  ;;                   ;; Most of this config comes from cgit.scm:%cgit-configuration-nginx
                  ;;                   (root cgit)
                  ;;                   (server-name '("git.karl.hallsby.com" "www.git.karl.hallsby.com"))
                  ;;                   (listen '("80"))
                  ;;                   (locations
                  ;;                    (list
                  ;;                     (nginx-location-configuration
                  ;;                      (uri "@cgit")
                  ;;                      (body '("fastcgi_param SCRIPT_FILENAME $document_root/lib/cgit/cgit.cgi;"
                  ;;                              "fastcgi_param PATH_INFO $uri;"
                  ;;                              "fastcgi_param QUERY_STRING $args;"
                  ;;                              "fastcgi_param HTTP_HOST $server_name;"
                  ;;                              "fastcgi_pass 127.0.0.1:9000;")))))
                  ;;                   (try-files (list "$uri" "@cgit"))
                  ;;                   (ssl-certificate (cert-path "git.karl.hallsby.com" 'fullchain))
                  ;;                   (ssl-certificate-key (cert-path "git.karl.hallsby.com" 'privkey)))))))
                  )
            (modify-services %base-services
                             (guix-service-type config =>
                                                (guix-configuration
                                                 (inherit config)
                                                 (authorized-keys
                                                  ;; Guix signing key generated by Guix in /etc/guix/
                                                  (append (list (local-file "/home/karl/guix/guix-coordinator.pub"))
                                                          %default-authorized-guix-keys)))))))))
(list (machine
       (operating-system %website-host)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       ;; IP or DNS-resolved address of machine(s) to manage
                       (host-name "45.76.23.26")
                       (system "x86_64-linux")
                       ;; SSH host key of system being configured
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKnv7f8EDm5o/ag/8zM54XI2RAp+KixlfLLay4ddJQty root@(none)")
                       (user "root")))))
