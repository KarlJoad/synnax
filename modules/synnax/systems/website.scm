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
                          ;; Redirect /cgit traffic to cgit server
                          (nginx-location-configuration
                           (uri "/cgit")
                           (body '("return 308 $scheme://cgit.karl.hallsby.com ;")))
                          ;; /cgit/repo.git redirects to cgit.karl.hallsby.com/repo.git
                          (nginx-location-configuration
                           (uri "~ /cgit(/.*)")
                           (body '("return 308 $scheme://cgit.karl.hallsby.com$1 ;")))
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
           (service cgit-service-type
                    (cgit-configuration
                     (root-desc "Code is made for sharing!") ;; Thanks samplet!
                     (repository-directory "/srv/git/")
                     (enable-git-config? #t) ;; cgit reads repo info from .git/config
                     (virtual-root "/") ;; Build links relative to this
                     (nginx
                      (list
                       (nginx-server-configuration
                        (listen '("80"))
                        (server-name '("cgit.karl.hallsby.com"))
                        (root "/srv/git/") ;; Sets $document_root
                        (index '("index.html"))
                        (try-files (list "$uri" "@cgit"))
                        (server-tokens? #f)
                        (locations
                         (list
                          (nginx-location-configuration ;; So CSS & co. are found
                           (uri "~ ^/share/cgit/")
                           (body `(("root " ,cgit ";"))))
                          (nginx-location-configuration
                           (uri "~ /cgit(/.*)") ;; server/git/repo.git is for HTTP git cloning
                           (body `(("fastcgi_param SCRIPT_FILENAME " ,cgit "/lib/cgit/cgit.cgi;")
                                   ;; () is Nginx regex captures, numbered from 1
                                   "fastcgi_param PATH_INFO $1;" ; Grab /repo.git and use that as the path
                                   "fastcgi_param QUERY_STRING $args;"
                                   "fastcgi_param HTTP_HOST $server_name;"
                                   "fastcgi_pass 127.0.0.1:9000;")))
                          (nginx-named-location-configuration
                           (name "cgit")
                           (body `(("fastcgi_param SCRIPT_FILENAME " ,cgit "/lib/cgit/cgit.cgi;")
                                   "fastcgi_param PATH_INFO $uri;"
                                   "fastcgi_param QUERY_STRING $args;"
                                   "fastcgi_param HTTP_HOST $server_name;"
                                   "fastcgi_pass 127.0.0.1:9000;"))))))))))
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
