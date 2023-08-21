(define-module (synnax systems website)
  #:export (%website-system))

(use-modules (gnu)
             (gnu system)
             (synnax packages personal-website)
             (synnax services fstrim)
             (synnax services web-deploy))

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

(define* (nginx-hsts-header #:key (duration (* 60 60 24 365))
                            (include-subdomains? #t)
                            (preload? #t)
                            (always-add? #t))
  "Build add_header Nginx directive to add an HSTS header that is valid for
DURATION in seconds. If INCLUDE-SUBDOMAINS? is #t, then this includes all
subdomains which this server may redirect to. If ALWAYS-ADD? is #t, then the
header is added, regardless of the request's returned status code.

By default, DURATION is set to 1 year (31536000 seconds), and both
INCLUDE-SUBDOMAINS? and ALWAYS-ADD? are set to #t.

https://www.nginx.com/blog/http-strict-transport-security-hsts-and-nginx/#Configuring-HSTS-in-NGINX-and-NGINX&nbsp;Plus"
  (string-append "add_header Strict-Transport-Security \""
                 (string-join (filter
                               (lambda (s) (not (string-null? s)))
                               (list (format #f "max-age=~a" duration)
                                     (if include-subdomains? "includeSubDomains" "")
                                     (if preload? "preload" "")))
                              "; " 'suffix)
                 "\" "
                 (if always-add? "always" "") ";"))

(define nginx-x-content-type-options-header
  "add_header X-Content-Type-Options nosniff;")

(define nginx-x-frame-options-header
  "add_header X-Frame-Options deny;")

(define* (nginx-x-xss-protection-header #:optional (enable? #t))
  (string-append "add_header "
                 "X-XSS-Protection "
                 (if enable? "\"1; mode=block\"" "0")
                 ";"))

(define* (nginx-referrer-policy-header #:key (policy-string "strict-origin-when-cross-origin"))
  (string-append "add_header "
                 "Referrer-Policy "
                 policy-string
                 ";"))

(define nginx-content-security-policy-header
  (string-append "add_header "
                 "Content-Security-Policy "
                 "\""
                 (string-join
                  (list "default-src 'self' https:"
                        "img-src 'self' https:"
                        "font-src 'self' https:"
                        "object-src 'self' https:"
                        "script-src 'self' 'unsafe-inline' https:"
                        "style-src 'self' 'unsafe-inline' https:"
                        "frame-ancestors 'self' https:")
                  "; " 'infix)
                 "\""
                 ";"))

(define* (nginx-client-side-cache-header #:key (age (* 60 60 24 365)))
  "Add a header to files with the provided EXTENSIONS allowing clients to cache
the content for AGE amount of time in seconds.
By default, age defaults to 1 year."
  (list "access_log off;"
        (string-append "add_header Cache-Control \"max-age=" (number->string age)
                       "\";")))

(define cgit-syntax-highlight-script
  (program-file
   "cgit-highlight-script"
   #~(begin
       (use-modules (srfi srfi-2) (ice-9 textual-ports) (ice-9 match))
       (define (file-extension path)
         "Attempt to get the extension of the file name at PATH.
If there is no period anywhere in the path name, return \"txt\".
This is intended to be used with highlight, and defaulting to a plaintext file
if there is no matching extension."
         ;; If Makefile, use mk extension. Try to grab extension. If none, default to txt.
         (if (string= (basename path) "Makefile") "mk"
             (or (and-let* ((pos (string-index-right path #\.))) (string-drop path (1+ pos)))
                 "txt")))

       ;; Environment variables taken from https://git.zx2c4.com/cgit/tree/filters/syntax-highlighting.sh
       (define cgit-env-vars-alist
         `((repo-url . ,(getenv "CGIT_REPO_URL"))
           (repo-name . ,(getenv "CGIT_REPO_NAME"))
           (repo-path . ,(getenv "CGIT_REPO_PATH"))
           (repo-owner . ,(getenv "CGIT_REPO_OWNER"))
           (repo-defbranch . ,(getenv "CGIT_REPO_DEFBRANCH"))
           (repo-section . ,(getenv "CGIT_REPO_SECTION"))
           (repo-clone-url . ,(getenv "CGIT_REPO_CLONE_URL"))))

       ;; File name is passed on command line. File contents passed on STDIN.
       ;; See source-filter under https://git.zx2c4.com/cgit/tree/cgitrc.5.txt
       (let ((file-to-highlight ((match-lambda ((_ file) file)) (program-arguments))))
         (set-current-error-port (open-output-file "/dev/null"))
         (execl #$(file-append highlight "/bin/highlight")
                "highlight"
                "--force"            ;; ALWAYS generate an output, even if unknown input type
                "--inline-css"       ;; CSS embedded in highlighted output
                "-f"                 ;; Omit HTML Header and Footer tags
                "-I"                 ;; Include style information in the output
                "-O" "xhtml"         ;; Output format
                "-S" (file-extension file-to-highlight))))))

(define personal-website-destination "/srv/http/personal")

(define-public %website-system
  (operating-system
   (locale "en_US.utf8")
   (timezone "America/Chicago")
   (keyboard-layout (keyboard-layout "us"))
   (host-name "website")
   (users (cons* (user-account
                   (name "git")
                   (group "git")
                   (comment "Git separation user")
                   (home-directory "/home/git")
                   (supplementary-groups `("git-daemon")))
                 %base-user-accounts))
   (groups (cons* (user-group (name "git")
                              (system? #t))
                  %base-groups))
   (packages
    (append
     (list vim
           git ;; Useful to have git for git operations.
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
                      `(("root" ,(local-file (string-append (getenv "HOME") "/.ssh/website_rsa.pub")))
                        ("git" ,(local-file (string-append (getenv "HOME") "/.ssh/website_rsa.pub")))))))
           (service fstrim-service-type)
           (service dhcp-client-service-type)
           (service git-daemon-service-type) ;; Allow cloning repos with git://
           (service website-deploy-service-type
                    (website-deploy-configuration
                     (sites (list
                             (site-configuration
                              (name "personal")
                              (website-package personal-website)
                              (target personal-website-destination))))))
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
                        (listen '("443 ssl"))
                        (server-name '("karl.hallsby.com"))
                        (ssl-certificate
                         "/etc/letsencrypt/live/website/fullchain.pem")
                        (ssl-certificate-key
                         "/etc/letsencrypt/live/website/privkey.pem")
                        (root personal-website-destination)
                        (locations
                         (list
                          ;; Allow for caching of static assets for 30 days
                          (nginx-location-configuration
                           (uri "~* \\.(?:jpg|jpeg|gif|png|ico|cur|gz|svg|mp4|ogg|ogv|webm|htc|webp|avif)$ ")
                           (body `(,@(nginx-client-side-cache-header #:age (* 60 60 24 30)))))
                          ;; Cache the CSS & JS for my _STATIC_ page for 1 year
                          (nginx-location-configuration
                           (uri "~* \\.(?:css|js)$ ")
                           (body `(,@(nginx-client-side-cache-header))))
                          ;; Redirect /cgit -> cgit.karl.hallsby.com
                          (nginx-location-configuration
                           (uri "= /cgit")
                           (body '("return 308 $scheme://cgit.karl.hallsby.com ;")))
                          ;; Redirect /cgit/repo.git -> cgit.karl.hallsby.com/repo.git
                          (nginx-location-configuration
                           (uri "~ /cgit(/.*)")
                           (body '("return 308 $scheme://cgit.karl.hallsby.com$1 ;")))
                          ;; NOTE: git-http is for cloning using HTTP, not browsing!
                          ;; If you browse, you will always get a black webpage
                          (git-http-nginx-location-configuration
                           (git-http-configuration))))
                        (raw-content (list (nginx-hsts-header)
                                           nginx-x-content-type-options-header
                                           nginx-x-frame-options-header
                                           (nginx-x-xss-protection-header)
                                           (nginx-referrer-policy-header)
                                           nginx-content-security-policy-header)))))))
           (service fcgiwrap-service-type) ;; Needed for git-http
           ;; Cannot refresh certs for karl.hallsby.com without running on that host.
           ;; NOTE: You must run nginx with all domains' root set to /var/www for
           ;; certbot to work
           (service certbot-service-type
                    (certbot-configuration
                     (email "karl@hallsby.com")
                     (certificates
                      (list
                       ;; The git page is under /cgit now.
                       (certificate-configuration
                        (name "website")
                        (domains '("karl.hallsby.com"))
                        (deploy-hook %certbot-nginx-deploy-hook))
                       (certificate-configuration
                        (name "cgit")
                        (domains '("cgit.karl.hallsby.com"))
                        (deploy-hook %certbot-nginx-deploy-hook))))))
           (service cgit-service-type
                    (cgit-configuration
                     (root-desc "Code is made for sharing!") ;; Thanks samplet!
                     (source-filter cgit-syntax-highlight-script)
                     (repository-directory "/srv/git/")
                     (enable-git-config? #t) ;; cgit reads repo info from .git/config
                     (virtual-root "/") ;; Build links relative to this
                     (favicon (file-append personal-website "/assets/favicon/favicon.ico"))
                     (nginx
                      (list
                       (nginx-server-configuration
                        (listen '("443 ssl"))
                        (server-name '("cgit.karl.hallsby.com"))
                        (ssl-certificate
                         "/etc/letsencrypt/live/cgit/fullchain.pem")
                        (ssl-certificate-key
                         "/etc/letsencrypt/live/cgit/privkey.pem")
                        (root "/srv/git/") ;; Sets $document_root
                        (index '("index.html"))
                        (try-files (list "$uri" "@cgit"))
                        (server-tokens? #f)
                        (raw-content (list (nginx-hsts-header)
                                           nginx-x-content-type-options-header
                                           nginx-x-frame-options-header
                                           (nginx-x-xss-protection-header)
                                           (nginx-referrer-policy-header)
                                           nginx-content-security-policy-header
                                           "add_header Cache-Control \"no-cache, no-store, must-revalidate\";"
                                           "add_header Pragma no-cache;"
                                           "add_header Expires 0;"))
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
     %base-file-systems))))

%website-system
