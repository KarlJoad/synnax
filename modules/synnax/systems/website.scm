(define-module (synnax systems website)
  #:use-module (ice-9 match)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu services certbot)
  #:use-module (gnu services cgit)
  #:use-module (gnu services networking)
  #:use-module (gnu services security)
  #:use-module (gnu services ssh)
  #:use-module (gnu services version-control)
  #:use-module (gnu services web)
  #:use-module (synnax packages website)
  #:use-module (synnax services fstrim)
  #:use-module (synnax services web-deploy)
  #:export (%website-system))

(define git-http-regex
  "^.*/(HEAD|info/refs|objects/info/.*|git-(upload|receive)-pack)$")

(define %certbot-nginx-deploy-hook
  (program-file
   "certbot-nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))

(define (nginx-add-header header-name header-vals)
  "Constructs an NGINX \"add_header\" directive for HEADER-NAME. HEADER-VALS is
a list of strings for the values to set for HEADER-NAME.

If something must be quoted and listed (i.e. Cache-Control or
Strict-Transport-Security), then it must be done by the caller!"
  (string-append
   (string-join `("add_header" ,header-name ,@header-vals))
   ";"))

(define (nginx-set-header header-alist)
  "Constructs an nginx-headers-more-module \"more_set_headers\" directive.
HEADER-VALS is an alist of header name and its value.

The header's value must be a single string!"
  (string-append
   (string-join
    `("more_set_headers"
      ,@(map (match-lambda
               ((header-name . header-vals)
                (string-append "\"" header-name ": " header-vals "\""))
               (_ (throw 'invalid-nginx-set-header-alist-item)))
             header-alist)))
   ";"))

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

When using preload, the max-age directive must be at least 31536000 (1
year), and the includeSubDomains directive must be present. Not part
of the specification.

https://www.nginx.com/blog/http-strict-transport-security-hsts-and-nginx/#Configuring-HSTS-in-NGINX-and-NGINX&nbsp;Plus"
  (let* ((actually-preload? (and preload? include-subdomains?
                                 (>= duration (* 60 60 24 365))))
         (policy (string-append
                  "\""
                  (string-join
                   (filter (lambda (s) (not (string-null? s)))
                           (list (format #f "max-age=~a" duration)
                                 (if include-subdomains? "includeSubDomains" "")
                                 (if actually-preload? "preload" "")))
                   "; " 'infix)
                  "\"")))
  (nginx-add-header "Strict-Transport-Security"
                    `(,policy
                      ,(if always-add? "always" "")))))

(define nginx-x-content-type-options-header
  (nginx-add-header "X-Content-Type-Options" '("nosniff")))

(define nginx-x-frame-options-header
  (nginx-add-header "X-Frame-Options" '("deny")))

(define* (nginx-x-xss-protection-header #:optional (enable? #t))
  (nginx-add-header "X-XSS-Protection"
                    `(,(if enable? "\"1; mode=block\"" "0"))))

(define* (nginx-referrer-policy-header #:key (policy-string "strict-origin-when-cross-origin"))
  (nginx-add-header "Referrer-Policy" `(,policy-string)))

(define (nginx-content-security-policy-header policies-alist)
  "Return Content-Security-Policy (CSP) HTTP header string from provided
POLICIES-ALIST."
  (define policy->string
    (match-lambda
      ;; NOTE: The ordering of these cases IS important! We need to match
      ;; an alist with a list as the value first, before a singleton string.
      ((policy-name . (policy-vals ..1))
       (format #f "~a ~a" (symbol->string policy-name) (string-join policy-vals)))
      ((policy-name . policy-vals)
       (format #f "~a ~a" (symbol->string policy-name) policy-vals))
      (_ (throw 'invalid-nginx-security-policy-format))))

  (define policies-strings
    (map policy->string policies-alist))

  (nginx-add-header
   "Content-Security-Policy"
   (list (string-append "\"" (string-join policies-strings "; ") "\""))))

(define* (nginx-client-side-cache-header #:key (age (* 60 60 24 365)))
  "Add a header to files with the provided EXTENSIONS allowing clients to cache
the content for AGE amount of time in seconds.
By default, age defaults to 1 year."
  (let ((cache-attrs (string-join
                      (list (string-append "max-age=" (number->string age))
                            "public" "must-revalidate")
                      ", ")))
    (list "access_log off;"
          (nginx-set-header `(("Cache-Control" . ,cache-attrs))))))

(define* (nginx-use-http2 #:key (use-http2? #t))
  (string-append "http2 "
                 (if use-http2? "on" "off")
                 ";"))

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

(define bad-bots
  (list
   "Amazonbot" "Amazonbot/0.1"
   "GPTBot" "GPTBot/1.2"
   "ClaudeBot" "ClaudeBot/1.0"
   "Bytespider" "PetalBot" "SemrushBot" "Semrush"
   "AhrefsBot" "YandexBot" "YandexImages" "MegaIndex.ru" "BLEXbot"
   "BLEXBot" "ZoominfoBot" "YaK" "VelenPublicWebCrawler" "SentiBot"
   "Vagabondo" "SEOkicks" "SEOkicks-Robot" "mtbot/1.1.0i" "SeznamBot"
   "DotBot" "Cliqzbot" "coccocbot" "python" "Scrap" "SiteCheck-sitecrawl"
   "MauiBot" "Java" "GumGum" "Clickagy" "AspiegelBot" "Yandex" "TkBot"
   "CCBot" "Qwantify" "MBCrawler" "serpstatbot" "AwarioSmartBot"
   "Semantici" "ScholarBot" "proximic" "MojeekBot" "GrapeshotCrawler"
   "IAScrawler" "linkdexbot" "contxbot" "PlurkBot" "PaperLiBot"
   "BomboraBot" "Leikibot" "weborama-fetcher" "NTENTbot"
   "Screaming Frog SEO Spider" "admantx-usaspb" "Eyeotabot"
   "VoluumDSP-content-bot" "SirdataBot" "adbeat_bot" "TTD-Content" "admantx"
   "Nimbostratus-Bot" "Mail.RU_Bot" "Quantcastboti" "Onespot-ScraperBot"
   "Taboolabot" "Baidu" "Jobboerse" "VoilaBot" "Sogou" "Jyxobot" "Exabot"
   "ZGrab" "Proximi" "Sosospider" "Accoona" "aiHitBot" "Genieo" "BecomeBot"
   "ConveraCrawler" "NerdyBot" "OutclicksBot" "findlinks" "JikeSpider"
   "Gigabot" "CatchBot" "Huaweisymantecspider" "Offline Explorer"
   "SiteSnagger" "TeleportPro" "WebCopier" "WebReaper" "WebStripper"
   "WebZIP" "Xaldon_WebSpider" "BackDoorBot" "AITCSRoboti"
   "Arachnophilia" "BackRub" "BlowFishi" "perl" "CherryPicker"
   "CyberSpyder" "EmailCollector" "Foobot" "GetURL" "httplib" "HTTrack"
   "LinkScan" "Openbot" "Snooper" "SuperBot" "URLSpiderPro" "MAZBot"
   "EchoboxBot" "SerendeputyBot" "LivelapBot" "linkfluence.com"
   "TweetmemeBot" "LinkisBot" "CrowdTanglebot"))

(define nginx-block-bad-bots
  (string-append
   "if ($http_user_agent ~* \""
   (string-join bad-bots "|" 'infix)
   "\") { return 403; }"))

(define nginx-gzip-types
  (string-join
   (list "text/css"
         "text/javascript"
         "text/xml"
         "text/plain"
         "text/x-component"
         "application/javascript"
         "application/x-javascript"
         "application/json"
         "application/xml"
         "application/rss+xml"
         "application/atom+xml"
         "font/truetype"
         "font/opentype"
         "application/vnd.ms-fontobject"
         "image/svg+xml")))

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
           )
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
           (service dhcpcd-service-type)
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
                     (global-directives
                      `((worker_processes . "auto")
                        ;; Guix's nginx is compiled with PCRE JIT enabled.
                        (pcre_jit . on)
                        (events . ((worker_connections . ,(* 10 1024))))))
                     (modules
                      (list
                       (file-append nginx-headers-more-module "\
/etc/nginx/modules/ngx_http_headers_more_filter_module.so")))
                     ;; Appended to http block
                     (extra-content
                      `("ssl_protocols TLSv1 TLSv1.1 TLSv1.2 TLSv1.3;" ; Disallow SSL
                        "ssl_prefer_server_ciphers on;"
                        "tcp_nopush on;"
                        "tcp_nodelay on;"
                        "sendfile on;"
                        "gzip on;"
                        "gzip_proxied expired no-cache no-store private auth;"
                        ;; text/html is always compressed by HTTPGzipModule
                        ,`("gzip_types " ,nginx-gzip-types ";")))
                     (server-blocks
                      (list
                       ;; Personal website virtual server
                       (nginx-server-configuration
                        (listen '("443 ssl"))
                        (server-name '("karl.hallsby.com" "raven.hallsby.com"))
                        (ssl-certificate
                         "/etc/letsencrypt/live/website/fullchain.pem")
                        (ssl-certificate-key
                         "/etc/letsencrypt/live/website/privkey.pem")
                        (root personal-website-destination)
                        (locations
                         (list
                          ;; Allow for caching of static assets for 30 days
                          (nginx-location-configuration
                           (uri "~* \\.(?:jpg|jpeg|gif|png|ico|cur|gz|svg|mp4|ogg|ogv|webm|htc|webp|avif)$")
                           (body `(,@(nginx-client-side-cache-header #:age (* 60 60 24 30))
                                   "etag on;"
                                   "if_modified_since exact;")))
                          ;; Cache the CSS, JS, and fonts for my _STATIC_ page for 1 year
                          (nginx-location-configuration
                           (uri "~* \\.(?:css|js|woff|woff2|ttf)$")
                           (body `(,@(nginx-client-side-cache-header)
                                   "etag on;"
                                   "if_modified_since exact;")))
                          ;; Redirect /cgit -> cgit.raven.hallsby.com
                          (nginx-location-configuration
                           (uri "= /cgit")
                           (body '("return 308 $scheme://cgit.raven.hallsby.com ;")))
                          ;; Redirect /cgit/repo.git -> cgit.raven.hallsby.com/repo.git
                          ;; If Git's cloning info is in the URI, e.g. /info/refs,
                          ;; then keep it along for the ride.
                          ;; We also need to preserve Git's query string too (the
                          ;; service=git-upload-pack), adding the ? with nginx's
                          ;; $is_args special variable.
                          (nginx-location-configuration
                           (uri "~ /cgit(/[^/\\s]+)(/[^\\s]+)?")
                           (body '("return 308 $scheme://cgit.raven.hallsby.com$1$2$is_args$query_string ;")))
                          ;; NOTE: git-http is for cloning using HTTP, not browsing!
                          ;; If you browse, you will always get a black webpage
                          (git-http-nginx-location-configuration
                           (git-http-configuration
                            (uri-path "/")))))
                        (raw-content
                         (list (nginx-hsts-header)
                               nginx-x-content-type-options-header
                               nginx-x-frame-options-header
                               (nginx-x-xss-protection-header)
                               (nginx-referrer-policy-header)
                               (nginx-content-security-policy-header
                                '((default-src . "'self'")
                                  (img-src . "'self'")
                                  (font-src . "'self'")
                                  (object-src . "'self'")
                                  (script-src . "'self'")
                                  (style-src . "'self'")
                                  (frame-ancestors . "'self'")
                                  (base-uri . "'self'")
                                  (form-action . "'self'")))
                               nginx-block-bad-bots
                               (nginx-use-http2)
                               (nginx-add-header "X-Clacks-Overhead"
                                                 '("\"GNU Terry Pratchett\"")))))))))
           (service fcgiwrap-service-type) ;; Needed for git-http
           ;; Cannot refresh certs for raven.hallsby.com without running on that host.
           ;; NOTE: You must run nginx with all domains' root set to /var/www for
           ;; certbot to work
           (service certbot-service-type
                    (certbot-configuration
                     (email "karl@hallsby.com")
                     (webroot "/srv/http/personal")
                     (certificates
                      (list
                       ;; The git page is under /cgit now.
                       (certificate-configuration
                        (name "website")
                        (domains '("karl.hallsby.com" "raven.hallsby.com"))
                        (deploy-hook %certbot-nginx-deploy-hook))
                       (certificate-configuration
                        (name "cgit")
                        (domains '("cgit.karl.hallsby.com" "cgit.raven.hallsby.com"))
                        (deploy-hook %certbot-nginx-deploy-hook))))))
           (service cgit-service-type
                    (cgit-configuration
                     (root-desc "Code is made for sharing!") ;; Thanks samplet!
                     (source-filter cgit-syntax-highlight-script)
                     (repository-directory "/srv/git/")
                     (enable-git-config? #t) ;; cgit reads repo info from .git/config
                     (virtual-root "/") ;; Build links relative to this
                     (favicon (file-append personal-website "/assets/favicon/favicon.ico"))
                     (clone-url
                      '("git://raven.hallsby.com/$CGIT_REPO_URL"
                        "https://raven.hallsby.com/cgit/$CGIT_REPO_URL"
                        "git://cgit.raven.hallsby.com/$CGIT_REPO_URL"
                        "https://cgit.raven.hallsby.com/$CGIT_REPO_URL"))
                     (enable-commit-graph? #t)
                     (enable-follow-links? #t)
                     (enable-index-links? #t)
                     (enable-log-filecount? #t)
                     (enable-log-linecount? #t)
                     (remove-suffix? #f) ; Keep .git suffix on URLs
                     (nginx
                      (list
                       (nginx-server-configuration
                        (listen '("443 ssl"))
                        (server-name '("cgit.karl.hallsby.com" "cgit.raven.hallsby.com"))
                        (ssl-certificate
                         "/etc/letsencrypt/live/cgit/fullchain.pem")
                        (ssl-certificate-key
                         "/etc/letsencrypt/live/cgit/privkey.pem")
                        (root "/srv/git/") ;; Sets $document_root
                        (index '("index.html"))
                        (try-files (list "$uri" "@cgit"))
                        (server-tokens? #f)
                        (raw-content
                         (list (nginx-hsts-header)
                               nginx-x-content-type-options-header
                               nginx-x-frame-options-header
                               (nginx-x-xss-protection-header)
                               (nginx-referrer-policy-header)
                               (nginx-content-security-policy-header
                                '((default-src . "'self'")
                                  (img-src . "'self'")
                                  (font-src . "'self'")
                                  (object-src . "'self'")
                                  (script-src . "'self'")
                                  (style-src . ("'self'" "'unsafe-inline'"))
                                  (frame-ancestors . "'self'")
                                  (base-uri . "'self'")
                                  (form-action . "'self'")))
                               (nginx-add-header
                                "Cache-Control"
                                `(,(string-append "\""
                                                  (string-join
                                                   (list
                                                    "no-cache"
                                                    "no-store"
                                                    "must-revalidate") ", " 'infix)
                                                  "\"")))
                               (nginx-add-header "Pragma" '("no-cache"))
                               (nginx-add-header "Expires" '("0"))
                               nginx-block-bad-bots
                               (nginx-use-http2)
                               (nginx-add-header "X-Clacks-Overhead"
                                                 '("\"GNU Terry Pratchett\""))))
                        (locations
                         (list
                          (nginx-location-configuration ;; So CSS & co. are found
                           (uri "~ ^/share/cgit/")
                           (body `(("root " ,cgit ";"))))
                          (nginx-location-configuration
                           (uri "~ /cgit(/[^/\\s]+)(/[^\\s]+)?") ;; server/git/repo.git is for HTTP git cloning
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
                                   "fastcgi_pass 127.0.0.1:9000;")))
                          ;; For cloning with cgit
                          ;; (nginx-location-configuration
                          ;;  (uri (string-append "~ " git-http-regex))
                          ;;  (body `("fastcgi_param CONTENT_LENGTH $content_length;"
                          ;;          "fastcgi_param CONTENT_TYPE $content_type;"
                          ;;          "fastcgi_param GIT_PROJECT_ROOT /srv/git;"
                          ;;          "fastcgi_param PATH_INFO $uri;"
                          ;;          "fastcgi_param QUERY_STRING $args;"
                          ;;          "fastcgi_param REQUEST_METHOD $request_method;"
                          ;;          ("fastcgi_param SCRIPT_FILENAME " ,git
                          ;;           "/libexec/git-core/git-http-backend;")
                          ;;          "fastcgi_pass 127.0.0.1:9000;")))
                          )))))))
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
