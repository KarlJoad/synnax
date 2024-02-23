(define-module (synnax services tailscale)
  #:use-module (guix gexp)
  #:use-module (guix least-authority)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module ((gnu system file-systems) #:select (file-system-mapping))
  #:use-module ((gnu build linux-container) #:select (%namespaces))
  #:use-module (gnu packages linux)
  #:use-module (synnax packages tailscale)
  #:export (tailscaled-service-type
            tailscaled-configuration))

(define-configuration/no-serialization tailscaled-configuration
  (package
   (file-like tailscale)
   "Tailscale package")
  (listen-port
   (number 41641)
   "Port for tailscaled to listen on.")
  (state-file
   (string "/var/lib/tailscale/tailscaled.state")
   "State file")
  (socket-file
   (string "/var/run/tailscale/tailscaled.sock")
   "Socket file")
  (no-logs?
   (boolean #f)
   "Whether to keep logs")
  (userspace-networking?
   (boolean #t)
   "Use socks5 proxy and userspace networking instead of @code{/dev/net/tun}?")
  (verbosity
   (number 0)
   "How noisy to make tailscaled's logging"))

(define (tailscaled-activation config)
  "Create the necessary directories for tailscale and run 'tailscaled
--cleanup' at startup, as recommended."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p (dirname #$(tailscaled-configuration-state-file config)))
        (mkdir-p (dirname #$(tailscaled-configuration-socket-file config)))
        (system* #$(file-append (tailscaled-configuration-package config)
                                "/sbin/tailscaled") "--cleanup"))))

;; FIXME: Can I do a match-record on the define-configuration?
(define (tailscaled-shepherd-service config)
  "Return a <shepherd-service> for Tailscaled with CONFIG"
  (let ((package (tailscaled-configuration-package config))
        (listen-port (tailscaled-configuration-listen-port config))
        (state-file (tailscaled-configuration-state-file config))
        (socket-file (tailscaled-configuration-socket-file config))
        (no-logs? (tailscaled-configuration-no-logs? config))
        (userspace-networking? (tailscaled-configuration-userspace-networking? config))
        (verbosity (tailscaled-configuration-verbosity config)))
    (list
     (shepherd-service
      (provision '(tailscaled))
      (documentation "Tailscaled networking daemon")
      (requirement '(networking))
      (start #~(make-forkexec-constructor
                (list #$(file-append package "/sbin/tailscaled")
                      ;; I would like to run tailscaled in a lead-authority-wrapper
                      ;; namespace, but that causes problems with /dev/net/tun.
                      ;; #$(least-authority-wrapper
                      ;;    (file-append package "/sbin/tailscaled")
                      ;;    #:name "tailscaled"
                      ;;    #:mappings (list (file-system-mapping
                      ;;                      (source "/var/lib/tailscale")
                      ;;                      (target source)
                      ;;                      (writable? #t))
                      ;;                     (file-system-mapping
                      ;;                      (source "/var/log")
                      ;;                      (target source)
                      ;;                      (writable? #t))
                      ;;                     (file-system-mapping
                      ;;                      (source "/var/run/tailscale")
                      ;;                      (target source)
                      ;;                      (writable? #t)))
                      ;;    ;; I am not sure which namespaces tailscaled needs to
                      ;;    ;; operate. We can (delq 'space %namespace) when we
                      ;;    ;; figure this out.
                      ;;    #:namespaces %namespaces)
                      #$@(if userspace-networking?
                             '()
                             '("--tun=userspace-networking"))
                      "-state" #$state-file
                      "-socket" #$socket-file
                      "-port" #$(number->string listen-port)
                      #$@(if no-logs?
                             '("-no-logs-no-support")
                             '())
                      "-verbose" #$(number->string verbosity))
                #:environment-variables (list (string-append
			                                         "PATH=" ; iptables is required for tailscale to work
			                                         #$(file-append iptables "/sbin")
			                                         ":"
			                                         #$(file-append iptables "/bin")))
                #:log-file "/var/log/tailscaled.log"))
      (stop #~(make-kill-destructor))))))

(define %tailscaled-log-rotation
  (list (log-rotation
         (files '("/var/log/tailscaled.log"))
         (options `("rotate 4"
                    ,@%default-log-rotation-options)))))

(define tailscaled-service-type
  (service-type
   (name 'tailscaled)
   (extensions
    (list (service-extension shepherd-root-service-type
                             tailscaled-shepherd-service)
          (service-extension activation-service-type
                             tailscaled-activation)
          (service-extension rottlog-service-type
                             (const %tailscaled-log-rotation))
          (service-extension profile-service-type
                             (compose list tailscaled-configuration-package))))
   (default-value (tailscaled-configuration))
   (description "Launch tailscaled.")))
