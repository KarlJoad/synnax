(use-modules (gnu)
             (gnu machine)
             (gnu machine ssh)
             (synnax systems ci))

;; NOTE: If using SSH-protected channels, MUST have nss-certs in globally
;; available packages!

(list (machine
       (operating-system ci-system)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
           ;; IP or DNS-resolved address of machine(s) to manage
           (host-name "cuirass.raven")
           (system "x86_64-linux")
           ;; SSH host key of system being configured
           (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH4zVKw1lt6WlDzM9rIqeKIDAYHQkRzlG6zj2JXvG4Bs root@(none)")
           (user "root")
           ;; SSH identity key for machine deploying to connect to remote
           (identity "~/.ssh/cuirass_rsa")))))
