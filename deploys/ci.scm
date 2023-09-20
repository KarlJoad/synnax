(use-modules (gnu)
             (synnax systems ci-test))

;; NOTE: If using SSH-protected channels, MUST have nss-certs in globally
;; available packages!

(list (machine
       (operating-system ci-test)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
           ;; IP or DNS-resolved address of machine(s) to manage
           (host-name "Karl-CI.raven")
           (system "x86_64-linux")
           ;; SSH host key of system being configured
           (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA0V77v/o/3XvaK1daKdzCRu0EaALdJRdXzerGbnYcni root@(none)")
           (user "root")
           ;; SSH identity key for machine deploying to connect to remote
           (identity "~/.ssh/ci_rsa")))))
