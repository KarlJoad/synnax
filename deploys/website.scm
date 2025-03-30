(use-modules (gnu)
             (synnax systems website))

(list (machine
       (operating-system %website-system)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
                       ;; IP or DNS-resolved address of machine(s) to manage
                       (host-name "raven.hallsby.com")
                       (system "x86_64-linux")
                       ;; Public SSH host key of system being configured
                       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFPpUuW06rpKos6uboMvqKqh5h0vU7o6awZrRtNYtTIb root@(none)")
                       (user "root")))))
