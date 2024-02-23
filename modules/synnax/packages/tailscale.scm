(define-module (synnax packages tailscale)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages linux)
  #:use-module ((nonguix licenses) #:prefix license:))

(define-public tailscale
  (package
   (name "tailscale")
   (version "1.60.0")
   (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_" version
                                  "_amd64.tgz"))
              (sha256
               (base32
                "0a44f28v3gbd0hmxffkvzdhcrnsz55jcaqxa1b63m18i8xf08nlq"))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan
     #~`((,(string-append "tailscale_" #$version "_amd64/tailscale") "/bin/")
         (,(string-append "tailscale_" #$version "_amd64/tailscaled") "/sbin/"))))
   (propagated-inputs
    (list iptables))
   (synopsis "Tailscale connects your team's devices and development environments
for easy access to remote resources.")
   (description "Tailscale is a zero config VPN for building secure networks.
Install on any device in minutes. Remote access from any network or physical
location.")
   (home-page "https://tailscale.com/")
   (license (license:nonfree "https://tailscale.com/legal"))))

tailscale
