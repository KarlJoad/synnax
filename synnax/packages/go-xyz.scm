(define-module (synnax packages go-xyz)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module ((guix licenses) #:prefix license:))

(define-public go-github-com-carrlos0-spin
  (package
   (name "go-github-com-carrlos0-spin")
   (version "1.1.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/caarlos0-graveyard/spin")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1pnijds4145j8nsxvq8203r2sg2pbk7x8prkdg2ilghhrzqj6vyc"))))
   (build-system go-build-system)
   (arguments '(#:import-path "github.com/caarlos0/spin"))
   (home-page "https://github.com/caarlos0-graveyard/spin")
   (synopsis "A very simple spinner for cli golang apps")
   (description "A very simple spinner for cli golang apps")
   (license license:expat)))

