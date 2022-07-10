(define-module (synnax packages go-xyz)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libusb)
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

(define-public go-github-com-google-gousb
  (package
   (name "go-github-com-google-gousb")
   (version "2.1.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/google/gousb")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1aki6hk009sicrf7gxy5nkjmj4j7lsy0by4kjgd9bwq8ragfyv5x"))))
   (native-inputs (list pkg-config libusb))
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/google/gousb"
      #:phases (modify-phases %standard-phases
                 ;; Delete the check phase because libusbContext and
                 ;; libusbDevHandle cannot be allocated in Go.
                 (delete 'check))))
   (home-page "https://github.com/google/gousb")
   (synopsis "Go-like bindings around the libusb library")
   (description "Go-like bindings around the libusb library")
   (license license:asl2.0)))

