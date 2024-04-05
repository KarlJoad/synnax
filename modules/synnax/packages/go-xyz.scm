(define-module (synnax packages go-xyz)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
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

(define-public go-github-com-marcinbor85-gohex
  (let ((commit "55fb1c624d845f0f5b79ee946cf09a15cb50ed89")
        (revision "0"))
    (package
      (name "go-github-com-marcinbor85-gohex")
      (version commit)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/marcinbor85/gohex")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0klzqnvmkx6xvy9kc7hbsgpsdcrswnljsq8frf0jxddxi27qh1hn"))))
      (build-system go-build-system)
      (arguments '(#:import-path "github.com/marcinbor85/gohex"))
      (home-page "https://github.com/marcinbor85/gohex")
      (synopsis "A Go library for parsing Intel HEX files")
      (description "A Go library for parsing Intel HEX files")
      (license license:expat))))

(define-public go-github-com-cheggaaa-pb-v1
  (package
    (inherit go-github-com-cheggaaa-pb-v3)
    (name "go-github-com-cheggaaa-pb-v1")
    (version "1.0.28")
    (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/cheggaaa/pb")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "13a66cqbpdif804qj12z9ad8r24va9q41gfk71qbc4zg1wsxs3rh"))))
    (arguments
     '(#:import-path "github.com/cheggaaa/pb/v1"
       ;; XXX: it does have tests but I'm not sure how to run them.
       ;; go-build-system is looking in the wrong directory.
       #:tests? #f))))

(define-public go-github-com-sri-csl-gllvm
  (package
   (name "go-github-com-sri-csl-gllvm")
   (version "1.3.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/SRI-CSL/gllvm")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0c0xh3z9wcl9yrxh13xn08bqk40wfwhvpd98z2pzpf0iffmdx2ha"))))
   (build-system go-build-system)
   (arguments
    `(#:unpack-path "github.com/SRI-CSL/gllvm"
      #:import-path "github.com/SRI-CSL/gllvm/cmd/..."
      #:install-source? #f
      #:phases
      (modify-phases %standard-phases
                     (delete 'check))))
   (home-page "https://github.com/SRI-CSL/gllvm")
   (synopsis "Clang/LLVM wrapper that preserves LLVM bitcode")
   (description "Clang tool for preserving and inlining all LLVM bitcode during
compilation to use in a second pass for final optimization and code generation.")
   (license license:bsd-3)))
