(define-module (synnax packages wally)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages pkg-config)
  #:use-module (synnax packages go-xyz)
  #:use-module (gnu packages libusb)
  #:use-module ((guix licenses) #:prefix license:))

(define-public wally-cli
  (package
   (name "wally-cli")
   (version "2.0.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/zsa/wally-cli")
                  (commit (string-append version "-linux"))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1iswh1z7llapjn116lyr5lvry7q93zfaasxvm36q5jx09hf91v1n"))))
   (native-inputs
    (list pkg-config
          go-github-com-carrlos0-spin
          go-github-com-google-gousb
          go-github-com-marcinbor85-gohex
          go-github-com-mattn-go-runewidth
          go-golang-org-x-sys
          go-github-com-cheggaaa-pb-v1))
   (inputs (list libusb))
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/zsa/wally-cli"
      ;; We don't need to install the source code for end-user applications.
      #:install-source? #f
      #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-cheggaaa-pb-go-mod-require
           ;; Some files that use this package use a strange package path that
           ;; go-build-system does not like. So, we replace the imported
           ;; package's old name with this new one, which does work.
           (lambda* (#:key import-path #:allow-other-keys)
             (substitute* (list (string-append "src/" import-path "/go.mod")
                                (string-append "src/" import-path "/main.go"))
               (("gopkg.in/cheggaaa/pb.v1")
                "github.com/cheggaaa/pb/v1")))))))
   (home-page "https://ergodox-ez.com/pages/wally-planck")
   (synopsis "A tool to flash firmware to mechanical keyboards")
   (description "Provides a command-line interface for the Wally GUI utility for
flashing firmware to mechanical keyboards. Particularly for Ergodox, ZSA Moonlander,
and Planck keyboards.")
   (license license:expat)))
