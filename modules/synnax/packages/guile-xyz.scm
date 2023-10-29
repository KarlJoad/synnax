(define-module (synnax packages guile-xyz)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages gnupg))

(define-public guile-basexx
  (let ((commit "8026bd0373c3b9eaed0c23b1c10f267471cafea5")
        (revision "0"))
  (package
    (name "guile-basexx")
    (version (git-version "0.0.1" revision commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/KarlJoad/guile-basexx.git")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kmn42f40ilz6pirqxhcdxfha7vfyp29lwf79cqq1xdl68lgwdap"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config texinfo guile-3.0))
    (inputs
     (list guile-gcrypt))
    (home-page "https://github.com/KarlJoad/guile-basexx.git")
    (synopsis "Base16/32/64 encoding/decoding library for Guile")
    (description "Base16/32/64 encoding/decoding library for Guile.
This is a @b{copy} of the base16/32/64 implementation in Guix! It has been
factored out to a separate library for any other project that wants to be able
to handle various base-encodings.")
    (license license:gpl3+))))
