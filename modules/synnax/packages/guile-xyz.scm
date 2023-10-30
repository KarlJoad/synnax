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
  (package
   (name "guile-basexx")
   (version "0.0.1")
   (home-page "https://github.com/KarlJoad/guile-basexx")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url (string-append home-page ".git"))
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0akgi3vfnh9vjr3icchzxd7ps9l5wn1crdd61xl7jxsiq980rjhg"))))
   (build-system gnu-build-system)
   (native-inputs
    (list autoconf automake pkg-config texinfo guile-3.0))
   (inputs
    (list guile-gcrypt))
   (synopsis "Base16/32/64 encoding/decoding library for Guile")
   (description "Base16/32/64 encoding/decoding library for Guile.
This is a @b{copy} of the base16/32/64 implementation in Guix! It has been
factored out to a separate library for any other project that wants to be able
to handle various base-encodings.")
   (license license:gpl3+)))
