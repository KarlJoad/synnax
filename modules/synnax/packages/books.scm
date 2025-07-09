(define-module (synnax packages books)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system ruby)
  #:use-module (gnu packages ruby-xyz)
  #:use-module ((guix licenses) #:prefix license:))

;; TODO: Put this in Guix!
(define-public ruby-asciidoctor-diagram
  (package
    (name "ruby-asciidoctor-diagram")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "asciidoctor-diagram" version))
       (sha256
        (base32 "0ij22gfr5pbjda47ychgy4z26c976q34d4lcas013saiq2y9ln22"))))
    (build-system ruby-build-system)
    (arguments
     (list
      ;; FIXME: Re-enable tests!
      #:tests? #f))
    (propagated-inputs (list ruby-asciidoctor ruby-rexml))
    (synopsis "Asciidoctor diagramming extension")
    (description "Asciidoctor diagramming extension.")
    (home-page "https://github.com/asciidoctor/asciidoctor-diagram")
    (license license:expat)))
