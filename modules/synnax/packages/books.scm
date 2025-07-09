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

(define-public ruby-asciidoctor-diagram-ditaamini
  (package
    (name "ruby-asciidoctor-diagram-ditaamini")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "asciidoctor-diagram-ditaamini" version))
       (sha256
        (base32 "13h65bfbq7hc7z3kqn0m28w9c6ap7fikpjcvsdga6jg01slb4c56"))))
    (build-system ruby-build-system)
    (arguments
     (list
      ;; FIXME: Re-enable tests!
      #:tests? #f
      ;; #:phases
      ;; #~(modify-phases %standard-phases
      ;;     (replace 'check
      ;;       (invoke "bundle" "exec" "rake" "test")))
      ))
    (synopsis "Ditaa JAR files wrapped in a Ruby gem")
    (description "Ditaa JAR files wrapped in a Ruby gem.")
    (home-page "https://github.com/asciidoctor/asciidoctor-diagram")
    (license license:expat)))
