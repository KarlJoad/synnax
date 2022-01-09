(define-module (packages personal-website)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages guile)
  #:use-module (guix packages guile-xyz))

(define-public personal-website
  (package
   (name "karl-personal-website")
   (version "git")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "git://git.karl.hallsby.com/haunt-website.git")
                  (commit "master")))
            (sha256
             (base32
              "0000000000000000000000000000000000000000000000000000"))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:phases
     #~(modify-phases %standard-phases
                      (delete 'configure)
                      (delete 'check)
                      (replace 'install
                               (lambda _ (copy-recursively "site/" #$output))))))
   (native-inputs
    `(("guile" ,guile-3.0)
      ("haunt" ,haunt)
      ("guile-reader" ,guile-reader)
      ("guile-commonmark" ,guile-commonmark)
      ("guile-syntax-highlight", guile-syntax-highlight)))
   (synopsis "Personal website built using Haunt static site generator")
   (description "Karl Hallsby's personal website built using the Haunt static site
generator.")
   (license #f)))
