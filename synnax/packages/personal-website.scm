(define-module (synnax packages personal-website)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (synnax packages resume))

(define-public personal-website
  (package
   (name "karl-personal-website")
   (version "git")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "git://git.karl.hallsby.com/haunt-website.git")
                  (commit "master")))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0lwnhjil5b8rbqnfvcrm8hjv0h9blk75010z0digwycjk5915z82"))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:phases
     #~(modify-phases %standard-phases
                      (delete 'configure)
                      (delete 'check)
                      (replace 'install
                               (lambda _ (copy-recursively "site" #$output)
                                       (mkdir-p (string-append #$output "/assets/pdf/resume"))
                                       (symlink #$resume (string-append #$output "/assets/pdf/resume/Hallsby_Karl.pdf")))))))
   (native-inputs
    `(("guile" ,guile-3.0)
      ("haunt" ,haunt)
      ("guile-reader" ,guile-reader)
      ("guile-commonmark" ,guile-commonmark)
      ("guile-syntax-highlight", guile-syntax-highlight)))
   (home-page "https://karl.hallsby.com")
   (synopsis "Personal website built using Haunt static site generator")
   (description "Karl Hallsby's personal website built using the Haunt static site
generator.")
   (license #f)))
