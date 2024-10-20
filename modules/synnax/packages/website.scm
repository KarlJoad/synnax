(define-module (synnax packages website)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (synnax packages resume))

(define-public personal-website
  (let ((commit "7961ed458dc39d11add77f65aee24e29fc6d74b0")
        (revision "16"))
    (package
     (name "karl-personal-website")
     (version (git-version "0.0.0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://karl.hallsby.com/website.git")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "138b8jzdlvqc0j4cy35flrn6rqlb9hq4a1b1aqia2wv6zqsdc42q"))))
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
        ("guile-reader" ,guile-reader)
        ("guile-commonmark" ,guile-commonmark)
        ("guile-syntax-highlight" ,guile-syntax-highlight)
        ;; Emacs is needed to build the built-in Modus-themes CSS
        ("emacs" ,emacs-minimal)))
     (inputs
      `(("haunt" ,haunt)))
     (home-page "https://karl.hallsby.com")
     (synopsis "Personal website built using Haunt static site generator")
     (description "Karl Hallsby's personal website built using the Haunt static site
generator.")
     (license #f))))
