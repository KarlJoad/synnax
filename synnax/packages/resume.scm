(define-module (synnax packages resume)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages tex))

(define-public resume
  (package
   (name "karl-resume")
   (version "git")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "git://git.karl.hallsby.com/resume.git")
                  (commit "master")))
            (sha256
             (base32
              "098126y7v941dn6kr6m1mfbj714jswrwl59qmmh8nqsyg1pb4ij2"))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:phases
     #~(modify-phases %standard-phases
                      (delete 'configure)
                      (delete 'check)
                      (replace 'build
                               (lambda _ (system* (string-append #+texlive "/bin/latexmk"))))
                      (replace 'install
                               (lambda _ (copy-file "Hallsby_Karl.pdf" #$output))))))
   (home-page "https://karl.hallsby.com")
   (synopsis "Personal Resume/CV built using LaTeX")
   (description "Karl Hallsby's personal resume/curriculum vitae built using LaTeX.")
   (license #f)))
