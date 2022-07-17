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
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1avaz7gkrcyf1kz61s8zhkr4q6lj4wyaq5spxp6d9wc16gkvz40l"))))
   (build-system gnu-build-system)
   (native-inputs
    (list texlive-base texlive-tools
          texlive-latex-geometry texlive-latex-tools
          texlive-inputenx texlive-babel ;; texlive-nth
          texlive-xcolor
          texlive-hyperref
          ;; texlive-cleveref
          texlive-caption texlive-enumitem ;; texlive-chngcntr
          texlive-booktabs texlive-latex-multirow
          texlive-titling texlive-latex-titlesec))
   (arguments
    (list
     #:phases
     #~(modify-phases %standard-phases
                      (delete 'configure)
                      (delete 'check)
                      (replace 'install
                               (lambda _ (copy-file "Hallsby_Karl.pdf" #$output))))))
   (home-page "https://karl.hallsby.com")
   (synopsis "Personal Resume/CV built using LaTeX")
   (description "Karl Hallsby's personal resume/curriculum vitae built using LaTeX.")
   (license #f)))
