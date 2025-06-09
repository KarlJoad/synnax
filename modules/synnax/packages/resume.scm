(define-module (synnax packages resume)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tex))

(define-public resume
  (let ((commit "82e0e809e33411be4815eb79448acd928f02b93e")
        (revision "5"))
    (package
     (name "karl-resume")
     (version (git-version "0.0.0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://karl.hallsby.com/resume.git")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1im38acgqmkkc5c9zdldsfs0gfwwqzf096kq238817y64sl1mxd3"))))
     (build-system gnu-build-system)
     (native-inputs
      (list perl
            texlive-scheme-medium
            (texlive-local-tree
             (list
              ;; Fonts
              texlive-collection-fontsrecommended
              texlive-collection-fontsextra
              texlive-latex-fonts
              texlive-ec
              texlive-amsfonts
              ;; Actual packages
              texlive-booktabs
              texlive-cm
              texlive-cm-super
              texlive-caption ; subcaption.sty
              texlive-chngcntr
              texlive-csquotes
              texlive-ctable
              texlive-datetime2 texlive-datetime2-english
              texlive-enumitem
              texlive-multirow
              texlive-titlesec
              texlive-titling
              texlive-transparent
              texlive-tools
              texlive-xcolor))))
     (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (add-before 'build 'set-HOME
             (lambda _
               (setenv "HOME" (getcwd))))
           (replace 'install
             (lambda _ (copy-file "Hallsby_Karl.pdf" #$output))))))
     (home-page "https://karl.hallsby.com")
     (synopsis "Personal Resume/CV built using LaTeX")
     (description "Karl Hallsby's personal resume/curriculum vitae built using LaTeX.")
     (license #f))))
