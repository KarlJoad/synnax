(define-module (synnax packages resume)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tex))

(define-public resume
  (let ((commit "af791397a75540419a9ce050d0f779b3cea13695")
        (revision "4"))
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
                "1az1p5cj9j96mpyivf8m4pwkiv64g9sak10qbf6ff1xla3gvq8lh"))))
     (build-system gnu-build-system)
     (native-inputs
      (list perl
            texlive-scheme-medium
            texlive-csquotes
            texlive-enumitem
            texlive-chngcntr
            texlive-transparent
            texlive-multirow
            texlive-titling
            texlive-titlesec
            texlive-datetime2 texlive-datetime2-english
            texlive-tools
            (texlive-updmap.cfg
             (list
              texlive-collection-fontsrecommended
              texlive-collection-fontsextra
              texlive-latex-fonts
              texlive-ec
              texlive-amsfonts
              texlive-cm
              texlive-cm-super))))
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
