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
              "0bqgl63gi27vzhsrb8mc8xdhbrjaccgyjixjii0p3n9lw56slag7"))))
   (build-system gnu-build-system)
   (native-inputs
    (list texlive))
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
