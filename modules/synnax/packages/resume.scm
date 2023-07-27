(define-module (synnax packages resume)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages tex))

(define-public resume
  (let ((commit "91ad41a60b8be9a0243a6d4365afd96f2c702662")
        (revision "1"))
    (package
     (name "karl-resume")
     (version (git-version "0.0.0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.karl.hallsby.com/resume.git")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "094ryrxifpjni4grm09l80k3vkicbzrjs53y406kwrzyd07602q9"))))
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
     (license #f))))
