(define-module (synnax packages resume)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages texlive))

(define-public resume
  (let ((commit "3786ef50d9c777ce488e1977ae5dc34cbc9d8a36")
        (revision "2"))
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
                "1xzacky7yg7fcqr2jh58bm0ssdixyygknv23j6fzsr8f59qcskpd"))))
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
