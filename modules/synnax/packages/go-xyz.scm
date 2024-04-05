(define-module (synnax packages go-xyz)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module ((guix licenses) #:prefix license:))

(define-public go-github-com-sri-csl-gllvm
  (package
   (name "go-github-com-sri-csl-gllvm")
   (version "1.3.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/SRI-CSL/gllvm")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0c0xh3z9wcl9yrxh13xn08bqk40wfwhvpd98z2pzpf0iffmdx2ha"))))
   (build-system go-build-system)
   (arguments
    `(#:unpack-path "github.com/SRI-CSL/gllvm"
      #:import-path "github.com/SRI-CSL/gllvm/cmd/..."
      #:install-source? #f
      #:phases
      (modify-phases %standard-phases
                     (delete 'check))))
   (home-page "https://github.com/SRI-CSL/gllvm")
   (synopsis "Clang/LLVM wrapper that preserves LLVM bitcode")
   (description "Clang tool for preserving and inlining all LLVM bitcode during
compilation to use in a second pass for final optimization and code generation.")
   (license license:bsd-3)))
