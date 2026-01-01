(define-module (synnax packages books)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ruby)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages java)
  #:use-module (gnu packages java-graphics)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages ruby-xyz)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wget)
  #:use-module ((guix licenses) #:prefix license:))

;; TODO: Put this in Guix!
(define-public ruby-asciidoctor-diagram
  (package
    (name "ruby-asciidoctor-diagram")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "asciidoctor-diagram" version))
       (sha256
        (base32 "0ij22gfr5pbjda47ychgy4z26c976q34d4lcas013saiq2y9ln22"))))
    (build-system ruby-build-system)
    (arguments
     (list
      ;; FIXME: Re-enable tests!
      #:tests? #f))
    (propagated-inputs (list ruby-asciidoctor ruby-rexml))
    (synopsis "Asciidoctor diagramming extension")
    (description "Asciidoctor diagramming extension.")
    (home-page "https://github.com/asciidoctor/asciidoctor-diagram")
    (license license:expat)))

(define-public ruby-asciidoctor-diagram-ditaamini
  (package
    (name "ruby-asciidoctor-diagram-ditaamini")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "asciidoctor-diagram-ditaamini" version))
       (sha256
        (base32 "13h65bfbq7hc7z3kqn0m28w9c6ap7fikpjcvsdga6jg01slb4c56"))))
    (build-system ruby-build-system)
    (arguments
     (list
      ;; FIXME: Re-enable tests!
      #:tests? #f
      ;; #:phases
      ;; #~(modify-phases %standard-phases
      ;;     (replace 'check
      ;;       (invoke "bundle" "exec" "rake" "test")))
      ))
    (synopsis "Ditaa JAR files wrapped in a Ruby gem")
    (description "Ditaa JAR files wrapped in a Ruby gem.")
    (home-page "https://github.com/asciidoctor/asciidoctor-diagram")
    (license license:expat)))

(define-public beambook
  (package
    (name "beam-book")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/happi/theBeamBook")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gzmdlp6h8lwfanahppx8r475chk8k460g7qpavccvp7bagblh86"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; #:make-flags #~(list "pdf-a4" "pdf-publish" "html" "epub")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'copy-beam-genop.tab
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((otp (assoc-ref inputs "beam-book-OTP-28.0-checkout")))
                (copy-recursively
                 (string-append otp "/lib/compiler/src/genop.tab")
                 "genop.tab"))))
          (add-before 'build 'set-font-env-vars
            (lambda* _
              (setenv "XDG_CACHE_HOME" "/tmp")))
          (delete 'configure)
          ;; TODO: Build epub and pdf-publish versions of book too.
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (mkdir-p out)
                (install-file "beam-book-a4.pdf" out)
                (copy-recursively "site" out))))
          (delete 'check))))
    (native-inputs
     (list git-minimal ; Only used to build author/contributor list
           rsync
           erlang
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/erlang/otp")
                   ;; The exact commit used is important ONLY IF generic
                   ;; instructions emitted by the compiler change.
                   (commit "OTP-28.0")))
             (file-name (git-file-name name "OTP-28.0"))
             (sha256
              (base32 "18glv9wh05i4jvr1snq92m7rr1wkiixw9kx74h0ywcgx1p021rps")))
           gnu-make
           ruby
           openjdk
           graphviz
           ditaa
           ruby-asciidoctor
           ruby-asciidoctor-pdf
           ruby-asciidoctor-diagram
           ruby-asciidoctor-diagram-ditaamini
           ruby-rouge))
    (home-page "https://github.com/happi/theBeamBook/")
    (synopsis "")
    (description "")
    (license
     (list license:cc-by-sa4.0 ;; The book itself
           ;; Erlang's genop.tab
           license:asl2.0))))

(define-public emacs-lisp-elements
  (package
   (name "emacs-lisp-elements")
   (version "2.0.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/protesilaos/emacs-lisp-elements")
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0w3jxgkynq37rn7c17pnricykqf3gmq4crsvkz2j2g3hc0ydf6qp"))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:tests? #f ; No code, no tests
     #:phases
     #~(modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'remove-artifacts
           (lambda _
             (delete-file "elispelem.texi")
             (delete-file "elispelem.info")))
         (replace 'build
           (lambda _
             ;; Generates both elispelem.texi & elispelem.info
             (invoke "emacs"
                     "--batch"
                     "--eval=(require 'ox-texinfo)"
                     "--eval=(find-file \"elispelem.org\")"
                     "--eval=(org-texinfo-export-to-info)")))
         (add-after 'build 'build-pdf
           (lambda _
             ;; PDF conversion
             (invoke "texi2any"
                     "--pdf"
                     "elispelem.texi")))
         (add-after 'build 'build-epub
           (lambda _
             (invoke "texi2any"
                     "--epub"
                     "elispelem.texi")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (info (string-append out "/share/info")))
               (mkdir-p out)
               (install-file "elispelem.pdf" out)
               (install-file "elispelem.epub" out)
               (install-file "elispelem.info" info)))))))
   (native-inputs
    (list emacs-minimal
          emacs-org
          grep
          sed
          texinfo-7
          (texlive-local-tree
           (list texlive-texinfo))
          texlive-collection-latexrecommended))
   (home-page "https://protesilaos.com/emacs/emacs-lisp-elements")
   (synopsis "Provides a big picture view of the Emacs Lisp programming language")
   (description
    "A book that provides a big picture view of the Emacs Lisp programming
language by combining prose with code.  The goal is to give readers an idea of
how Elisp works by showing some of the main concepts or patterns discernible in
everyday code.  The book is not meant to be a replacement for the built-in Emacs
Lisp Reference Manual.  It simply gives you enough information to reason about
Elisp.")
   (license license:fdl1.3+)))
