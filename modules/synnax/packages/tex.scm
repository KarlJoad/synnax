(define-module (synnax packages tex)
  #:use-module (ice-9 ftw)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  ;; #:use-module (guix build-system gnu)
  #:use-module (guix build-system texlive)
  #:use-module (gnu packages fontutils)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (texlive-inconsolata-lgc))

(define-public texlive-inconsolata-lgc
  (package
    (name "texlive-inconsolata-lgc")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/MihailJP/Inconsolata-LGC.git")
                    (commit (string-append "LGC-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wjs6vq7i6c4my759cpm629c21c5mf71qi0va5zlvp1b8scykg0c"))))
    (build-system texlive-build-system)
    (arguments
     `(#:modules ((guix build texlive-build-system)
                  (guix build utils)
                  (ice-9 ftw))
       #:phases
       (modify-phases %standard-phases
           (add-before 'build 'run-make
             (lambda* _
               (invoke "make")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (doc (assoc-ref outputs "doc")))
                 ;; Take care of documentation.
                 (when (directory-exists? "doc")
                   (unless doc
                     (format (current-error-port)
                             "warning: missing 'doc' output for package documentation~%"))
                   (let ((doc-dir (string-append (or doc out) "/share/texmf-dist/doc")))
                     (mkdir-p doc-dir)
                     (copy-recursively "doc" doc-dir)))
                 ;; Install runfiles.  The package may not contain any, though.  Create
                 ;; #$output anyway to handle this situation gracefully.
                 ;; #$out/share/texmf-dist/fonts/opentype/public/libertine
                 (mkdir-p out)
                 (define (install-font type font-extension)
                   (let ((destination (string-append out "/share/texmf-dist/fonts/"
                                                     type "/public/inconsolata-lgc/"))
                         (font-files (scandir "." (lambda (f)
                                                    (string-suffix? font-extension f)))))
                     (mkdir-p destination)
                     (for-each (lambda (f)
                                 (format #t "Copying ~a to ~a/~a~%"
                                         f destination f)
                                 (install-file f destination)) font-files)))
                 (install-font "opentype" ".otf")
                 (install-font "truetype" ".ttf")))))))
    (native-inputs
     (list fontforge))
    (home-page "https://github.com/MihailJP/Inconsolata-LG")
    (synopsis "Fork of Inconsolata font, with proper support of Cyrillic and Greek")
    (description
     "Inconsolata is one of the most suitable font for programmers created by Raph
Levien. Since the original Inconsolata does not contain Cyrillic alphabet,
it was slightly inconvenient for not a few programmers from Russia.

Inconsolata LGC is a modified version of Inconsolata with added the Cyrillic
alphabet which directly descends from Inconsolata Hellenic supporting modern
Greek.

Inconsolata LGC is licensed under SIL OFL.

Inconsolata LGC changes:
* Cyrillic glyphs added.
* Italic and Bold font added.

Changes inherited from Inconsolata Hellenic:
* Greek glyphs.

Changes inherited from Inconsolata-dz:
* Straight quotation marks.")
    (license license:silofl1.1)))

texlive-inconsolata-lgc
