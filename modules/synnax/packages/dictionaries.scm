(define-module (synnax packages dictionaries)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages)
  #:use-module (gnu packages dictionaries)
  #:use-module (synnax packages))

(define-public dico-xdg
  (package
    (inherit
     (parameterize ((%patch-path (append %synnax-patch-path (%patch-path))))
       (package-with-extra-patches dico
                                   (search-patches "dico-search-XDG_CONFIG_HOME.patch"))))
    (name "dico-xdg")))

(define-public dict-moby-thesaurus
  (let ((commit "e12f3f30f78bf176a7f168cb1038f7bb124a684d")
        (revision "0"))
    (package
      (name "dict-moby-thesaurus")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/ferdnyc/dictd-dicts/")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "00raxdx6hl0dawhclzvvb1xy3j7g5j509l6ap3m3xpx5wz23585m"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan
         ;; The repo comes with many dictionaries. We only want the
         ;; moby-thesaurus for this package.
         '(("." "share/dictd/" #:include-regexp ("moby-thesaurus.*")))))
      (synopsis "Largest and most comprehensive thesaurus for dictd/dico")
      (description
       "\"Moby Thesaurus List by Grady Ward\" is a comprehensive thesaurus
published in the early 21st century.  This reference book serves as a valuable
resource for writers, students, and anyone in need of an extensive collection of
synonyms and related terms in the English language.  It includes a significant
number of words and is designed to aid in improving vocabulary and enhancing
language skills.")
      (home-page "https://www.gutenberg.org/ebooks/3202")
      (license license:public-domain))))
