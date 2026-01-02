(define-module (synnax packages datomic)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public datomic
  ;; stolen from https://codeberg.org/simendsjo/dotfiles.git
  (package
    (name "datomic")
    (version "1.0.7469")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://datomic-pro-downloads.s3.amazonaws.com/"
             version "/datomic-pro-" version ".zip"))
       (sha256
        (base32 "0bngfp8zh50h1p1idmbcvhnd7ja1s3xgc3p0ndydij44hh7dk4s9"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (invoke "unzip" #$source)
              (chdir (string-append "datomic-pro-" #$version "/"))))
          (add-after 'unpack 'patch-locations
            (lambda* (#:key inputs #:allow-other-keys)
              (patch-shebang "bin/repl-jline")
              (let ((java-bin (search-input-file inputs "bin/java")))
                (substitute* "bin/console"
                  (("/usr/bin/env java") java-bin))
                (substitute* "bin/rest"
                  (("/usr/bin/env java") java-bin))
                (substitute* "bin/repl-jline"
                  (("/usr/bin/env java") java-bin))
                (substitute* "bin/run"
                  (("/usr/bin/env java") java-bin))
                (substitute* "bin/shell"
                  (("java") java-bin))
                (substitute* "bin/transactor"
                  (("exec java") (string-append "exec " java-bin)))))))))
    (native-inputs
     (list unzip))
    (inputs
     (list openjdk))
    (home-page "https://datomic.com")
    (synopsis "The Datomic database")
    (description "The fully transactional, cloud-ready, distributed database.")
    ;; https://blog.datomic.com/2023/04/datomic-is-free.html
    (license license:asl2.0)))
