(define-module (synnax packages dictionaries)
  #:use-module (guix gexp)
  #:use-module (guix packages)
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
