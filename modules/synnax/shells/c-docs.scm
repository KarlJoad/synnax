(define-module (synnax shells c-docs)
  #:use-module (guix)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (gnu packages)
  #:export (c-docs-manifest))

(define c-docs-manifest
  (specifications->manifest
   (list "gcc-toolchain"
         "man-pages"
         "mandoc"
         "info-reader")))

c-docs-manifest
