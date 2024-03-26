(define-module (synnax shells pdf-tools)
  #:use-module (guix profiles)
  #:use-module (gnu packages pdf))

(packages->manifest
 (list
  ;; Basic PDF tools, like pdffonts
  xpdf
  ;; Modifying PDFs
  stapler))
