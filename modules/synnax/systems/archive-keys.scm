(define-module (synnax systems archive-keys)
  #:use-module (guix gexp))

(define-public %nonguix
  (plain-file "nonguix-archive-key.pub"
              "(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
  )
 )"))
