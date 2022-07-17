(use-modules (guix packages)
             (guix download)
             (guix build-system python)
             (gnu packages python-xyz)
             ((guix licenses) #:prefix license:))

(package
  (name "python-qmk")
  (version "1.1.0")
  (source (origin
            (method url-fetch)
            (uri (pypi-uri "qmk" version))
            (sha256
             (base32
              "1qd5rnl0v97d0qmfbp32a193lw90njp3dawvjq4gxdwfwv4pf5bp"))))
  (build-system python-build-system)
  (propagated-inputs (list python-hid
                           python-hjson
                           python-jsonschema
                           python-milc
                           python-pillow
                           python-pygments
                           python-pyusb
                           python-qmk-dotty-dict
                           python-setuptools))
  (home-page "")
  (synopsis "A program to help users work with QMK Firmware.")
  (description
   "This package provides a program to help users work with QMK Firmware.")
  (license license:expat))
