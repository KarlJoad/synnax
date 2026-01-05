(define-module (synnax systems smalltalk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages smalltalk)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public glamorous-toolkit
  (package
    (name "glamorous-toolkit")
    (version "1.1.84")
    ;; FIXME: Swap source from gtoolkit that ships with all runtime libraries to
    ;; the image-without-world variant? This way Guix provides most of the
    ;; libraries we need, instead of getting them from gtoolkit.
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/feenkcom/gtoolkit/releases/download/"
                           "v" version
                           "/GlamorousToolkit-Linux-x86_64-v" version ".zip"))
       (sha256
        (base32 "0s1y94m6xylp58f3ccskkjc22njaxnxa13rh7n8p9xldyxaa0ihg"))))
    (build-system copy-build-system)
    (arguments
     (list
      ;; Pharo Smalltalk and Glamorous Toolkit are image-based, like Common Lisp,
      ;; and ship extra libraries too.
      ;; This means we need to install everything
      #:install-plan
      #~'(("." "."))
      #:phases
      #~(modify-phases %standard-phases
          ;; The zip file is flat, so when unzip unzips the zip file, the zip's
          ;; contents end up in CWD. This does not fit in with how Guix works
          ;; with sources in building, nor the copy-build-system's
          ;; #:install-plan.
          (add-before 'unpack 'create-unpack-dir
            (lambda _
              (mkdir-p "source")
              (chdir "source")))
          ;; For some reason, we end up in /tmp/guix-build-.../source/bin after
          ;; unpacking. Return to the unpacked root (source/) for rest of build.
          (add-after 'unpack 'return-to-root
            (lambda _
              (chdir "..")))
          ;; TODO: Add libgcc_s.so.1!
          (add-after 'patch-generated-file-shebangs 'fix-linker
            (lambda* (#:key inputs #:allow-other-keys)
              (format #t "Fixing dynamic linker~%")
              (with-directory-excursion "bin"
                (invoke "patchelf"
                        "--set-interpreter"
                        (search-input-file inputs #$(glibc-dynamic-linker))
                        ;; "--set-rpath"
                        ;; (search-input-file inputs "lib/libgcc_s.so.1")
                        "GlamorousToolkit")
                (invoke "patchelf"
                        "--set-interpreter"
                        (search-input-file inputs #$(glibc-dynamic-linker))
                        ;; "--set-rpath"
                        ;; (search-input-file inputs "lib/libgcc_s.so.1")
                        "GlamorousToolkit-cli"))))
          ;; I don't want to have to do GlamorousToolkit --image GlamorousToolkit.image
          ;; everytime I want to start this thing. So We make a little wrapper
          ;; to do it for us.
          (add-before 'install 'create-wrapper
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (format #t "Creating \"wrapper\" script for GlamorousToolkit binary so --image is not required.~%")
              (define (gtoolkit-image-wrapper bin)
                "Wrapper script for BIN to pass the image file from the store."
                (let ((out (assoc-ref outputs "out"))
                      (bin-hidden-name (format #f ".~a-real" bin))
                      (libs (list
                             ;; gtoolkit ships with its own Pharo engine image dump,
                             ;; which is required for some of the reflection operations
                             ;; gtoolkit does on itself.
                             (string-append (assoc-ref outputs "out") "/lib")
                             (string-append (assoc-ref inputs "pharo-vm") "/lib")
                             ;; XXX: gcc has multiple outputs. Using assoc-ref this way
                             ;; to find the lib output only works because the lib output
                             ;; is earlier in the inputs alist!
                             (string-append (assoc-ref inputs "cairo") "/lib")
                             (string-append (assoc-ref inputs "dbus") "/lib")
                             (string-append (assoc-ref inputs "freetype") "/lib")
                             (string-append (assoc-ref inputs "fontconfig-minimal") "/lib")
                             (string-append (assoc-ref inputs "gcc") "/lib")
                             (string-append (assoc-ref inputs "glib") "/lib")
                             (string-append (assoc-ref inputs "gobject-introspection") "/lib")
                             (string-append (assoc-ref inputs "gtk+") "/lib")
                             (string-append (assoc-ref inputs "harfbuzz") "/lib")
                             (string-append (assoc-ref inputs "libglvnd") "/lib")
                             (string-append (assoc-ref inputs "libx11") "/lib")
                             (string-append (assoc-ref inputs "libxcb") "/lib")
                             (string-append (assoc-ref inputs "libxext") "/lib")
                             (string-append (assoc-ref inputs "libxi") "/lib")
                             (string-append (assoc-ref inputs "libxrandr") "/lib")
                             (string-append (assoc-ref inputs "libxrender") "/lib")
                             (string-append (assoc-ref inputs "libxkbcommon") "/lib")
                             (string-append (assoc-ref inputs "libsoup") "/lib")
                             (string-append (assoc-ref inputs "skia") "/lib")
                             (string-append (assoc-ref inputs "util-linux") "/lib")
                             (string-append (assoc-ref inputs "webkitgtk-for-gtk3") "/lib"))))
                  (format #t "Hiding ~a as ~a~%" bin bin-hidden-name)
                  (copy-file bin bin-hidden-name)
                  (call-with-output-file bin
                    (lambda (port)
                      (display (string-append "#!" (which "bash") "\n") port)
                      ;; No need to detect the script dir. We know where out is
                      ;; at build-time, before the script exists, let alone is
                      ;; run.
                      ;; FIXME: This if is just gross
                      (if (string=? bin "GlamorousToolkit-cli")
                          (display
                           (string-join `("exec"
                                          ,(string-append "\"" out "/bin/" bin-hidden-name "\"")
                                          ,(string-append out "/GlamorousToolkit.image")))
                           ;; (string-append "exec \"" out "/bin/" bin-hidden-name "\""
                           ;;                ;; FIXME: Allow arguments to be passed through?
                           ;;                " " out "/GlamorousToolkit.image\"")
                           port)
                          (display
                           (string-append "exec \"" out "/bin/" bin-hidden-name
                                          "\" --image \"" out "/GlamorousToolkit.image\"")
                           port))
                      (chmod port #o755)))
                  ;; bin is now the script we just created, NOT the original
                  ;; binary!
                  (format #t "Wrapping ~a~%" bin)
                  (wrap-program bin
                    `("LD_LIBRARY_PATH" ":" = ,libs))
                  (chmod bin #o555)))
              (with-directory-excursion "bin"
                ;; FIXME: We must wrap all the *.so-s in out/lib too?
                (gtoolkit-image-wrapper "GlamorousToolkit")
                (gtoolkit-image-wrapper "GlamorousToolkit-cli"))))

          (add-after 'install 'create-desktop-file
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((icons-dir (string-append #$output "/share/icons/hicolor/scalable/apps")))
                (mkdir-p icons-dir)
                (copy-file (assoc-ref inputs "gtoolkit.svg")
                           (string-append icons-dir "/GlamorousToolkit.svg")))
              (make-desktop-entry-file
               (string-append #$output "/share/applications/glamoroustoolkit.destop")
               #:name "Glamorous Toolkit"
               #:type "Application"
               ;; NOTE: This GlamorousToolkit MUST be our wrapped version!
               #:exec (string-append #$output "/bin/GlamorousToolkit")
               #:icon "GlamorousToolkit.svg"
               #:keywords '()
               #:categories '("Application" "Development")
               #:terminal #f
               #:startup-notify #t
               #:startup-w-m-class "glamorous-toolkit"
               ;; TODO: Does gtoolkit have a MIME type handler setup?
               ;; #:mime-type "x-scheme-handler/???"
               #:comment
               '(("en" "Moldable Development Environment")
                 (#f "Moldable Development Environment")))))
          ;; Create some symlinked aliases for GlamorousToolkit.
          (add-after 'install 'create-aliases
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (symlink (string-append out "/bin/GlamorousToolkit")
                         (string-append out "/bin/gtoolkit")))))
          ;; FIXME: Bring validate-runpath phase back!
          (delete 'validate-runpath))))
    (native-inputs
     (list unzip
           ;; The SVG icon
           (origin
             (method url-fetch)
             ;; FIXME: Where the hell did this URI come from? I copied it from
             ;; Nixpkgs.
             (uri "https://gist.githubusercontent.com/qbit/cb52e6cd193c410e0b0aee8a216f6574/raw/2b042bde1dc4cbd30457f14c9d18c889444bf3d0/glamoroustoolkit.svg")
             (file-name "gtoolkit.svg")
             (sha256
              (base32
               "02wjh6qkw466wgh91fdjn5sa5k3yi05czi94yzm74simzpqfidsf")))
           patchelf))
    ;; TODO: Do we actually need pharo-vm?
    ;; The GlamorousToolkit.image image might already have it inside the image.
    (inputs
     (list pharo-vm
           cairo
           dbus
           freetype
           fontconfig
           harfbuzz
           `(,gcc "lib")
           glib
           gobject-introspection
           gtk+
           libglvnd
           libsoup
           libx11
           libxcb
           libxext
           libxi
           libxrandr
           libxrender
           libxkbcommon
           skia
           `(,util-linux "lib") ; libuuid
           webkitgtk-for-gtk3))
    (propagated-inputs
     (list))
    (home-page "https://gtoolkit.com/")
    (synopsis "A Moldable Development Environment that helps make systems explainable with contextual micro tools")
    (description "")
    (license license:expat)))

glamorous-toolkit
