(define-module (synnax packages plan9)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:))

(define-public plan9port
  (let ((commit "be7c68f6954f7dcaa53403e0f600716f65a13d32")
        (revision "0"))
    (package
      (name "plan9port")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/9fans/plan9port")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1imayp35cvizxism949s6iwwjawd356xm274ijlgfgry0xxi9fl6"))))
      (build-system gnu-build-system)
      (inputs
       (list bash-minimal
             which
             perl
             fontconfig
             freetype
             libx11
             libxext
             libxt
             xorgproto))
      (propagated-inputs
       (list gcc-toolchain))
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'bootstrap 'fix-install-script
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "INSTALL"
                  ;; Remove hard-coded bin directories
                  (("/bin:/usr/bin:") "")
                  ;; Remove hard-coded & absolute include dirs for X11
                  ;; But leave in the plan9 include/ directory.
                  ;; For some magical reason, cleanname requires this -Iinclude
                  ;; be present!
                  (("(-I[a-zA-Z0-9/]*[\n\t\\\\ ]*)+") "-Iinclude ")
                  ;; Rewrite calls to cc with calls to gcc
                  (("cc") (string-append (assoc-ref inputs "gcc") "/bin/gcc")))
                ;; Forcibly increase the number of cores
                (with-output-to-port (open-file "INSTALL" "a")
                  (lambda ()
                    (format #t "NPROC='~a'" (getenv "NIX_BUILD_CORES"))))
                #t))
            (add-before 'configure 'fix-which
              (lambda _
                (substitute* "bin/9c"
                  ;; Make which actually use the which in the store.
                  (("which uniq") #$(file-append which "/bin/which uniq")))))
            (add-before 'configure 'replace-egrep
              (lambda _
                ;; The files to fix were found by manually grep-ing upstream.
                (define files-to-fix
                  (list "bin/9l"
                        "bin/9c"
                        "dist/checkman.awk"
                        "lib/fortunes"
                        "man/man7/regexp.7"
                        "man/lookman.index"
                        "src/cmd/auxstats/mkfile"
                        "src/cmd/rc/checkparse"
                        "src/cmd/mkfile"
                        "src/cmd/devdraw/mkwsysrules.sh"
                        "src/mkfile"))
                (substitute* files-to-fix
                  (("egrep") "grep -E"))))
            (replace 'configure
              (lambda* (#:key inputs #:allow-other-keys)
                (setenv "C_INCLUDE_PATH" (string-append
                                          (string-append (assoc-ref inputs "freetype")
                                                         "/include/freetype2")
                                          ":"
                                          (getenv "C_INCLUDE_PATH")))
                ;; We forcibly set CC9 and X11 so they can be found later.
                (with-output-to-file "LOCAL.config"
                  (lambda ()
                    (format #t "CC9='~a'
X11='~a'~%"
                            (string-append (assoc-ref inputs "gcc") "/bin/gcc")
                            (string-append (assoc-ref inputs "libxt") "/include"))))))
            ;; Guix has a second patch-phase after the configure phase, for generated
            ;; files, while Nix does not. So, unlike Nix, we do not need to manually
            ;; call fixupPhase a second time.
            (replace 'build
              (lambda* (#:key outputs #:allow-other-keys)
                (setenv "PLAN9_TARGET" (string-append (assoc-ref outputs "out")
                                                      "/plan9"))
                (invoke "./INSTALL" "-b")))
            ;; There are no build-directory tests available.
            (delete 'check)
            ;; Unfortunately, installing this is done with the INSTALL script,
            ;; which is also used for building...
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  (mkdir-p out)
                  (mkdir-p (string-append out "/bin"))
                  (copy-recursively "." (string-append out "/plan9"))
                  (chdir (string-append out "/plan9"))
                  (invoke "./INSTALL" "-c")
                  ;; Symlink 9 to $out/bin/9 so we can figure out $PLAN9
                  (symlink (string-append out "/plan9/bin/9") (string-append out "/bin/9"))
                  #t)))
            (add-after 'install 'install-check
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out"))
                      (test-prog "test.c"))
                  (setenv "PLAN9" (string-append out "/plan9"))
                  (setenv "PATH" (string-append (string-append out "/bin")
                                                ":"
                                                (string-append (getenv "PLAN9") "/bin")
                                                ":"
                                                (getenv "PATH")))
                  ;; Check that the plan9 shell, rc, works.
                  (invoke (string-append out "/plan9/bin/9") "rc" "-c" "echo rc is working")
                  (invoke (string-append out "/bin/9") "rc" "-c" "echo symlinked rc is working")
                  ;; Go to $TMP, create a C file, and test that the plan9 C
                  ;; compiler works. (Checking env-vars and embedded paths).
                  (with-directory-excursion (getenv "TMP")
                    (with-output-to-file test-prog
                      (lambda ()
                        (format #t "
#include <u.h>
#include <libc.h>
#include <thread.h>
void
threadmain(int argc, char **argv)
{
    threadexitsall(nil);
}")))
                    (invoke (string-append out "/bin/9") "9c" "-o" "test.o" test-prog)
                    (invoke (string-append out "/bin/9") "9l" "-o" "test" "test.o")
                    (invoke "./test"))
                  #t))))))
      (home-page "https://9fans.github.io/plan9port/")
      (synopsis "A port of many Plan9 programs to other Unix-like operating systems")
      (description "Plan 9 from User Space (aka plan9port) is a port of many Plan 9
programs from their native Plan 9 environment to other Unix-like operating systems.
This includes the venerable acme editor with tools like plumber available too.")
      (license (list license:expat     ;modifications
                     license:lpl1.02   ;original Plan9 code
                     license:zlib))))) ;src/cmd/bzip2

plan9port
