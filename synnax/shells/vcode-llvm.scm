(define-module (synnax shells vcode-llvm)
  #:use-module (guix profiles)
  #:use-module (gnu packages file)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages ncurses)
  #:use-module (synnax packages go-xyz))

(packages->manifest
 (list go
       go-github-com-sri-csl-gllvm
       file
       ncurses/tinfo
       libxml2 libxml++
       llvm-12
       clang-toolchain-12
       linux-libre-headers))
