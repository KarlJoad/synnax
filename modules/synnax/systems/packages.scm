(define-module (synnax systems packages)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (nongnu packages mozilla)
  #:use-module (nongnu packages messaging) ;; zoom, element-desktop
  #:use-module (nongnu packages emacs) ;; clhs
  #:use-module (nongnu packages productivity) ;; zotero
  #:use-module (synnax packages dictionaries)
  #:use-module (synnax packages scripts)
  #:export (%nonguix-packages
            %web-browser-packages
            %virtualization-packages
            %filesystems-packages
            %wm-packages
            %utils-packages
            %system-packages
            %base-home-packages
            %messaging-packages
            %keyboard-packages
            %documentation-packages
            %eda-packages
            %document-prep-packages
            %development-packages
            %emacs-metapackage
            %font-packages
            %home-packages
            %desktop-home-packages))

(use-package-modules
 base             ; glibc, locales, grep, sed, etc.
 gawk
 gnuzilla ; Icecat
 web-browsers ; Nyxt
 gstreamer
 emacs emacs-xyz
 tree-sitter
 vim
 agda
 admin
 databases ; recutils
 password-utils ; passwd
 mail ; mbsync & mu
 telegram
 xorg
 xdisorg          ; X11 helper programs
 wget curl
 version-control
 wm ; Stumpwm, dunst
 certs
 moreutils pciutils lsof
 dns            ; isc-bind (host, dig, etc.)
 networking     ; ethtools
 disk             ; dosfstools (mkfs.fat)
 cryptsetup samba
 freedesktop      ; XDG stuff, xdg-desktop-portal
 linux pulseaudio ; ALSA/PulseAudio
 compression
 terminals      ; alacritty, urxvt
 virtualization ; virt-manager
 gdb
 code           ; global
 shellutils
 engineering    ; lepton-eda, poke, radare2
 text-editors   ; texmacs
 cmake
 tex            ; texlive packages, biber
 ncurses
 texinfo
 maths          ; GNUPlot
 gcc            ; GNU C Manual
 python-xyz     ; python-pygments
 aspell
 kde-graphics   ; okular
 pdf
 flashing-tools
 gnome          ; yelp (documentation generator)
 libreoffice
 fonts
 package-management ; stow, nix
 gnucash
 video          ; vlc
 image-viewers
 ebook          ; calibre
 lisp lisp-xyz
 perl
 rsync
 vpn
 cups
 web
 screen)

(define %nonguix-packages (list firefox zoom element-desktop))

(define %web-browser-packages
  (list icecat
        ;; Nyxt & associated packages
        nyxt xclip xsel gst-plugins-base gst-plugins-good))

(define %virtualization-packages
  (list libvirt virt-manager))

(define %filesystems-packages
  (list squashfs-tools dosfstools
        cryptsetup
        cifs-utils
        usbutils))

(define %wm-packages
  (list stumpwm+slynk xsetroot dunst))

(define %utils-packages
  (list coreutils moreutils pciutils lsof hwdata binutils
        grep sed gawk
        procps ; ps, top, & co.
        (list isc-bind "utils")
        inetutils
        ethtool
        util-linux))

(define %system-packages
  (append
   (list pwgen
         glibc-locales
         xdg-utils
         xdg-desktop-portal
         xdg-desktop-portal-wlr
         xautolock ; Run command for user after some time has passed with no input
         alsa-utils pavucontrol
         vim
         ;; emacs emacs-guix
         wget curl
         zip unzip
         nix
         rxvt-unicode alacritty
         cups)
   %utils-packages
   %wm-packages
   %web-browser-packages
   %virtualization-packages))


;; Find the tree-sitter-* grammars with this:
;; guix search tree-sitter-.+ | recsel -p name | awk '{print $2}' | grep -e '^tree-sitter-*' | uniq | sort
(define %emacs-metapackage
  (list emacs-pgtk
        emacs-guix
        emacs-vterm
        emacs-agda2-mode
        tree-sitter
        tree-sitter-cli
        ;; tree-sitter grammars to install
        tree-sitter-ada
        tree-sitter-arduino
        tree-sitter-awk
        tree-sitter-bash
        tree-sitter-bibtex
        tree-sitter-bicep
        tree-sitter-blueprint
        tree-sitter-c
        tree-sitter-chatito
        tree-sitter-clarity
        tree-sitter-cli
        tree-sitter-clojure
        tree-sitter-cmake
        tree-sitter-comment
        tree-sitter-cpp
        tree-sitter-c-sharp
        tree-sitter-css
        tree-sitter-dart
        tree-sitter-devicetree
        tree-sitter-dockerfile
        tree-sitter-dot
        tree-sitter-doxygen
        tree-sitter-elixir
        tree-sitter-elm
        tree-sitter-embedded-template
        tree-sitter-erlang
        tree-sitter-firrtl
        tree-sitter-fortran
        tree-sitter-func
        tree-sitter-gdscript
        tree-sitter-gitignore
        tree-sitter-gleam
        tree-sitter-gn
        tree-sitter-go
        tree-sitter-gomod
        tree-sitter-gosum
        tree-sitter-gpr
        tree-sitter-groovy
        tree-sitter-gstlaunch
        tree-sitter-hack
        tree-sitter-haskell
        tree-sitter-hcl
        tree-sitter-heex
        tree-sitter-html
        tree-sitter-hyprlang
        tree-sitter-ini
        tree-sitter-janet
        tree-sitter-java
        tree-sitter-java-properties
        tree-sitter-javascript
        tree-sitter-jsdoc
        tree-sitter-json
        tree-sitter-jsonnet
        tree-sitter-julia
        tree-sitter-kconfig
        tree-sitter-kdl
        tree-sitter-kotlin
        tree-sitter-latex
        tree-sitter-linkerscript
        tree-sitter-lua
        tree-sitter-luadoc
        tree-sitter-lua-patterns
        tree-sitter-magik
        tree-sitter-make
        tree-sitter-markdown
        tree-sitter-matlab
        tree-sitter-mermaid
        tree-sitter-meson
        tree-sitter-netlinx
        tree-sitter-nim
        tree-sitter-ninja
        tree-sitter-nix
        tree-sitter-ocaml
        tree-sitter-odin
        tree-sitter-org
        tree-sitter-pascal
        tree-sitter-pem
        tree-sitter-pgn
        tree-sitter-php
        tree-sitter-plantuml
        tree-sitter-po
        tree-sitter-powershell
        tree-sitter-printf
        tree-sitter-prisma
        tree-sitter-proto
        tree-sitter-puppet
        tree-sitter-purescript
        tree-sitter-python
        tree-sitter-python-manifest
        tree-sitter-python-requirements
        tree-sitter-qml
        tree-sitter-qmldir
        tree-sitter-query
        tree-sitter-r
        tree-sitter-racket
        tree-sitter-readline
        tree-sitter-rego
        tree-sitter-ron
        tree-sitter-rst
        tree-sitter-ruby
        tree-sitter-rust
        tree-sitter-scala
        tree-sitter-scheme
        tree-sitter-solidity
        tree-sitter-starlark
        tree-sitter-sway
        tree-sitter-tablegen
        tree-sitter-tcl
        tree-sitter-test
        tree-sitter-tlaplus
        tree-sitter-toml
        tree-sitter-twig
        tree-sitter-typescript
        tree-sitter-typst
        tree-sitter-udev
        tree-sitter-ungrammar
        tree-sitter-uxntal
        tree-sitter-verilog
        tree-sitter-vhdl
        tree-sitter-vim
        tree-sitter-vimdoc
        tree-sitter-xcompose
        tree-sitter-xml
        tree-sitter-yaml
        tree-sitter-zig))

(define %base-home-packages
  (append
   (list password-store ;; pass
         stow
         last-reconfigure-date
         nix-gc-roots
         geeqie ;; Lightweight GTK+ image-viewer
         rsync
         recutils ;; Text-based databases from the command-line
         openvpn)
   %emacs-metapackage))

(define %extra-guix-packages
  (list guix-icons ;; Guix icons & artwork
        guix-modules ;; Incremental development of guix-shell-like module environments
        gwl ;; Declarative workflow management system using Guile & Guix
        ))

(define %eda-packages (list lepton-eda kicad))

(define %document-prep-packages
  (list texmacs
        texlive-scheme-full ;; Include ALL of TeXLive, because I am lazy and disks are large
        texlive-biber ;; I prefer biber over old-school bibtex
        python-pygments ;; To make texlive-minted work
        ncurses ;; lualatex requires "tput", which ncurses provides
        zotero
        gnuplot
        okular
        sioyek
        aspell
        dico-xdg
        libreoffice))

(define %development-packages
  (list binutils ;; TODO: Only install the binutils info manuals to global home path?
        gnu-make ;; Include the make command by default
        cmake
        global   ;; Global & gtags
        poke
        gdb
        perl ;; For magit to work correctly (Rebase, commit-ammend, etc.)
        cscope
        git
        poke ;; Interactive editor for binary files
        radare2 ;; Reverse engineering tools (good for diff of assembly)
        direnv
        screen
        texinfo))

(define %mailing-list-packages
  (list l2md))

(define %messaging-packages
  (list telegram-desktop
        zulip-desktop))

(define avr-toolchain
  ((@ (gnu packages avr) make-avr-toolchain)))

(define %keyboard-packages
  (list avr-toolchain))

(define %documentation-packages
  (list clhs ;; Common Lisp HyperSpec, with Emacs mode
        gnu-c-manual ;; The GNU manual for their C implementation
        (list cmake "doc")))

(define %font-packages
  (list font-iosevka font-iosevka-slab font-iosevka-term font-iosevka-term-slab
        font-fira-mono font-fira-code
        ;; Add Google's noto font family (mostly for Emoji support)
        font-google-noto font-google-noto-emoji))

(define %scientific-packages
  (list octave
        gnuplot))

(define %home-packages
  (append
   %extra-guix-packages
   %scientific-packages
   %development-packages
   %documentation-packages
   %document-prep-packages
   %eda-packages
   %messaging-packages
   %font-packages
   %filesystems-packages
   %base-home-packages))

(define %desktop-home-packages
  (append
   (list vlc obs
         gnucash (list gnucash "doc") yelp
         calibre
         sbcl cl-asdf cl-slynk sbcl-slynk
         wally-cli)
   %mailing-list-packages))
