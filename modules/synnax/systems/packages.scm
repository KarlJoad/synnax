(define-module (synnax systems packages)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (nongnu packages mozilla)
  #:use-module (nongnu packages messaging) ;; zoom, element-desktop
  #:use-module (nongnu packages emacs) ;; clhs
  #:use-module (nongnu packages productivity) ;; zotero
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
 base             ; glibc, locales, etc.
 gnuzilla ; Icecat
 web-browsers ; Nyxt
 gstreamer
 emacs emacs-xyz
 tree-sitter
 vim
 agda
 admin
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
 texinfo
 maths          ; GNUPlot
 gcc            ; GNU C Manual
 python-xyz     ; python-pygments
 aspell         ; ispell
 kde            ; okular
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
        tree-sitter-awk
        tree-sitter-bash
        tree-sitter-bibtex
        tree-sitter-blueprint
        tree-sitter-c
        tree-sitter-cli
        tree-sitter-clojure
        tree-sitter-cmake
        tree-sitter-cpp
        tree-sitter-c-sharp
        tree-sitter-css
        tree-sitter-devicetree
        tree-sitter-dockerfile
        tree-sitter-dot
        tree-sitter-elixir
        tree-sitter-elm
        tree-sitter-embedded-template
        tree-sitter-erlang
        tree-sitter-go
        tree-sitter-gomod
        tree-sitter-haskell
        tree-sitter-hcl
        tree-sitter-heex
        tree-sitter-html
        tree-sitter-janet
        tree-sitter-java
        tree-sitter-javascript
        tree-sitter-json
        tree-sitter-julia
        tree-sitter-kdl
        tree-sitter-kotlin
        tree-sitter-latex
        tree-sitter-lua
        tree-sitter-markdown
        tree-sitter-matlab
        tree-sitter-mermaid
        tree-sitter-meson
        tree-sitter-nix
        tree-sitter-ocaml
        tree-sitter-org
        tree-sitter-php
        tree-sitter-plantuml
        tree-sitter-prisma
        tree-sitter-python
        tree-sitter-qml
        tree-sitter-query
        tree-sitter-r
        tree-sitter-racket
        tree-sitter-ron
        tree-sitter-ruby
        tree-sitter-rust
        tree-sitter-scala
        tree-sitter-scheme
        tree-sitter-starlark
        tree-sitter-sway
        tree-sitter-tlaplus
        tree-sitter-toml
        tree-sitter-typescript
        tree-sitter-typst
        tree-sitter-ungrammar
        tree-sitter-verilog
        tree-sitter-vhdl
        tree-sitter-vim
        tree-sitter-vimdoc
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
        zotero
        gnuplot
        okular
        sioyek
        ispell
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
