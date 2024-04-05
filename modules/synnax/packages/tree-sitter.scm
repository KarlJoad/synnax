(define-module (synnax packages tree-sitter)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages tree-sitter))

(define-public tree-sitter-tlaplus
  ((@@ (gnu packages tree-sitter) tree-sitter-grammar)
   "tlaplus" "TLA+"
   "1k60dnzafj6m9c2d4xnwiz3d7yw3bg3iwx7c1anhwr76iyxdci3w"
   "1.0.8"
   ;; Version 1.2.1 is most recent, but requires tree-sitter >0.21.0
   #:repository-url "https://github.com/tlaplus-community/tree-sitter-tlaplus"))

(define-public tree-sitter-latex
  ((@@ (gnu packages tree-sitter) tree-sitter-grammar)
   "latex" "LaTeX"
   "0lc42x604f04x3kkp88vyqa5dx90wqyisiwl7nn861lyxl6phjnf"
   "0.3.0"
   #:repository-url "https://github.com/latex-lsp/tree-sitter-latex"))

(define-public tree-sitter-nix
  ((@@ (gnu packages tree-sitter) tree-sitter-grammar)
   "nix" "Nix"
   "0nn3ij8k6wkbf3kcvkyyp0vhfjcksi31wyyfwmsbx66maf2xgaii"
   "0.0.0"
   ;; The most recent commit at time of packaging, no tags.
   #:commit "763168fa916a333a459434f1424b5d30645f015d"
   #:repository-url "https://github.com/nix-community/tree-sitter-nix"))
