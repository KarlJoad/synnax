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
