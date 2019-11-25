(require 'myext-auctex-base) ; myext-auctex-base--if-regexp

(defface myext-auctex-face-begin-document-line-face
  `((t
     :background ,mytheme-section-background-0
     ))
  "Font Lock mode face for \\begin{frame}.")
(defvar myext-auctex-face-begin-document-line-face 'myext-auctex-face-begin-document-line-face
  "face")

(defface myext-auctex-face-begin-frame-line-face
  `((t
     :background ,mytheme-section-background-2
     ))
  "Font Lock mode face for \\begin{frame}.")
(defvar myext-auctex-face-begin-frame-line-face 'myext-auctex-face-begin-frame-line-face
  "face")

(defface myext-auctex-face-end-frame-line-face
  `((t
     :background ,mytheme-section-background-2
     ))
  "Font Lock mode face for \\end{frame}.")
(defvar myext-auctex-face-end-frame-line-face 'myext-auctex-face-end-frame-line-face
  "face")

(defface myext-auctex-face-part-line-face
  `((t
     :background ,mytheme-section-background-0
     ))
  "Font Lock mode face for \\part{...}.")
(defvar myext-auctex-face-part-line-face 'myext-auctex-face-part-line-face
  "face")

(defface myext-auctex-face-section-line-face
  `((t
     :background ,mytheme-section-background-1
     ))
  "Font Lock mode face for \\section{...}.")
(defvar myext-auctex-face-section-line-face 'myext-auctex-face-section-line-face
  "face")

(defface myext-auctex-face-subsection-line-face
  `((t
     :background ,mytheme-section-background-2
     ))
  "Font Lock mode face for \\section{...}.")
(defvar myext-auctex-face-subsection-line-face 'myext-auctex-face-subsection-line-face
  "face")

(defface myext-auctex-face-if-face
  `((t
     :foreground ,mytheme-keyword-foreground-2
     ;; :inherit font-lock-semi-keyword-face
     ))
  "Font Lock mode face for \\if, \\ifhoge, \\else, \fi")
(defvar myext-auctex-face-if-face 'myext-auctex-face-if-face
  "face")

;;; colorize greek characters
(defvar myext-auctex-face-greek-lower
  '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "pi" "rho" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega"))
(defvar myext-auctex-face-greek-capital
  '("Gamma" "Delta" "Theta" "Lambda" "Xi" "Pi" "Sigma" "Upsilon" "Phi" "Psi" "Omega"))
(defvar myext-auctex-face-greek-var
  '("varepsilon" "vartheta" "varrho" "varphi"))
(defvar myext-auctex-face-greek
  (append myext-auctex-face-greek-lower
          myext-auctex-face-greek-capital
          myext-auctex-face-greek-var))
(defvar myext-auctex-face-greek-regexp
  (rx "\\"
      (eval (cons 'or myext-auctex-face-greek))
      word-boundary))
(defface myext-auctex-face-greek-face
  `((t :foreground ,mytheme-keyword-foreground-3 ))
  "Font Lock mode face for greek characters")
(defvar myext-auctex-face-greek-face 'myext-auctex-face-greek-face
  "face")

;;; colorize commands in tikz
;;; ↓myext-auctex-indent-env--tikz-commands と重複してる
(defvar myext-auctex-face-tikz-keyword
  '("path" "draw" "coordinate" "clip" "node" "pic"))
(defvar myext-auctex-face-tikz-keyword-regexp
  (rx "\\"
      (eval (cons 'or myext-auctex-face-tikz-keyword))
      word-boundary))
(defface myext-auctex-face-tikz-keyword-face
  `((t :foreground ,mytheme-keyword-foreground-4))
  "Font Lock mode face for greek characters")
(defvar myext-auctex-face-tikz-keyword-face 'myext-auctex-face-tikz-keyword-face
  "face")
;;; colorize points in tikz
(defvar myext-auctex-face-tikz-point
  '("p" "x" "y"))
;;; 下の2つの regexp をあわせて (or ...) にすると何故か動かない
(defvar myext-auctex-face-tikz-numeric-point-regexp
  (rx (group "\\"
             (eval (cons 'or myext-auctex-face-tikz-point)))
      (group numeric)))
(defvar myext-auctex-face-tikz-alpha-point-regexp
  (rx (group "\\"
             (eval (cons 'or myext-auctex-face-tikz-point)))
      "{"
      (group (1+ alphanumeric))
      "}"))
(defface myext-auctex-face-tikz-point-face
  `((t :foreground ,mytheme-keyword-foreground-5))
  "Font Lock mode face for greek characters")
(defvar myext-auctex-face-tikz-point-face 'myext-auctex-face-tikz-point-face
  "face")
(defface myext-auctex-face-tikz-point-name-face
  '((t :inherit font-lock-variable-name-face))
  "Font Lock mode face for greek characters")
(defvar myext-auctex-face-tikz-point-name-face 'myext-auctex-face-tikz-point-name-face
  "face")
;;; colorize commands in tikzcd
(defvar myext-auctex-face-tikzcd-keyword
  '("arrow" "ar"))
(defvar myext-auctex-face-tikzcd-keyword-regexp
  (rx "\\"
      (eval (cons 'or myext-auctex-face-tikzcd-keyword))
      word-boundary))
(defface myext-auctex-face-tikzcd-keyword-face
  `((t :foreground ,mytheme-keyword-foreground-4))
  "Font Lock mode face for greek characters")
(defvar myext-auctex-face-tikzcd-keyword-face 'myext-auctex-face-tikzcd-keyword-face
  "face")



(font-lock-add-keywords
 'latex-mode
 `(("\\\\begin{document}.*\n" . myext-auctex-face-begin-document-line-face)
   ("\\\\begin{frame}.*\n" . myext-auctex-face-begin-frame-line-face)
   ("\\\\end{frame}" . myext-auctex-face-end-frame-line-face)
   ("\\\\part\\*?{.*}.*\n" . (0 myext-auctex-face-part-line-face t))
   ("\\\\section\\*?{.*}.*\n" . (0 myext-auctex-face-section-line-face t))
   ("\\\\subsection\\*?{.*}.*" . (0 myext-auctex-face-subsection-line-face t))
   ;; \section{hoge $a=b$} のような場合にも face をつけるために，OVERRIDE を t にした
   ;; $...$ は "syntax highlighting" なので優先度が高いらしい？
   ;;   https://emacs.stackexchange.com/questions/19182/how-to-highlight-specific-keywords-inside-strings-quotes/19191
   ;; 設定方法の詳細などは font-lock-keywords の document を参照
   ("\\(^\\|[^\\]\\)\\(\\\\&\\)" 2 'font-latex-warning-face)
   ;; ↑font-latex.el からパクってきた (元は \\\\& じゃなくて &+ だった)
   (,(concat "\\\\" (eval myext-auctex-base--if-regexp)) . myext-auctex-face-if-face)
   (,(rx "\\" (or "else" "fi") word-boundary) . myext-auctex-face-if-face)
   (,(rx (or "\\makeatletter" "\\makeatother") word-boundary) . font-lock-semi-keyword-face)
   (,myext-auctex-face-greek-regexp . myext-auctex-face-greek-face)
   (,myext-auctex-face-tikz-keyword-regexp . myext-auctex-face-tikz-keyword-face)
   (,myext-auctex-face-tikz-numeric-point-regexp (1 myext-auctex-face-tikz-point-face)
                                                 (2 myext-auctex-face-tikz-point-name-face))
   (,myext-auctex-face-tikz-alpha-point-regexp (1 myext-auctex-face-tikz-point-face)
                                               (2 myext-auctex-face-tikz-point-name-face))
   (,myext-auctex-face-tikzcd-keyword-regexp . myext-auctex-face-tikzcd-keyword-face)))

(provide 'myext-auctex-face)
