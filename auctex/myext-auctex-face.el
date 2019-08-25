(require 'myext-auctex-base) ; LaTeX-if-regexp

(defface font-lock-latex-begin-document-line-face
  `((t
     :background ,mytheme-section-background-0
     ))
  "Font Lock mode face for \\begin{frame}.")
(defvar font-lock-latex-begin-document-line-face 'font-lock-latex-begin-document-line-face
  "face")

(defface font-lock-latex-begin-frame-line-face
  `((t
     :background ,mytheme-section-background-2
     ))
  "Font Lock mode face for \\begin{frame}.")
(defvar font-lock-latex-begin-frame-line-face 'font-lock-latex-begin-frame-line-face
  "face")

(defface font-lock-latex-end-frame-line-face
  `((t
     :background ,mytheme-section-background-2
     ))
  "Font Lock mode face for \\end{frame}.")
(defvar font-lock-latex-end-frame-line-face 'font-lock-latex-end-frame-line-face
  "face")

(defface font-lock-latex-part-line-face
  `((t
     :background ,mytheme-section-background-0
     ))
  "Font Lock mode face for \\part{...}.")
(defvar font-lock-latex-part-line-face 'font-lock-latex-part-line-face
  "face")

(defface font-lock-latex-section-line-face
  `((t
     :background ,mytheme-section-background-1
     ))
  "Font Lock mode face for \\section{...}.")
(defvar font-lock-latex-section-line-face 'font-lock-latex-section-line-face
  "face")

(defface font-lock-latex-subsection-line-face
  `((t
     :background ,mytheme-section-background-2
     ))
  "Font Lock mode face for \\section{...}.")
(defvar font-lock-latex-subsection-line-face 'font-lock-latex-subsection-line-face
  "face")

(defface font-lock-latex-if-face
  `((t
     :foreground ,mytheme-keyword-foreground-2
     ;; :inherit font-lock-semi-keyword-face
     ))
  "Font Lock mode face for \\if, \\ifhoge, \\else, \fi")
(defvar font-lock-latex-if-face 'font-lock-latex-if-face
  "face")

;;; colorize greek characters
(defvar my-LaTeX-greek-lower
  '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "pi" "rho" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega"))
(defvar my-LaTeX-greek-capital
  '("Gamma" "Delta" "Theta" "Lambda" "Xi" "Pi" "Sigma" "Upsilon" "Phi" "Psi" "Omega"))
(defvar my-LaTeX-greek-var
  '("varepsilon" "vartheta" "varrho" "varphi"))
(defvar my-LaTeX-greek
  (append my-LaTeX-greek-lower
          my-LaTeX-greek-capital
          my-LaTeX-greek-var))
(defvar my-LaTeX-greek-regexp
  (rx "\\"
      (eval (cons 'or my-LaTeX-greek))
      word-boundary))
(defface font-lock-latex-greek-face
  `((t :foreground ,mytheme-keyword-foreground-3 ))
  "Font Lock mode face for greek characters")
(defvar font-lock-latex-greek-face 'font-lock-latex-greek-face
  "face")

;;; colorize commands in tikz
(defvar my-LaTeX-tikz-keyword
  '("path" "draw" "coordinate"))
(defvar my-LaTeX-tikz-keyword-regexp
  (rx "\\"
      (eval (cons 'or my-LaTeX-tikz-keyword))
      word-boundary))
(defface font-lock-latex-tikz-keyword-face
  `((t :foreground ,mytheme-keyword-foreground-4))
  "Font Lock mode face for greek characters")
(defvar font-lock-latex-tikz-keyword-face 'font-lock-latex-tikz-keyword-face
  "face")
;;; colorize points in tikz
(defvar my-LaTeX-tikz-point
  '("p" "x" "y"))
;;; 下の2つの regexp をあわせて (or ...) にすると何故か動かない
(defvar my-LaTeX-tikz-numeric-point-regexp
  (rx (group "\\"
             (eval (cons 'or my-LaTeX-tikz-point)))
      (group numeric)))
(defvar my-LaTeX-tikz-alpha-point-regexp
  (rx (group "\\"
             (eval (cons 'or my-LaTeX-tikz-point)))
      "{"
      (group (1+ alphanumeric))
      "}"))
(defface font-lock-latex-tikz-point-face
  `((t :foreground ,mytheme-keyword-foreground-5))
  "Font Lock mode face for greek characters")
(defvar font-lock-latex-tikz-point-face 'font-lock-latex-tikz-point-face
  "face")
(defface font-lock-latex-tikz-point-name-face
  '((t :inherit font-lock-variable-name-face))
  "Font Lock mode face for greek characters")
(defvar font-lock-latex-tikz-point-name-face 'font-lock-latex-tikz-point-name-face
  "face")
;;; colorize commands in tikzcd
(defvar my-LaTeX-tikzcd-keyword
  '("arrow" "ar"))
(defvar my-LaTeX-tikzcd-keyword-regexp
  (rx "\\"
      (eval (cons 'or my-LaTeX-tikzcd-keyword))
      word-boundary))
(defface font-lock-latex-tikzcd-keyword-face
  `((t :foreground ,mytheme-keyword-foreground-4))
  "Font Lock mode face for greek characters")
(defvar font-lock-latex-tikzcd-keyword-face 'font-lock-latex-tikzcd-keyword-face
  "face")



(font-lock-add-keywords 'latex-mode
                        `(("\\\\begin{document}.*\n" . font-lock-latex-begin-document-line-face)
                          ("\\\\begin{frame}.*\n" . font-lock-latex-begin-frame-line-face)
                          ("\\\\end{frame}" . font-lock-latex-end-frame-line-face)
                          ("\\\\part\\*?{.*}.*\n" . (0 font-lock-latex-part-line-face t))
                          ("\\\\section\\*?{.*}.*\n" . (0 font-lock-latex-section-line-face t))
                          ("\\\\subsection\\*?{.*}.*" . (0 font-lock-latex-subsection-line-face t))
                          ;; \section{hoge $a=b$} のような場合にも face をつけるために，OVERRIDE を t にした
                          ;; $...$ は "syntax highlighting" なので優先度が高いらしい？
                          ;;   https://emacs.stackexchange.com/questions/19182/how-to-highlight-specific-keywords-inside-strings-quotes/19191
                          ;; 設定方法の詳細などは font-lock-keywords の document を参照
                          ("\\(^\\|[^\\]\\)\\(\\\\&\\)" 2 'font-latex-warning-face)
                          ;; ↑font-latex.el からパクってきた (元は \\\\& じゃなくて &+ だった)
                          (,(concat "\\\\" (eval LaTeX-if-regexp)) . font-lock-latex-if-face)
                          (,(rx "\\" (or "else" "fi") word-boundary) . font-lock-latex-if-face)
                          (,(rx (or "\\makeatletter" "\\makeatother") word-boundary) . font-lock-semi-keyword-face)
                          (,my-LaTeX-greek-regexp . font-lock-latex-greek-face)
                          (,my-LaTeX-tikz-keyword-regexp . font-lock-latex-tikz-keyword-face)
                          (,my-LaTeX-tikz-numeric-point-regexp (1 font-lock-latex-tikz-point-face)
                                                               (2 font-lock-latex-tikz-point-name-face))
                          (,my-LaTeX-tikz-alpha-point-regexp (1 font-lock-latex-tikz-point-face)
                                                             (2 font-lock-latex-tikz-point-name-face))
                          (,my-LaTeX-tikzcd-keyword-regexp . font-lock-latex-tikzcd-keyword-face)))

(provide 'myext-auctex-face)
