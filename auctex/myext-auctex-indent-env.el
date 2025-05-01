;;; indent as the following (customizable):
;;; \begin{itemize}
;;;   \item hoge
;;;     fuga
;;;   \item piyo
;;; \end{itemize}
(defun myext-auctex-indent-env--indent-item--inner (regexp-environ item-cmd)
  "intersection of myext-auctex-indent-env--indent-item and myext-auctex-indent-env--indent-bibitem"
  (save-match-data
    (let* ((offset LaTeX-indent-level)
           (contin (or (and (boundp 'myext-auctex-indent-env--indent-level-item-continuation)
                            myext-auctex-indent-env--indent-level-item-continuation)
                       (* 2 LaTeX-indent-level)))
           (re-beg "\\\\begin{")
           (re-end "\\\\end{")
           (re-env regexp-environ)
           (indent (save-excursion
                     (when (looking-at (concat re-beg re-env "}"))
                       (end-of-line))
                     (LaTeX-find-matching-begin)
                     (current-column))))
      (cond ((looking-at (concat re-beg re-env "}"))
             (or (save-excursion
                   (beginning-of-line)
                   (ignore-errors
                     (LaTeX-find-matching-begin)
                     (+ (current-column)
                        (if (looking-at (concat re-beg re-env "}"))
                            contin
                          offset))))
                 indent))
            ((looking-at (concat re-end re-env "}"))
             indent)
            ((looking-at item-cmd)
             (+ offset indent))
            (t
             (+ contin indent))))))
(defvar myext-auctex-item-environment
  '("itemize" "enumerate" "description")
  "environment names which contain \\item")
(defun myext-auctex-indent-env--indent-item ()
  "Provide proper indentation for LaTeX \"itemize\",\"enumerate\", and
\"description\" environments.

  \"\\item\" is indented `LaTeX-indent-level' spaces relative to
  the the beginning of the environment.

  Continuation lines are indented either twice
  `LaTeX-indent-level', or `myext-auctex-indent-env--indent-level-item-continuation'
  if the latter is bound."
  (myext-auctex-indent-env--indent-item--inner
   (rx-to-string `(group (or ,@myext-auctex-item-environment)) t)
   "\\\\item\\>"))
(defun myext-auctex-indent-env--indent-bibitem ()
  "Provide proper indentation for LaTeX \"thebibliography\" environment.

  \"\\bibitem\" is indented `LaTeX-indent-level' spaces relative to
  the the beginning of the environment.

  Continuation lines are indented either twice
  `LaTeX-indent-level', or `myext-auctex-indent-env--indent-level-item-continuation'
  if the latter is bound."
  (myext-auctex-indent-env--indent-item--inner "\\(thebibliography\\)"
                                               "\\\\bibitem"))

(defcustom myext-auctex-indent-env--indent-level-item-continuation 4
  "*Indentation of continuation lines for items in itemize-like
environments."
  :group 'LaTeX-indentation
  :type 'integer)

;;; indent in tikzpicture
;;; ↓myext-auctex-face-tikz-keyword と重複してる
(defvar myext-auctex-indent-env--tikz-commands
  '("path" "draw" "coordinate" "clip" "node" "pic" "useasboundingbox")
  "commands which begin tikz commands (e.g. \\path, \\draw)")
(defun myext-auctex-indent-env--goto-previous-nontriv-line ()
  "go to (the beginning of) the previous nontrivial line"
  (forward-line -1)
  (back-to-indentation)
  (while (looking-at (rx (or line-end "%")))
    (forward-line -1)
    (back-to-indentation)))
(defun myext-auctex-indent-env--indent-tikzpicture ()
  "indent in tikzpicture"
  (save-excursion
    (let ((indent-increase 0)
          prev-indent)
      ;; current line starts with \end
      (when (looking-at (rx "\\end"))
        (setq indent-increase (- indent-increase
                                 LaTeX-indent-level)))
      (myext-auctex-indent-env--goto-previous-nontriv-line)
      (setq prev-indent (current-column))
      ;; read previous line
      (when (looking-at (rx "\\begin"))
        (setq indent-increase (+ indent-increase
                                 LaTeX-indent-level)))
      (when (looking-at (eval `(rx "\\" (or ,@myext-auctex-indent-env--tikz-commands))))
        (setq indent-increase (+ indent-increase
                                 LaTeX-indent-level)))
      (when (looking-at (rx (0+ (not (any "%\n"))) ";"))
        (setq indent-increase (- indent-increase
                                 LaTeX-indent-level)))
      (+ prev-indent
         indent-increase))))

(setq LaTeX-indent-environment-list
      (nconc '(("thebibliography" myext-auctex-indent-env--indent-bibitem)
               ("tikzpicture" myext-auctex-indent-env--indent-tikzpicture)
               ("scope" myext-auctex-indent-env--indent-tikzpicture))
             LaTeX-indent-environment-list
             (mapcar (lambda (name) (list name 'myext-auctex-indent-env--indent-item))
                     myext-auctex-item-environment)))

(provide 'myext-auctex-indent-env)
