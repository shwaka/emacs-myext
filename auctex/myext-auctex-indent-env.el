;;; indent as the following (customizable):
;;; \begin{itemize}
;;;   \item hoge
;;;     fuga
;;;   \item piyo
;;; \end{itemize}
(defun LaTeX-indent-item--inner (regexp-environ item-cmd)
  "intersection of LaTeX-indent-item and LaTeX-indent-bibitem"
  (save-match-data
    (let* ((offset LaTeX-indent-level)
           (contin (or (and (boundp 'LaTeX-indent-level-item-continuation)
                            LaTeX-indent-level-item-continuation)
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
(defun LaTeX-indent-item ()
  "Provide proper indentation for LaTeX \"itemize\",\"enumerate\", and
\"description\" environments.

  \"\\item\" is indented `LaTeX-indent-level' spaces relative to
  the the beginning of the environment.

  Continuation lines are indented either twice
  `LaTeX-indent-level', or `LaTeX-indent-level-item-continuation'
  if the latter is bound."
  (LaTeX-indent-item--inner "\\(itemize\\|\\enumerate\\|description\\)"
                            "\\\\item\\>"))
(defun LaTeX-indent-bibitem ()
  "Provide proper indentation for LaTeX \"thebibliography\" environment.

  \"\\bibitem\" is indented `LaTeX-indent-level' spaces relative to
  the the beginning of the environment.

  Continuation lines are indented either twice
  `LaTeX-indent-level', or `LaTeX-indent-level-item-continuation'
  if the latter is bound."
  (LaTeX-indent-item--inner "\\(thebibliography\\)"
                            "\\\\bibitem"))

(defcustom LaTeX-indent-level-item-continuation 4
  "*Indentation of continuation lines for items in itemize-like
environments."
  :group 'LaTeX-indentation
  :type 'integer)

;;; indent in tikzpicture
(defvar my-LaTeX-tikz-commands
  '("path" "draw" "coordinate" "clip" "node")
  "commands which begin tikz commands (e.g. \\path, \\draw)")
(defun my-LaTeX-goto-previous-nontriv-line ()
  "go to (the beginning of) the previous nontrivial line"
  (forward-line -1)
  (back-to-indentation)
  (while (looking-at (rx (or line-end "%")))
    (forward-line -1)
    (back-to-indentation)))
(defun my-LaTeX-indent-tikzpicture ()
  "indent in tikzpicture"
  (save-excursion
    (let ((indent-increase 0)
          prev-indent)
      ;; current line starts with \end
      (when (looking-at (rx "\\end"))
        (setq indent-increase (- indent-increase
                                 LaTeX-indent-level)))
      (my-LaTeX-goto-previous-nontriv-line)
      (setq prev-indent (current-column))
      ;; read previous line
      (when (looking-at (rx "\\begin"))
        (setq indent-increase (+ indent-increase
                                 LaTeX-indent-level)))
      (when (looking-at (eval `(rx "\\" (or ,@my-LaTeX-tikz-commands))))
        (setq indent-increase (+ indent-increase
                                 LaTeX-indent-level)))
      (when (looking-at (rx (0+ (not (any "%\n"))) ";"))
        (setq indent-increase (- indent-increase
                                 LaTeX-indent-level)))
      (+ prev-indent
         indent-increase))))

(setq LaTeX-indent-environment-list
      (nconc '(("itemize" LaTeX-indent-item)
               ("enumerate" LaTeX-indent-item)
               ("description" LaTeX-indent-item)
               ("thebibliography" LaTeX-indent-bibitem)
               ("tikzpicture" my-LaTeX-indent-tikzpicture)
               ("scope" my-LaTeX-indent-tikzpicture))
             LaTeX-indent-environment-list))

(provide 'myext-auctex-indent-env)
