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


;;; indent in \ifx cond ... \else ... \fi
(require 'myext-auctex-base)
(defvar my-left-keywords-regexp
  (rx (or "left" "bigl" "biggl") word-boundary))
(defvar my-right-keywords-regexp
  (rx (or "right" "bigr" "biggr") word-boundary))

(defun adv:LaTeX-indent-level-count ()
  "Count indentation change caused by all \\left, \\right, \\begin, and
\\end commands in the current line."
  (save-excursion
    (save-restriction
      (let ((count 0))
        (narrow-to-region (point)
                          (save-excursion
                            (re-search-forward
                             (concat "[^" TeX-esc "]"
                                     "\\(" LaTeX-indent-comment-start-regexp
                                     "\\)\\|\n\\|\\'"))
                            (backward-char)
                            (point)))
        (while (search-forward TeX-esc nil t)
          (cond
           ((looking-at my-left-keywords-regexp)
            (setq count (+ count LaTeX-left-right-indent-level)))
           ((looking-at my-right-keywords-regexp)
            (setq count (- count LaTeX-left-right-indent-level)))
           ((looking-at LaTeX-begin-regexp)
            (setq count (+ count LaTeX-indent-level)))
           ((looking-at myext-auctex-base--if-regexp)
            (unless (looking-back (rx "\\newif\\"))
              (setq count (+ count LaTeX-indent-level))))
           ((looking-at LaTeX-else-regexp))
           ((looking-at LaTeX-end-regexp)
            (setq count (- count LaTeX-indent-level)))
           ((looking-at (regexp-quote TeX-esc))
            (forward-char 1))))
        count))))
(advice-add 'LaTeX-indent-level-count :override 'adv:LaTeX-indent-level-count)

(defun override:LaTeX-indent-calculate-last (&optional force-type)
  "Return the correct indentation of a normal line of text.
The point is supposed to be at the beginning of the current line.
FORCE-TYPE can be used to force the calculation of an inner or
outer indentation in case of a commented line.  The symbols
'inner and 'outer are recognized."
  (let (line-comment-current-flag
	line-comment-last-flag
	comment-current-flag
	comment-last-flag)
    (beginning-of-line)
    (setq line-comment-current-flag (TeX-in-line-comment)
	  comment-current-flag (TeX-in-commented-line))
    (if comment-current-flag
	(skip-chars-backward "%\n\t ")
      (skip-chars-backward "\n\t "))
    (beginning-of-line)
    ;; If we are called in a non-comment line, skip over comment
    ;; lines.  The computation of indentation should in this case
    ;; rather take the last non-comment line into account.
    ;; Otherwise there might arise problems with e.g. multi-line
    ;; code comments.  This behavior is not enabled in docTeX mode
    ;; where large amounts of line comments may have to be skipped
    ;; and indentation should not be influenced by unrelated code in
    ;; other macrocode environments.
    (while (and (not (eq major-mode 'doctex-mode))
		(not comment-current-flag)
		(TeX-in-commented-line)
		(not (bobp)))
      (skip-chars-backward "\n\t ")
      (beginning-of-line))
    (setq line-comment-last-flag (TeX-in-line-comment)
	  comment-last-flag (TeX-in-commented-line))
    (LaTeX-back-to-indentation force-type)
    ;; Separate line comments and other stuff (normal text/code and
    ;; code comments).  Additionally we don't want to compute inner
    ;; indentation when a commented and a non-commented line are
    ;; compared.
    (cond ((or (and (eq major-mode 'doctex-mode)
		    (or (and line-comment-current-flag
			     (not line-comment-last-flag))
			(and (not line-comment-current-flag)
			     line-comment-last-flag)))
	       (and force-type
		    (eq force-type 'inner)
		    (or (and comment-current-flag
			     (not comment-last-flag))
			(and (not comment-current-flag)
			     comment-last-flag))))
	   0)
	  ((looking-at (concat (regexp-quote TeX-esc)
			       "begin *{\\("
			       LaTeX-document-regexp
			       "\\)}"))
	   ;; I dislike having all of the document indented...
	   (+ (LaTeX-current-indentation force-type)
	      ;; Some people have opening braces at the end of the
	      ;; line, e.g. in case of `\begin{letter}{%'.
	      (TeX-brace-count-line)))
	  ((and (eq major-mode 'doctex-mode)
		(looking-at (concat (regexp-quote TeX-esc)
				    "end[ \t]*{macrocode\\*?}"))
		fill-prefix
		(TeX-in-line-comment))
	   ;; Reset indentation to zero after a macrocode
	   ;; environment.
	   0)
	  ((looking-at (concat (regexp-quote TeX-esc)
			       "begin *{\\("
			       (LaTeX-verbatim-regexp)
			       "\\)}"))
	   0)
	  ((looking-at (concat (regexp-quote TeX-esc)
			       "end *{\\("
			       (LaTeX-verbatim-regexp)
			       "\\)}"))
	   ;; If I see an \end{verbatim} in the previous line I skip
	   ;; back to the preceding \begin{verbatim}.
	   (save-excursion
	     (if (re-search-backward (concat (regexp-quote TeX-esc)
					     "begin *{\\("
					     (LaTeX-verbatim-regexp)
					     "\\)}") 0 t)
		 (LaTeX-indent-calculate-last force-type)
	       0)))
	  (t (+ (LaTeX-current-indentation force-type)
		(if (not (and force-type
			      (eq force-type 'outer)
			      (TeX-in-commented-line)))
		    (+ (LaTeX-indent-level-count)
		       (TeX-brace-count-line))
		  0)
		(cond ((looking-at (concat (regexp-quote TeX-esc)
					   "\\("
					   LaTeX-end-regexp
					   "\\)"))
		       LaTeX-indent-level)
		      ((looking-at
			(concat (regexp-quote TeX-esc) my-right-keywords-regexp))
		       LaTeX-left-right-indent-level)
		      ((looking-at (concat (regexp-quote TeX-esc)
					   "\\("
					   LaTeX-item-regexp
					   "\\)"))
		       (- LaTeX-item-indent))
		      ((looking-at "}")
		       TeX-brace-indent-level)
		      (t 0)))))))
(advice-add 'LaTeX-indent-calculate-last :override 'override:LaTeX-indent-calculate-last)

(defun override:LaTeX-indent-calculate (&optional force-type)
  "Return the indentation of a line of LaTeX source.
FORCE-TYPE can be used to force the calculation of an inner or
outer indentation in case of a commented line.  The symbols
'inner and 'outer are recognized."
  (save-excursion
    (LaTeX-back-to-indentation force-type)
    (let ((i 0)
	  (list-length (safe-length docTeX-indent-inner-fixed))
	  (case-fold-search nil)
	  entry
	  found)
      (cond ((save-excursion (beginning-of-line) (bobp)) 0)
	    ((and (eq major-mode 'doctex-mode)
		  fill-prefix
		  (TeX-in-line-comment)
		  (progn
		    (while (and (< i list-length)
				(not found))
		      (setq entry (nth i docTeX-indent-inner-fixed))
		      (when (looking-at (nth 0 entry))
			(setq found t))
		      (setq i (1+ i)))
		    found))
	     (if (nth 2 entry)
		 (- (nth 1 entry) (if (integerp comment-padding)
				      comment-padding
				    (length comment-padding)))
	       (nth 1 entry)))
	    ((looking-at (concat (regexp-quote TeX-esc)
				 "\\(begin\\|end\\){\\("
				 (LaTeX-verbatim-regexp)
				 "\\)}"))
	     ;; \end{verbatim} must be flush left, otherwise an unwanted
	     ;; empty line appears in LaTeX's output.
	     0)
	    ((and LaTeX-indent-environment-check
		  ;; Special environments.
		  (let ((entry (assoc (or LaTeX-current-environment
					  (LaTeX-current-environment))
				      LaTeX-indent-environment-list)))
		    (and entry
			 (nth 1 entry)
			 (funcall (nth 1 entry))))))
	    ((looking-at (concat (regexp-quote TeX-esc)
				 "\\("
				 LaTeX-end-regexp
				 "\\)"))
	     ;; Backindent at \end.
	     (- (LaTeX-indent-calculate-last force-type) LaTeX-indent-level))
	    ((looking-at (concat (regexp-quote TeX-esc) my-right-keywords-regexp))
	     ;; Backindent at \right.
	     (- (LaTeX-indent-calculate-last force-type)
		LaTeX-left-right-indent-level))
	    ((looking-at (concat (regexp-quote TeX-esc)
				 "\\("
				 LaTeX-item-regexp
				 "\\)"))
	     ;; Items.
	     (+ (LaTeX-indent-calculate-last force-type) LaTeX-item-indent))
	    ((looking-at "}")
	     ;; End brace in the start of the line.
	     (- (LaTeX-indent-calculate-last force-type)
		TeX-brace-indent-level))
	    (t (LaTeX-indent-calculate-last force-type))))))
(advice-add 'LaTeX-indent-calculate :override 'override:LaTeX-indent-calculate)

(provide 'myext-auctex-indent)
