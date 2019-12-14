;;; markdown 内に liquid tag を書くための拡張

(defun mdlq-liquid-tag-regexp (&optional name-list)
  (let ((name-rx (if name-list
                     (cons 'or name-list)
                   '(1+ alpha))))
    (eval `(rx (group-n 1 "{%")
               (0+ space)
               (group-n 3 ,name-rx)
               (group-n 4 (1+ (not (any ?% ?{ ?}))))
               (group-n 2 "%}")))))

(font-lock-add-keywords
 'markdown-mode
 `((,(mdlq-liquid-tag-regexp)
    (1 'font-lock-constant-face)
    (2 'font-lock-constant-face)
    (3 'font-lock-keyword-face))))

(defvar mdlq-liquid-begin-tag-list
  '("theorem" "definition" "proposition" "lemma" "corollary" "remark"))

(defvar mdlq-liquid-end-tag-list
  (mapcar (lambda (name) (concat "end" name))
          mdlq-liquid-begin-tag-list))

(defvar mdlq-liquid-begin-tag-regexp
  (mdlq-liquid-tag-regexp mdlq-liquid-begin-tag-list))

(defvar mdlq-liquid-end-tag-regexp
  (mdlq-liquid-tag-regexp mdlq-liquid-end-tag-list))

(defun mdlq-line-contains (regexp &optional line)
  "current line if LINE is 0, and previous line if LINE is -1"
  (save-excursion
    (forward-line line)
    (beginning-of-line)
    (search-forward-regexp regexp (line-end-position) t)))

(defvar mdlq-liquid-indent-width 2)

(defun mdlq-calc-indents ()
  (let* ((prev-line-pos (markdown-prev-line-indent))
         (indent prev-line-pos))
    (when (mdlq-line-contains mdlq-liquid-begin-tag-regexp -1)
      (setq indent (+ indent mdlq-liquid-indent-width)))
    (when (mdlq-line-contains mdlq-liquid-end-tag-regexp 0)
      (setq indent (- indent mdlq-liquid-indent-width)))
    (if (or (= indent prev-line-pos) (< indent 0))
        nil
      (list indent))))

(defun mdlq-advice:markdown-calc-indents (orig-func &rest args)
  (let ((result (apply orig-func args))
        (liquid-indent (mdlq-calc-indents)))
    ;; (my-message "%S" result)
    ;; (my-message "%S" (markdown-prev-line-indent))
    (append liquid-indent result)))
(advice-add 'markdown-calc-indents :around 'mdlq-advice:markdown-calc-indents)

(provide 'myext-markdown-liquid)
