;;; kotlin-mode での lambda のインデントを変更する

(defun override:kotlin-mode--line-continuation ()
  "Return whether this line continues a statement in the previous line"
  (or
   (kotlin-mode--line-begins "\\([.=:]\\|->\\|[sg]et\\b\\)")
   ;; (save-excursion
   ;;   (kotlin-mode--prev-line)
   ;;   (kotlin-mode--line-ends "\\([=:]\\|->\\)"))
   ))
(advice-add 'kotlin-mode--line-continuation :override 'override:kotlin-mode--line-continuation)

(defun override:kotlin-mode--count-to-line-start (counter)
  "Count the brackets on the current line, starting from the
cursor position, and working backward, incrementing the count +1
for open-brackets, -1 for close-brackets.

Mark the COUNTER finished, set indentation, and return as soon as
the overall count exceeds zero.  If the counter is zero at the
beginning of the line, Mark the counter finished and set
indentation.  If we hit a beginning of line but the counter is
negative, just return without marking finished."
  (when (nth 4 (syntax-ppss))
    ;; If the point is inside a comment, goto the beginning of the comment.
    (goto-char (nth 8 (syntax-ppss))))
  (save-excursion
    (let ((line-beginning-position (line-beginning-position)))
      (while (and (<= (oref counter count) 0) (not (bolp)))
        (forward-comment (- (point)))
        (backward-char)
        (when (< (point) line-beginning-position)
          (goto-char line-beginning-position))
        (cond ((eq (char-syntax (char-after)) ?\()
               (cl-incf (oref counter count)))
              ((eq (char-syntax (char-after)) ?\))
               (cl-decf (oref counter count))))))
    ;; We are at the beginning of the line, or just before an
    ;; unmatching open bracket.
    (cond
     ;; If the net-bracket-count is zero, use this indentation
     ((= (oref counter count) 0)
      (oset counter finished t)
      (if (oref counter use-base)
          ;; Indenting a line that is neither close bracket nor the
          ;; first element of a block or a list.  Found the previous
          ;; line.  So align with the previous line, without effect of
          ;; continued expression at the previous line.
          (kotlin-mode--add-indent counter (kotlin-mode--base-indentation))
        ;; Indenting close bracket or the first element of a block or
        ;; a list.  So align with this line, optionally with extra
        ;; indentation.
        (kotlin-mode--add-indent counter (current-indentation))))
     ;; If we've now counted more open-brackets than close-brackets,
     ;; use the indentation of the content immediately following the
     ;; final open-bracket.
     ;;
     ;; Example:
     ;;
     ;; Suppose indenting "bar2()" in the following example:
     ;;
     ;; foo(  bar1(),
     ;;       bar2())
     ;;
     ;; We are at just before the open bracket of "foo".  So skip the
     ;; open bracket and spaces, then align "bar2()" with "bar1()".
     ((> (oref counter count) 0)
      (oset counter finished t)
      (forward-char)
      (skip-syntax-forward "(")
      (skip-syntax-forward "-")
      (kotlin-mode--add-indent counter
                               ;; coding convention に合わせて変更
                               (+ (current-indentation) kotlin-tab-width)
                               ;; (current-column)
                               )))))
(advice-add 'kotlin-mode--count-to-line-start :override 'override:kotlin-mode--count-to-line-start)

(provide 'myext-kotlin-mode-lambda)
