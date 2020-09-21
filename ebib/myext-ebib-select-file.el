(defun override:ebib--select-file (files n key)
  "Split FILES into separate files and return the Nth.
FILES should be a string of file names separated by
`ebib-filename-separator'.  If there is only one file name in
FILES, it is returned regardless of the value of N.  If N is nil,
the user is asked to enter a number, unless there is only one
file in FILES, in which case that one is chosen automatically.
If FILES is nil, a file name is created on the basis of KEY.  See
the function `ebib--create-file-name-from-key' for details."
  (if (not files)
      (ebib--create-file-name-from-key key "pdf")
    (let ((file-list (ebib--split-files files)))
      (cond
       ((= (length file-list) 1)
        (nth 0 file-list))
       ((null n)
        (cond
         ((and (boundp 'ivy-mode) ivy-mode)
          (let (selected-file)
            (ivy-read "Select file: " file-list
                      :action (lambda (file) (setq selected-file file)))
            selected-file  ; null check しなくて大丈夫？
            ))
         (t
          (setq n (string-to-number (read-string (format "Select file [1-%d]: " (length file-list)))))
          (unless (<= 1 n (length file-list))  ; Unless n is within range.
            (error "[Ebib] No such file (%d)" n))
          (nth (1- n) file-list))))
       (t (nth (1- n) file-list)))
      ;; (cond
      ;;  ((= (length file-list) 1)
      ;;   (setq n 1))
      ;;  ((null n)
      ;;   (setq n (string-to-number (read-string (format "Select file [1-%d]: " (length file-list)))))))
      ;; (unless (<= 1 n (length file-list))  ; Unless n is within range.
      ;;   (error "[Ebib] No such file (%d)" n))
      ;; (nth (1- n) file-list)
      )))
(advice-add 'ebib--select-file :override 'override:ebib--select-file)

(provide 'myext-ebib-select-file)
