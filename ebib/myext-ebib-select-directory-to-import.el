(defun override:ebib-import-file (arg)
  "Import a file into the database.
Ask the user for a file path, rename it and move it to the first
directory in `ebib-file-search-dirs'.  The new name is created by
applying the function in `ebib-name-transform-function' to the
entry key.  The file extension of the original file is retained.
If prefix ARG is non-nil, do not delete the original file."
  (interactive "P")
  (let* ((key (ebib--get-key-at-point))
         (file-path (expand-file-name (read-file-name "File to import: " ebib-import-directory nil t)))
         (ext (file-name-extension file-path))
         (new-name (ebib--create-file-name-from-key key ext))
         (dest-dir (file-name-as-directory
                    (read-directory-name "Import to: " (car ebib-file-search-dirs) nil t)))
         (dest-path (concat dest-dir new-name))
         (overwrite nil))
    (if (not (file-writable-p dest-path))
        (error "[Ebib] Cannot write file %s" dest-path))
    (while (and (file-exists-p dest-path)
                (not overwrite))
      (let ((choice (read-char-choice (format "File %s already exists; (o)verwrite / (r)ename / (c)ancel? " new-name) '(?o ?r ?c ?q))))
        (cl-case choice
          ((?c ?q) (error "[Ebib] Cancelled importing file"))
          (?r (setq new-name (read-string (format "Change `%s' to: " new-name) new-name))
              (setq dest-path (concat (file-name-as-directory (car ebib-file-search-dirs)) new-name)))
          (?o (setq overwrite t)))))
    (copy-file file-path dest-path)
    (unless arg
      (delete-file file-path t))
    (let ((files (ebib-get-field-value ebib-file-field key ebib--cur-db 'noerror 'unbraced)))
      (when (or (null files)
                (not (string-match-p (regexp-quote new-name) files)))
        (ebib-set-field-value ebib-file-field (ebib--transform-file-name-for-storing (expand-file-name dest-path)) key ebib--cur-db ebib-filename-separator)
        (ebib--set-modified t ebib--cur-db t (seq-filter (lambda (dependent)
                                                           (ebib-db-has-key key dependent))
                                                         (ebib--list-dependents ebib--cur-db)))
        (ebib--update-entry-buffer)))))
(advice-add 'ebib-import-file :override 'override:ebib-import-file)

(provide 'myext-ebib-select-directory-to-import)
