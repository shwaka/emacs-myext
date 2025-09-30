;;; 以下2つは使われてないっぽい (バージョンアップのせい？)
;; (defun override:ebib--create-collection-ivy (databases)
;;   "Create a collection for use in `ebib-read-entry-ivy'.
;; The keys for the collection are taken from the databases listed
;; in DATABASES."
;;   (seq-reduce (lambda (coll db)
;;                 (let ((file (propertize (ebib-db-get-filename db 'short) 'face 'ebib-display-bibfile-face)))
;;                   (append (mapcar (lambda (key)
;;                                     (propertize (funcall myext-ebib-key-display-function db key file)
;;                                                 'ebib-key key
;;                                                 'ebib-db db))
;;                                   (ebib-db-list-keys db))
;;                           coll)))
;;               databases nil))
;; (advice-add 'ebib--create-collection-ivy :override 'override:ebib--create-collection-ivy)

;; (defun override:ebib--create-collection-helm (databases)
;;   "Create a collection for use in `ebib-read-entry-helm'.
;; The keys for the collection are taken from the databases listed
;; in DATABASES."
;;   (seq-reduce (lambda (coll db)
;;                 (let ((file (propertize (ebib-db-get-filename db 'short) 'face 'ebib-display-bibfile-face)))
;;                   (append (mapcar (lambda (key)
;;                                     (cons (funcall myext-ebib-key-display-function db key file)
;;                                           (list key db)))
;;                                   (ebib-db-list-keys db))
;;                           coll)))
;;               databases nil))
;; (advice-add 'ebib--create-collection-helm :override 'override:ebib--create-collection-helm)

(defun override:ebib--create-completion-collection (databases &optional prepend-db)
  "Create a collection for use in `ebib-read-entry-*' functions.
The keys for the collection are taken from DATABASES.  Return
value is an alist with the completion strings as keys and a list
of entry key and database as values.  If PREPEND-DB is non-nil,
the database name is prepended to the candidate string.  This is
especially useful if helm or ivy is used as completion system.

If PREPEND-DB is nil, the database name is added to the string in
the text property `selectrum-candidate-display-right-margin'.
When selectrum is used as the completion system, the database is
displayed at the right margin.  If selectrum is not used, the key
is prepended to the completion candidates."
  (seq-reduce (lambda (coll db)
                (let ((file (propertize (ebib-db-get-filename db 'short) 'face 'ebib-display-bibfile-face)))
                  (append (mapcar (lambda (key)
                                    (let ((candidate (funcall myext-ebib-key-display-function db key file)))
                                      (setq candidate
                                            (cond
                                             (prepend-db
                                              (concat (format "%-20s  " file) "  " candidate))
                                             ((and (boundp 'selectrum-mode) selectrum-mode)
                                              (propertize candidate 'selectrum-candidate-display-right-margin file))
                                             (t (concat key "  " candidate))))
                                      (cons candidate (list key db))))
                                  (ebib-db-list-keys db))
                          coll)))
              databases nil))
(advice-add 'ebib--create-completion-collection :override 'override:ebib--create-completion-collection)

(defvar myext-ebib-key-display-function
  #'myext-ebib-default-key-display-function
  "function to create candidate")

(defun myext-ebib-default-key-display-function (db key file)
  (format "%-20s  %s (%s) «%s»"
          file
          (ebib--get-field-value-for-display "Author/Editor" key db 'face 'ebib-display-author-face)
          (ebib--get-field-value-for-display "Year" key db 'face 'ebib-display-year-face)
          (ebib--get-field-value-for-display "Title" key db)))

(provide 'myext-ebib-create-collection)
