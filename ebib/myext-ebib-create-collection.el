(defun override:ebib--create-collection-ivy (databases)
  "Create a collection for use in `ebib-read-entry-ivy'.
The keys for the collection are taken from the databases listed
in DATABASES."
  (seq-reduce (lambda (coll db)
                (let ((file (propertize (ebib-db-get-filename db 'short) 'face 'ebib-display-bibfile-face)))
                  (append (mapcar (lambda (key)
                                    (propertize (myext-ebib-create-candidate db key file)
                                                'ebib-key key
                                                'ebib-db db))
                                  (ebib-db-list-keys db))
                          coll)))
              databases nil))
(advice-add 'ebib--create-collection-ivy :override 'override:ebib--create-collection-ivy)

(defun override:ebib--create-collection-helm (databases)
  "Create a collection for use in `ebib-read-entry-helm'.
The keys for the collection are taken from the databases listed
in DATABASES."
  (seq-reduce (lambda (coll db)
                (let ((file (propertize (ebib-db-get-filename db 'short) 'face 'ebib-display-bibfile-face)))
                  (append (mapcar (lambda (key)
                                    (cons (myext-ebib-create-candidate db key file)
                                          (list key db)))
                                  (ebib-db-list-keys db))
                          coll)))
              databases nil))
(advice-add 'ebib--create-collection-helm :override 'override:ebib--create-collection-helm)

(defun myext-ebib-create-candidate (db key file)
  (format "%-20s  %s (%s) «%s»"
          file
          (ebib--get-field-value-for-display "Author/Editor" key db 'face 'ebib-display-author-face)
          (ebib--get-field-value-for-display "Year" key db 'face 'ebib-display-year-face)
          (ebib--get-field-value-for-display "Title" key db)))

(provide 'myext-ebib-create-collection)
