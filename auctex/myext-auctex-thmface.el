;;; lemma や theorem などの環境に背景色をつける
;;; https://tex.stackexchange.com/questions/35605/how-to-highlight-certain-environments-in-emacs
;;; リンク先からの変更点
;;; - 関数定義を外に出した
;;; - setup を LaTeX-mode-hook の中ではなく，tex-mode が load された後すぐに実行するようにした
;;; - font-latex-keywords-2 に font-latex-match-theorem-envII を追加する際の色々なフラグを変更

(defcustom font-latex-theorem-environments
  '("theorem" "lemma" "proposition" "corollary" "definition")
  "List of theorem-like environment names for font locking."
  :type '(repeat string)
  :group 'font-latex)

(defface my-latex-theorem-face
  `((t :background ,mytheme-environment-background
       ))
  "face for theorem")

(defun my-latex-generate-regexp (command environments)
  (let ((environments (cond
                       ((stringp environments)
                        (list environments))
                       ((listp environments)
                        environments)
                       (t (error "invalid environments")))))
    (eval `(rx "\\" (eval command) (0+ (any " \t")) "{"
               (group (or ,@environments)
                      (? "*"))
               "}"))))

(defun font-latex-match-theorem-envII (limit)
  "Match theorem patterns up to LIMIT.
Used for patterns like:
\\begin{equation}
 fontified stuff
\\end{equation}
The \\begin{equation} and \\end{equation} are not fontified here."
  (when (re-search-forward (my-latex-generate-regexp "begin" font-latex-theorem-environments)
                           ;; (eval `(rx "\\begin" (0+ (any " \t")) "{"
                           ;;            (group (or ,@font-latex-theorem-environments)
                           ;;                   (? "*"))
                           ;;            "}"))
                           ;; (concat "\\\\begin[ \t]*{"
                           ;;         (regexp-opt font-latex-theorem-environments t)
                           ;;         "\\*?}")
                           limit t)
    (let ((beg (match-end 0)) end)
      (if (re-search-forward (my-latex-generate-regexp
                              "end" (buffer-substring-no-properties (match-beginning 1)
                                                                    (match-end 1)))
                             ;; (rx "\\end" (0+ (any " \t")) "{"
                             ;;     (eval (buffer-substring-no-properties
                             ;;            (match-beginning 1)
                             ;;            (match-end 1)))
                             ;;     "}")
                             ;; (concat "\\\\end[ \t]*{"
                             ;;         (regexp-quote
                             ;;          (buffer-substring-no-properties
                             ;;           (match-beginning 1)
                             ;;           (match-end 0))))
                             ;; XXX: Should this rather be done by
                             ;; extending the region to be fontified?
                             (+ limit font-latex-multiline-boundary) 'move)
          (setq end (match-beginning 0))
        (goto-char beg)
        (setq end beg))
      (font-latex-put-multiline-property-maybe beg end)
      (store-match-data (list beg end))
      t)))

(defun font-latex-extend-region-backwards-theorem-envII (beg end)
  "Return position to extend region backwards for theorem environments.
Return nil if region does not have to be extended for a multiline
environment to fit in.  The region between the positions BEG and
END marks boundaries for searching for environment ends."
  (save-excursion
    (goto-char end)
    (catch 'extend
      (while (re-search-backward
              (my-latex-generate-regexp "end" font-latex-theorem-environments)
              ;; (eval `(rx "\\end" (0+ (any " \t")) "{"
              ;;            (group (or ,@font-latex-theorem-environments)
              ;;                   (? "*"))
              ;;            "}"))
              ;; (concat "\\\\end[ \t]*{"
              ;;         (regexp-opt font-latex-theorem-environments t)
              ;;         "\\*?}")
              beg t)
        (when (and (re-search-backward (my-latex-generate-regexp
                                        "begin"
                                        (buffer-substring-no-properties (match-beginning 1)
                                                                        (match-end 1)))
                                       ;; (rx "\\begin" (0+ (any " \t")) "{"
                                       ;;     (eval (buffer-substring-no-properties
                                       ;;            (match-beginning 1)
                                       ;;            (match-end 1)))
                                       ;;     "}")
                                       ;; (concat  "\\\\begin[ \t]*{"
                                       ;;          (buffer-substring-no-properties
                                       ;;           (match-beginning 1)
                                       ;;           (match-end 0)))
                                       (- beg font-latex-multiline-boundary) t)
                   (< (point) beg))
          (throw 'extend (point))))
      nil)))

(defun my-latex-theorem-env-setup ()
  "これを実行すると反映される"
  (require 'font-latex)
  (add-to-list 'font-latex-keywords-2
               '(font-latex-match-theorem-envII (0 'my-latex-theorem-face prepend t))
               t)
  (add-to-list 'font-latex-extend-region-functions
               'font-latex-extend-region-backwards-theorem-envII))

(my-latex-theorem-env-setup)

(provide 'myext-auctex-thmface)
