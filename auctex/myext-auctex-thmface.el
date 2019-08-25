;;; lemma や theorem などの環境に背景色をつける
;;; https://tex.stackexchange.com/questions/35605/how-to-highlight-certain-environments-in-emacs
;;; リンク先からの変更点
;;; - 関数定義を外に出した
;;; - setup を LaTeX-mode-hook の中ではなく，tex-mode が load された後すぐに実行するようにした
;;; - font-latex-keywords-2 に myext-auctex-thmface--match-theorem-envII を追加する際の色々なフラグを変更

(defcustom myext-auctex-thmface--theorem-environments
  '("theorem" "lemma" "proposition" "corollary" "definition")
  "List of theorem-like environment names for font locking."
  :type '(repeat string)
  :group 'font-latex)

(defface myext-auctex-thmface-theorem-face
  `((t :background ,mytheme-environment-background
       ))
  "face for theorem")

(defun myext-auctex-thmface--generate-regexp (command environments)
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

(defun myext-auctex-thmface--match-theorem-envII (limit)
  "Match theorem patterns up to LIMIT.
Used for patterns like:
\\begin{equation}
 fontified stuff
\\end{equation}
The \\begin{equation} and \\end{equation} are not fontified here."
  (when (re-search-forward (myext-auctex-thmface--generate-regexp "begin" myext-auctex-thmface--theorem-environments)
                           ;; (eval `(rx "\\begin" (0+ (any " \t")) "{"
                           ;;            (group (or ,@font-latex-theorem-environments)
                           ;;                   (? "*"))
                           ;;            "}"))
                           ;; (concat "\\\\begin[ \t]*{"
                           ;;         (regexp-opt myext-auctex-thmface--theorem-environments t)
                           ;;         "\\*?}")
                           limit t)
    (let ((beg (match-end 0)) end)
      (if (re-search-forward (myext-auctex-thmface--generate-regexp
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

(defun myext-auctex-thmface--extend-region-backwards-theorem-envII (beg end)
  "Return position to extend region backwards for theorem environments.
Return nil if region does not have to be extended for a multiline
environment to fit in.  The region between the positions BEG and
END marks boundaries for searching for environment ends."
  (save-excursion
    (goto-char end)
    (catch 'extend
      (while (re-search-backward
              (myext-auctex-thmface--generate-regexp "end" myext-auctex-thmface--theorem-environments)
              ;; (eval `(rx "\\end" (0+ (any " \t")) "{"
              ;;            (group (or ,@font-latex-theorem-environments)
              ;;                   (? "*"))
              ;;            "}"))
              ;; (concat "\\\\end[ \t]*{"
              ;;         (regexp-opt myext-auctex-thmface--theorem-environments t)
              ;;         "\\*?}")
              beg t)
        (when (and (re-search-backward (myext-auctex-thmface--generate-regexp
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

(defun myext-auctex-thmface-setup ()
  "これを実行すると反映される"
  (require 'font-latex)
  (add-to-list 'font-latex-keywords-2
               '(myext-auctex-thmface--match-theorem-envII (0 'myext-auctex-thmface-theorem-face prepend t))
               t)
  (add-to-list 'font-latex-extend-region-functions
               'myext-auctex-thmface--extend-region-backwards-theorem-envII))

(myext-auctex-thmface-setup)

(provide 'myext-auctex-thmface)
