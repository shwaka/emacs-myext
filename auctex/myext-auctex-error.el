;;; auctex のコンパイルエラーを，分かりやすく通知する
;;; - mode line が赤く光る
;;; - post-command-hook を使っているので，何もしなければ赤いまま
;;; - エラー発生から3秒間は，操作したとしても赤いまま

;;; error 時
(defun override:TeX-TeX-sentinel-check (process name)
  "Cleanup TeX output buffer after running TeX.
Return nil ifs no errors were found."
  (save-excursion
    (goto-char (point-max))
    (cond
     ((and (string-match "ConTeXt" name) (boundp 'ConTeXt-Mark-version)
	   (with-current-buffer TeX-command-buffer
	     (string= ConTeXt-Mark-version "IV")))
      (when (re-search-backward " > result saved in file: \\(.*?\\), " nil t)
	(let ((output-file (TeX-match-buffer 1)))
	  ;; Shave off quotation marks if present.
	  (when (string-match "\\`\"\\(.*\\)\"\\'" output-file)
	    (setq output-file (match-string 1 output-file)))
	  (setq TeX-output-extension
		(if (string-match "\\.\\([^.]*\\)$" output-file)
		    (match-string 1 output-file)
		  "dvi")))
	(if (re-search-forward ", \\([0-9]+\\) shipped pages, " nil t)
	    (setq TeX-current-page (concat "{" (TeX-match-buffer 1) "}")))))
     (t
      (if (re-search-backward "^Output written on \\(.*?\\) (\\([0-9]+\\) page"
			      nil t)
	  (let ((output-file (TeX-match-buffer 1)))
	    (setq TeX-current-page (concat "{" (TeX-match-buffer 2) "}"))
	    ;; Shave off quotation marks if present.
	    (when (string-match "\\`\"\\(.*\\)\"\\'" output-file)
	      (setq output-file (match-string 1 output-file)))
	    (setq TeX-output-extension
		  (if (string-match "\\.\\([^.]*\\)$" output-file)
		      (match-string 1 output-file)
		    "dvi")))))))
  (if process (TeX-format-mode-line process))
  (if (re-search-forward "^\\(!\\|.*:[0-9]+:\\) " nil t)
      (progn
        (myext-auctex-error--highlight)
	(message "%s errors in `%s'. Use %s to display." name (buffer-name)
		 (substitute-command-keys
		  "\\<TeX-mode-map>\\[TeX-next-error]"))
	(setq TeX-command-next TeX-command-default)
	;; error reported to TeX-error-report-switches
	(setq TeX-error-report-switches
	      (plist-put TeX-error-report-switches
			 (intern (plist-get TeX-error-report-switches
					    'TeX-current-master))
			 t))
	t)
    (let (dvi2pdf)
      (if (with-current-buffer TeX-command-buffer
	    (and TeX-PDF-mode (setq dvi2pdf (TeX-PDF-from-DVI))))
	  (setq TeX-command-next dvi2pdf)
        (setq TeX-command-next TeX-command-Show)))
    nil))
(advice-add 'TeX-TeX-sentinel-check :override 'override:TeX-TeX-sentinel-check)
(defvar myext-auctex-error--remap-list nil)
(defun myext-auctex-error--add-remap (face key value)
  (push (cons
         (current-buffer)
         (face-remap-add-relative face key value))
        myext-auctex-error--remap-list))
(defun myext-auctex-error--highlight ()
  "エラー発生を目立たせる"
  (let ((orig-bg (face-background 'mode-line)))
    ;; (set-face-background 'mode-line "red")
    ;; (run-at-time "1 sec" nil
    ;;              (lambda (bg)
    ;;                (set-face-background 'mode-line bg))
    ;;              orig-bg)
    (with-current-buffer TeX-command-buffer
      (myext-auctex-error--add-remap 'mode-line :background "red")
      (myext-auctex-error--add-remap 'mode-line-inactive :background "red")
      (run-at-time "3 sec" nil
                   (lambda ()
                     (add-hook 'post-command-hook #'myext-auctex-error--remove-remaps))))))
(defun myext-auctex-error--remove-remaps ()
  (dolist (buf-cookie myext-auctex-error--remap-list)
    (let ((buf (car buf-cookie))
          (cookie (cdr buf-cookie)))
      (with-current-buffer buf
        (face-remap-remove-relative cookie))))
  (setq myext-auctex-error--remap-list nil)
  (remove-hook 'post-command-hook #'myext-auctex-error--remove-remaps))


(provide 'myext-auctex-error)
