;;; myext-auctex-error.el --- Highlight mode-line on AUCTeX error -*- lexical-binding: t; -*-

;;; auctex のコンパイルエラーを，分かりやすく通知する
;;; - mode line が赤く光る
;;; - post-command-hook で，操作した後に赤色を消す

(require 'tex)
(require 'face-remap)

(defgroup myext-auctex-error nil
  "Notify AUCTeX errors by highlighting the mode-line."
  :group 'TeX)

(defcustom myext-auctex-error-background "red"
  "Background color used to notify AUCTeX errors."
  :type 'color)

(defvar-local myext-auctex-error--remap-cookies nil
  "Face-remap cookies for the current buffer.")

(defvar myext-auctex-error--highlighted-buffers nil
  "Buffers whose mode-line is currently highlighted.")

(defun myext-auctex-error--remove-from-buffer (buffer)
  "Remove mode-line highlighting from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (dolist (cookie myext-auctex-error--remap-cookies)
        (face-remap-remove-relative cookie))
      (setq myext-auctex-error--remap-cookies nil)
      (force-mode-line-update))))

(defun myext-auctex-error-clear ()
  "Remove all AUCTeX error mode-line highlighting."
  (interactive)
  (dolist (buffer myext-auctex-error--highlighted-buffers)
    (myext-auctex-error--remove-from-buffer buffer))
  (setq myext-auctex-error--highlighted-buffers nil)
  (remove-hook 'post-command-hook #'myext-auctex-error-clear)
  (force-mode-line-update t))

(defun myext-auctex-error--highlight-buffer (buffer)
  "Highlight mode-line of BUFFER until the next command finishes."
  (when (buffer-live-p buffer)
    ;; 既に赤くなっている場合は、いったん古い remap を消す。
    (myext-auctex-error--remove-from-buffer buffer)
    (with-current-buffer buffer
      (push (face-remap-add-relative
             'mode-line
             :background myext-auctex-error-background)
            myext-auctex-error--remap-cookies)
      (push (face-remap-add-relative
             'mode-line-inactive
             :background myext-auctex-error-background)
            myext-auctex-error--remap-cookies)
      ;; Emacs 29 以降などでは mode-line-active もあり得る。
      (when (facep 'mode-line-active)
        (push (face-remap-add-relative
               'mode-line-active
               :background myext-auctex-error-background)
              myext-auctex-error--remap-cookies))
      (cl-pushnew buffer myext-auctex-error--highlighted-buffers)
      (force-mode-line-update)))
  ;; 次のコマンド終了時に解除する。
  ;; 何も操作しなければ赤いまま。
  (add-hook 'post-command-hook #'myext-auctex-error-clear))

(defun myext-auctex-error--latexmk-command-p (name)
  "Return non-nil if AUCTeX command NAME should trigger notification."
  (string= name "LatexMk"))

(defun myext-auctex-error--TeX-TeX-sentinel-check-around (orig-fun process name)
  "Highlight mode-line if ORIG-FUN reports an error for LatexMk."
  (let ((has-error (funcall orig-fun process name)))
    (when (and has-error
               (myext-auctex-error--latexmk-command-p name)
               (boundp 'TeX-command-buffer)
               (buffer-live-p TeX-command-buffer))
      (myext-auctex-error--highlight-buffer TeX-command-buffer))
    has-error))

(with-eval-after-load 'tex
  (advice-add 'TeX-TeX-sentinel-check
              :around
              #'myext-auctex-error--TeX-TeX-sentinel-check-around))

(provide 'myext-auctex-error)
