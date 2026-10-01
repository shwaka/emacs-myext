;;; Matches if???, where ??? is an almost arbitrary string.
;;; Doesn't match iff, since this is not an if-condition
(defvar myext-auctex-base--if-regexp
  (rx (seq "if" (or ""
                    (>= 2 (char alpha "@"))
                    (char "a-eg-zA-Z@")))
      word-boundary)
  "\\ifhoge をインデントするために追加した変数")
;; (setq myext-auctex-base--begin-regexp "\\(?:begin\\|ifx\\|ifnum\\|ifdefined\\)\\b")
(defvar myext-auctex-base--else-regexp
  (rx "else" word-boundary)
  "\\else をインデントするために追加した変数")
(setq LaTeX-end-regexp (rx (or "end" "else" "fi") word-boundary)) ; これだけは元々auctexに存在


(defvar myext-auctex--tikz-commands
  '("path" "draw" "coordinate" "clip" "node" "pic" "useasboundingbox" "fill")
  "`myext-auctex-face-tikz-keyword' と `myext-auctex-indent-env--tikz-commands' で利用する")

(provide 'myext-auctex-base)
