;;; Matches if???, where ??? is an almost arbitrary string.
;;; Doesn't match iff, since this is not an if-condition
(setq myext-auctex-base--if-regexp
      (rx (seq "if" (or ""
                        (>= 2 (char alpha "@"))
                        (char "a-eg-zA-Z@")))
          word-boundary))
;; (setq myext-auctex-base--begin-regexp "\\(?:begin\\|ifx\\|ifnum\\|ifdefined\\)\\b")
(setq myext-auctex-base--else-regexp (rx "else" word-boundary))
(setq LaTeX-end-regexp (rx (or "end" "else" "fi") word-boundary)) ; これだけは元々auctexに存在

(provide 'myext-auctex-base)
