;;; Matches if???, where ??? is an almost arbitrary string.
;;; Doesn't match iff, since this is not an if-condition
(setq LaTeX-if-regexp
      (rx (seq "if" (or ""
                        (>= 2 (char alpha "@"))
                        (char "a-eg-zA-Z@")))
          word-boundary))
;; (setq LaTeX-begin-regexp "\\(?:begin\\|ifx\\|ifnum\\|ifdefined\\)\\b")
(setq LaTeX-else-regexp (rx "else" word-boundary))
(setq LaTeX-end-regexp (rx (or "end" "else" "fi") word-boundary))

(provide 'myext-auctex-base)
