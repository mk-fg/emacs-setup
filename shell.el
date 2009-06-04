;; Shell tuning (DEPRECATED: use ansi-term instead)
; (add-hook 'term-mode-hook (lambda () (buffer-face-set "shadow")))

(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(setq multi-term-scroll-show-maximum-output t)

;; Add any face changes here
; (add-hook 'term-mode-hook (lambda ()
	; (set-face-background 'default "red")))
