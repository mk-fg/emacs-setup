(setq inferior-lisp-program "/usr/bin/sbcl")

;; Dunno what it's good for
; (require 'cl)

(require 'slime)
(slime-setup)

(add-to-list 'auto-mode-alist
	'("\\.cl$\\|.el$|\\.emacs$" . lisp-mode))
