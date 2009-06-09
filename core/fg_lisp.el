(require 'cl)
(setq inferior-lisp-program "/usr/bin/sbcl")

(require 'slime)
(slime-setup)

(add-to-list 'auto-mode-alist
	'("\\.cl$\\|.el$|\\.emacs$" . lisp-mode))
