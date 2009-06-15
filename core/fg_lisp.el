(require 'cl) ; superb stuff
(setq inferior-lisp-program "/usr/bin/sbcl")

(require 'slime)
(slime-setup)

(add-to-list 'auto-mode-alist
	'("\\.cl$" . lisp-mode))
(add-to-list 'auto-mode-alist
	'("/\\.?emacs\\(\\.d/.*\\.el\\)?$" . lisp-interaction-mode))
