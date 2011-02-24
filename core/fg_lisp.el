(require 'cl) ; superb stuff


;; Parenthesis editing helpers (TODO: MOAR!)
(when
	(load-library-safe "highlight-parentheses")
	(defun fg-lisp-setup () (highlight-parentheses-mode t))
	(add-hook 'lisp-interaction-mode-hook 'fg-lisp-setup)
	(add-hook 'lisp-mode-hook 'fg-lisp-setup)
	(add-hook 'scheme-mode-hook 'fg-lisp-setup))


(setq inferior-lisp-program "/usr/bin/sbcl")
(eval-after-load "slime" '(progn
	(slime-setup)
	(add-to-list 'auto-mode-alist
		'("\\.cl$" . lisp-mode))))

(add-to-list 'auto-mode-alist
	'("/\\.?emacs\\(\\.d/.*\\.el\\)?$" . lisp-interaction-mode))
