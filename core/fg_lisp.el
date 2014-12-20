(require 'cl) ; superb stuff


;; Parenthesis editing helpers (TODO: MOAR!)
(when
	(load-library-safe "highlight-parentheses")
	(defun fg-lisp-setup () (highlight-parentheses-mode t))
	(dolist
		(hook
			'(emacs-lisp-mode-hook lisp-mode-hook
				scheme-mode-hook lisp-interaction-mode-hook))
		(add-hook hook 'fg-lisp-setup)))


(setq inferior-lisp-program "/usr/bin/sbcl")
(eval-after-load "slime" '(progn
	(slime-setup)
	(add-to-list 'auto-mode-alist
		'("\\.cl$" . lisp-mode))))

(add-to-list 'auto-mode-alist
	'("/\\.?emacs\\(\\.d/.*\\.el\\)?$" . lisp-interaction-mode))
