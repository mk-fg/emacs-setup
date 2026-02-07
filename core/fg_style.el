;; Tabs appearance and formatting basics
(setq-default
	indent-tabs-mode t
	tab-width 2
	indent-region-function
		'fg-indent-command ; default one inserts spaces
	require-final-newline t
	tab-always-indent t ; overidden by fg-tab
	fill-column 80

	py-indent-offset 2
	python-indent 2
	;; Following opts disable spaces/tab flip in py buffers
	python-indent-guess-indent-offset nil
	python-guess-indent nil

	c-syntactic-indentation nil ; stupid ten-tabs indentation
	lua-electric-flag nil) ;; disable indents on closing brackets

;; Develock mode long-line highlight overrides
(let ((develock-mcs develock-max-column-plist))
	(dolist
		(mode '(c-mode c++-mode html-mode html-helper-mode))
		(plist-put develock-mcs mode 99))
	(customize-set-variable 'develock-max-column-plist develock-mcs))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use // instead of /* */ comments in C - doesn't work like this
;; (eval-after-load "cc-mode" '(c-toggle-comment-style -1))

;; Try to disable as much of "electric" stuff as possible
(add-hook 'after-change-major-mode-hook #'(lambda () (electric-indent-mode -1)))

;; Adjust some develock line lengths
(eval-after-load "develock"
	(dolist (mode '(emacs-lisp-mode html-mode))
		(plist-put develock-max-column-plist mode 99)))

;; Make absolutely sure python-mode uses tabs
(add-hook 'python-mode-hook
	#'(lambda ()
		(setq tab-width 2 indent-tabs-mode t)
		(defun python--f-string-p (ppss) nil))) ;; f-string parsing is slow and not very useful

;; Short-circuit stupid indentation hacks in newer python.el
(eval-after-load "python" '(progn
	(defun python-indent-post-self-insert-function () nil)))

(add-hook 'tuareg-mode-hook #'(lambda () (setq indent-tabs-mode t)))
(add-hook 'markdown-mode-hook #'(lambda () (setq tab-width 2)))
