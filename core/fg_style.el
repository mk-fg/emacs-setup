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

;; Use // instead of /* */ comments in C - doesn't work like this
;; (eval-after-load "cc-mode" '(c-toggle-comment-style -1))

;; Try to disable as much of "electric" stuff as possible
(add-hook 'after-change-major-mode-hook
	#'(lambda () (electric-indent-mode -1)))

;; Adjust some develock line lengths
(eval-after-load "develock"
	(dolist (mode '(emacs-lisp-mode html-mode))
		(plist-put develock-max-column-plist mode 99)))

;; Make absolutely sure python-mode uses tabs
(add-hook 'python-mode-hook
	#'(lambda () (setq tab-width 2 indent-tabs-mode t)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Short-circuit stupid indentation hacks in newer python.el
(eval-after-load "python" '(progn
	(defun python-indent-post-self-insert-function () nil)))

;; tuareg-mode hates tabs
(add-hook 'tuareg-mode-hook
	#'(lambda () (setq indent-tabs-mode t)))

;; God I hate php, but sometimes they force me to use it
;; TODO: regional action, look at fg-comment
(defun* fg-php-tag-line (&key force-insert force-strip)
	"Surround current php code line in output tags (?>stuff<?),
if FORCE-INSERT is set, or unless it's already surrounded,
otherwise (or if FORCE-STRIP is set) strip these tags from line."
	(interactive)
	(save-excursion
		(let*
			((tag-beginning
				(progn (fg-beginning-of-line t)
					(let ((pt (point))) (string= (buffer-substring pt (min (+ pt 2) (point-max))) "?>"))))
				(tag-end
					(progn (fg-end-of-line)
						(let ((pt (point))) (string= (buffer-substring (max (- pt 2) (point-min)) pt) "<?"))))
				(surrounded (unless force-strip (and (not force-insert) tag-beginning tag-end))))
			;; (message "D %s %s %s" surrounded tag-beginning tag-end)
			(if surrounded
				(progn
					(when tag-beginning (fg-beginning-of-line t) (delete-char 2))
					(when tag-beginning (fg-end-of-line) (delete-char -2)))
				(progn
					(unless tag-beginning (fg-beginning-of-line t) (insert "?>"))
					(unless tag-end (fg-end-of-line) (insert "<?")))))))
