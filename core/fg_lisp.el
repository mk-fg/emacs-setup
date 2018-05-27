;; Basics

(setq-default
	eval-expression-print-length nil
	eval-expression-print-level nil)

(add-to-list 'auto-mode-alist
	'("/\\.?emacs\\(\\.d/.*\\.el\\)?$" . lisp-interaction-mode))


;; Parenthesis editing helpers (TODO: MOAR!)

(when
	(load-library-safe "highlight-parentheses")
	(defun fg-lisp-setup () (highlight-parentheses-mode t))
	(dolist
		(hook
			'(emacs-lisp-mode-hook lisp-mode-hook
				scheme-mode-hook lisp-interaction-mode-hook))
		(add-hook hook 'fg-lisp-setup)))


;; SLIME

(eval-after-load "slime" '(progn
	(setq inferior-lisp-program "/usr/bin/sbcl")
	(slime-setup)
	(add-to-list 'auto-mode-alist
		'("\\.cl$" . lisp-mode))))


;; TEH LISHP MUSHT FLOW...

(cl-defun fg-lisp-format (&key pp spaces)
	"Indent (default) or pretty-print (if PP is non-nil)
s-expressions in tainted region or whole buffer.
If SPACES is non-nil (number or t),
also force indentation level to be equal to specified number of spaces
(`tab-width' if t) and replace all tabs with these."
	(interactive)
	(multiple-value-bind (start end)
		(if (use-region-p)
			(fg-taint :whole-lines-only t)
			(list (point-min) (point-max)))
		(if (not spaces) (fg-lisp-format-region start end)
			(let ((end-marker (progn (goto-char end) (point-marker))))
				(when (eq spaces t) (setq spaces tab-width))
				(goto-char start)
				(let ((indent (make-string spaces ? )))
					(while (re-search-forward "^\t+" end-marker t) (replace-match
						(replace-regexp-in-string "\t" indent (match-string 0) t t))))
				(let
					((indent-tabs-mode nil)
						(tab-width spaces)
						(indent-region-function nil))
					(fg-lisp-format-region start (marker-position end-marker)))))))

(defun fg-lisp-format-region (start end)
	(goto-char start)
	(let (sexp-start sexp-end-marker sexp-end-last)
		(loop
			(forward-sexp 1)
			(setq sexp-end-marker (point-marker))
			(forward-sexp -1)
			(setq sexp-start (point))
			(when (<= sexp-end-marker (or sexp-end-last sexp-start))
				(when sexp-end-last (goto-char sexp-end-last))
				(return))
			(save-restriction
				(narrow-to-region sexp-start sexp-end-marker)
				(goto-char (point-min))
				(if pp (pp-buffer) (indent-sexp))
				(goto-char (point-max))
				(if (eq (char-before) ?\n) (delete-char -1)))
			(goto-char sexp-end-marker)
			(setq sexp-end-last (point)))))
