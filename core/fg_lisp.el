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

(cl-defmacro fg-lisp-format-func (&key type fill)
	"Make interactive function to format s-expressions
using `fg-lisp-format' with number of SPACES as interactive argument."
	`(lambda (arg)
		(interactive "P")
		(fg-lisp-format :type ,type :fill ,fill
			:spaces (when arg (if (numberp arg) arg t)))))

(cl-defun fg-lisp-format (&key type spaces fill)
	"Format s-expressions in tainted region or whole buffer.
Default mode is to just auto-indent them.

TYPE can be one of the following symbols:
- nil - auto-indent using `indent-sexp' (emacs).
- pp - pretty-print s-exps using `pp-buffer' (emacs).
- lispy-oneline - format using `lispy-oneline' (lispy).
- lispy-multiline - format using `fg-lispy-multiline' (lispy).

If SPACES is non-nil (number or t),
also force indentation level to be equal to specified number of spaces
(`tab-width' if t) and replace all tabs with these.

FILL is only accepted as t and passed
as 'fill argument to `lispy-multiline' at the moment."
	(multiple-value-bind (start end)
		(if (use-region-p)
			(fg-taint :whole-lines-only t)
			(list (point-min) (point-max)))
		(let
			((func (pcase type
				('nil #'indent-sexp)
				('pp #'pp-buffer)
				('lispy-oneline (require 'lispy) #'lispy-oneline)
				('lispy-multiline (require 'lispy)
					(apply-partially #'fg-lispy-multiline (and fill 'fill)))
				(- (error (format "Unrecognized fg-lisp-format type: %s" type))))))
			(if (not spaces) (fg-lisp-format-region func start end)
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
						(fg-lisp-format-region func start (marker-position end-marker))))))))

(defun fg-lisp-format-region (func start end)
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
				(funcall func)
				(goto-char (point-max))
				(if (eq (char-before) ?\n) (delete-char -1)))
			(goto-char sexp-end-marker)
			(setq sexp-end-last (point)))))

(defun fg-lispy-multiline (&optional arg)
	"Spread current sexp over multiple lines.
When ARG is `fill', do nothing for short expressions.
Copy of `lispy-multiline' from 20180516.826,
but with added `fill-column' check on splitting at `lispy-flow' with 'fill ARG."
	(interactive "p")
	(require 'lispy)
	(unless (or (lispy-left-p) (lispy-right-p)) (lispy--out-backward 1))
	(lispy-from-left
		(let*
			((bnd (lispy--bounds-list))
				(str (lispy--string-dwim bnd))
				(plain-expr (read str))
				(expr (lispy--read str))
				res)
			(unless (listp plain-expr) (setq plain-expr nil))

			(if
				(or (cl-some #'listp plain-expr)
					(member '(ly-raw newline) expr))

				(let ((pt (point)))
					(lispy-forward 1)
					(when
						(while (and (lispy-flow 1) (> (point) pt))
							(unless
								(or (looking-at "\]\\|)\\|\n")
									(and (eq arg 'fill)
										(<= (- (line-end-position) (line-beginning-position)) fill-column)))
								(when (looking-at " *")
									(replace-match "\n")
									(backward-char 1)))))
					(goto-char pt)
					(indent-sexp))

				(progn
					(delete-region (car bnd) (cdr bnd))
					(setq res
						(butlast (cl-mapcan
							(lambda (y)
								(if (memq y '(ly-raw clojure-map clojure-set))
									(list y) (list y '(ly-raw newline))))
							(lispy--read str))))
					(when (vectorp expr) (setq res (apply #'vector res)))
					(lispy--insert res))))))
