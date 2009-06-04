;; Terminal keybindings' translation
(defvar real-keyboard-keys
	'(("M-<up>"				. "\M-[1;3A")
		("M-<down>"			. "\M-[1;3B")
		("M-<right>"		 . "\M-[1;3C")
		("M-<left>"			. "\M-[1;3D")
		("C-<return>"		. "\C-j")
		("C-<delete>"		. "\M-[3;5~")
		("C-<up>"				. "\M-[1;5A")
		("C-<down>"			. "\M-[1;5B")
		("C-<right>"		 . "\M-[1;5C")
		("C-<left>"			. "\M-[1;5D"))
	"An assoc list of pretty key strings and their terminal equivalents.")

(defun key (desc)
	(or (and window-system (read-kbd-macro desc))
		(or (cdr (assoc desc real-keyboard-keys))
			(read-kbd-macro desc))))


;; Flux-style pane glide
(global-set-key (key "M-<left>") 'windmove-left)
(global-set-key (key "M-<right>") 'windmove-right)
(global-set-key (key "M-<up>") 'windmove-up)
(global-set-key (key "M-<down>") 'windmove-down)

;; C-z is outrighty dangerous otherwise
(global-set-key [?\C-z] 'multi-term)

;; Make the mouse wheel scroll Emacs
(mouse-wheel-mode t)


(defun indent-rigidly (start end arg)
	"Indent all lines starting in the region sideways by ARG columns.
Called from a program, takes three arguments, START, END and ARG.
You can remove all indentation from a region by giving a large negative ARG."
	(interactive "r\np")
	(save-excursion
		(goto-char end)
		(setq end (point-marker))
		(goto-char start)
		(or (bolp) (forward-line 1))
		(while (< (point) end)
			(let ((indent (current-indentation))
			eol-flag)
	(save-excursion
		(skip-chars-forward " \t")
		(setq eol-flag (eolp)))
	(or eol-flag
			(indent-to (max 0 (+ indent arg)) 0))
	(delete-region (point) (progn (skip-chars-forward " \t") (point))))
			(forward-line 1))
		(move-marker end nil)))


;; TAB is TAB, and backspace is a backspace, goddamnit!
; (global-set-key (kbd "TAB") 'self-insert-command)
(global-set-key [backspace] 'backward-delete-char)


(global-set-key (kbd "TAB") 'insert-tab)
