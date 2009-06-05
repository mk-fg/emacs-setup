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

;; Consistent undo/redo
;; C-z is outrighty dangerous otherwise
(global-set-key [?\C-z] 'undo)
(global-set-key [C-S-z] 'redo)
(global-set-key [?\C-x ?\C-z] 'repeat)
(global-set-key [C-return] 'multi-term)

;; Make the mouse wheel scroll Emacs
(mouse-wheel-mode t)

;; TAB is TAB, and backspace is a backspace, goddamnit!
(global-set-key [backspace] 'backward-delete-char)
(global-set-key (kbd "TAB") 'self-insert-command)

; (global-set-key (kbd "TAB") 'tab-to-tab-stop)
; (setq tab-stop-list '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30))
