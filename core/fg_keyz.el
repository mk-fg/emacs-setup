;; Terminal/X independent keybindings' translation
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

;; Had to leave C-S-NUM bindings reserved for the tentacled alien masters
(global-set-key (key "C-?") 'delete-window)
(global-set-key (key "C-/") 'delete-other-windows)
(global-set-key (key "C->") 'split-window-vertically)
(global-set-key (key "C-<") 'split-window-horizontally)

;; Tab cycling w/ status in minibuffer
(require 'wcy-swbuff)
(global-set-key (kbd "<C-tab>") 'wcy-switch-buffer-forward)
(global-set-key (kbd "<C-S-iso-lefttab>") 'wcy-switch-buffer-backward)
; (global-set-key (kbd "C-<tab>") 'bury-buffer)
; (global-set-key (kbd "C-S-<tab>") 'unbury-buffer)

;; Consistent undo/redo
;; C-z is outrighty dangerous otherwise!
(require 'redo) ; consistent redo, grabbed from XEmacs
;; TODO: bind deactivate-mark w/ let on undo/redo (try tabbing region, then undo)
(global-set-key (key "C-z") 'undo)
(global-set-key (key "C-S-z") 'redo)
(global-set-key (key "C-M-z") 'repeat)

;; Emacs' clipboard was designed by a bunch of certified lunatics
(global-set-key (key "C-c") 'clipboard-kill-ring-save)
; (global-set-key (key "C-x") 'clipboard-kill-region) ; I hate original binding for this key
(global-set-key (key "C-v") 'clipboard-yank)

;; Pitiful replacement for xterm but it'll have to do
(require 'multi-term)
(global-set-key (key "C-<return>") 'multi-term)

;; Line ops
(global-set-key (key "<delete>") 'delete-char) ; so it won't be aliased to C-d
(global-set-key (key "C-d") 'duplicate-line) ; fg_macro, ever-amazing emacs doesn't seem to have it!
(global-set-key (key "C-k") 'kill-whole-line)

;; File/buffer stuff
(global-set-key (key "C-S-c") 'save-buffers-kill-terminal)
(global-set-key (key "C-S-f") 'find-file)
(global-set-key (key "C-S-r") 'revert-buffer)
(global-set-key (key "C-S-s") 'save-buffer)

;; Make it scroll!
(mouse-wheel-mode t)

;; TAB is TAB, and backspace is a backspace, goddamnit!
(global-set-key (key "<backspace>") 'backward-delete-char)

(global-set-key (key "<tab>") 'smart-tab)
(global-set-key (key "<backtab>") 'smart-untab)
; (global-set-key (key "tab") 'self-insert-command)
