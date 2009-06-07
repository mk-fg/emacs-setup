;; Terminal/X independent keybindings' translation
;; TODO: Make single symbol table for key - func binding instead of global-set-key
;; TODO: Make decorator w/ embedded save-excursion
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


;; Basic ops
; (global-set-key (key "<return>") 'newline-and-indent) ; TODO: MAKE IT WORK!
(global-set-key (key "<backspace>") 'fg-del-char-backwards)
(global-set-key (key "<delete>") 'fg-del-char)
(global-set-key (key "<tab>") 'fg-tab)
(global-set-key (key "<backtab>") 'fg-untab)
(global-set-key (key "<prior>") 'fg-page-up)
(global-set-key (key "<next>") 'fg-page-down)
(global-set-key (key "<home>") 'fg-beginning-of-line)

; (global-set-key (key "S-<prior>") 'backward-page) ; TODO: make it via motion-n-select decorator (advice)
; (global-set-key (key "S-<next>") 'forward-page)


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
;; TODO: Add buffer filtering (only *scratch* and *msgz* form sys-buffz)
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
;; TODO: bind some key to fg-copy-line
(global-set-key (key "C-c") 'fg-copy-region)
; (global-set-key (key "C-x") 'kill-region) ; I hate original binding for this key
(global-set-key (key "C-v") 'yank)

;; Pitiful replacement for xterm but it'll have to do
(require 'multi-term)
(global-set-key (key "C-<return>") 'multi-term)

;; Line/word ops
(global-set-key (key "C-d") 'fg-clone) ; fg_macro, ever-amazing emacs doesn't seem to have it!
(global-set-key (key "C-k") 'kill-whole-line)
(global-set-key (key "C-w") 'fg-del-word-backwards)
(global-set-key (key "C-u") 'fg-kill-line-blank)
(global-set-key (key "C-S-u") 'fg-kill-line-backwards)

;; File/buffer stuff
(global-set-key (key "C-S-c") 'save-buffers-kill-terminal)
(global-set-key (key "C-S-f") 'find-file)
(global-set-key (key "C-S-r") 'revert-buffer)
(global-set-key (key "C-S-s") 'save-buffer)

;; Make it scroll!
(mouse-wheel-mode t)
