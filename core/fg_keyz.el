;; Key translation table and wrappers
(defvar fg-dict-keys
	'(("M-<up>" . "\M-[1;3A")
		("M-<down>" . "\M-[1;3B")
		("M-<right>" . "\M-[1;3C")
		("M-<left>" . "\M-[1;3D")
		("C-<return>" . "\C-j")
		("C-<delete>" . "\M-[3;5~")
		("C-<up>" . "\M-[1;5A")
		("C-<down>" . "\M-[1;5B")
		("C-<right>" . "\M-[1;5C")
		("C-<left>" . "\M-[1;5D"))
	"An alist of pretty key strings, and their terminal equivalents.")

(defun key (desc)
	"Return env-dependant (X / term) key sequence."
	(or (and window-system (read-kbd-macro desc))
		(or (cdr (assoc desc fg-dict-keys))
			(read-kbd-macro desc))))

(defun transient-wrap (func &optional mode)
	"Execute FUNC w/o deactivating mark.
MODE is argument presentation mode (see `interactive').
Not all modes are handled correctly (tested w/ p and r only)."
	(let
		((args
			(if (and mode (string-match "r" mode))
				'(arg1 arg2)
				'(arg))))
		`(lambda (&optional ,@args)
				(interactive ,mode)
				(let (deactivate-mark) (,func ,@args)))))

(defun* iwrap (func &optional (arg 'undefined)) ; default val to check if arg is supplied, CL
	"Return interactive-wrapped FUNC, called w/ ARG, if specified."
	(let ((func (if (not (eq arg 'undefined)) `(,func ,arg) func)))
		`(lambda () (interactive) ,func)))


;; Includes
(require 'setnu) ; line numbers mode
(require 'wcy-swbuff) ; buffer cycling thru minibuff
(require 'redo) ; consistent redo, grabbed from XEmacs
(require 'multi-term) ; improved ansi-term


;; Mode setting globals
(global-set-key (key "M-/") 'fg-scite-code)
(global-set-key (key "M-'") 'fg-scite-aux)
(global-set-key (key "M-]") 'fg-scite-core)
(global-set-key (key "C-?") 'setnu-mode)

;; Lookz switching should work everywhere as well
(global-set-key (key "C-M-/") 'fg-masq-x-light)
(global-set-key (key "C-M-'") 'fg-masq-x-dark)
(global-set-key (key "C-M-]") 'fg-masq-x-pitch)


(define-minor-mode fg-scite-core
	"SciTE-like keybindings (cross-buffer core).
Keymap of this mode is used as a parent for the rest of fg-scite modes."
	:init-value t
	:lighter "/-"
	:keymap `(
		;; -- Movement --
		;; Basic ops
		(,(key "M-`") . universal-argument)
		(,(key "<home>") . fg-beginning-of-line)
		(,(key "<end>") . move-end-of-line) ; vanilla works just fine

		;; -- Frame controls --
		;; Flux-style pane glide
		(,(key "M-<left>") . windmove-left)
		(,(key "M-<right>") . windmove-right)
		(,(key "M-<up>") . windmove-up)
		(,(key "M-<down>") . windmove-down)

		;; Had to leave C-S-NUM bindings reserved for the tentacled aliens from outta space
		(,(key "C-,") . split-window-horizontally)
		(,(key "C-.") . split-window-vertically)
		(,(key "C-<") . delete-window)
		(,(key "C->") . delete-other-windows)
		(,(key "M-,") . ,(iwrap 'kill-buffer nil)) ; kill current buffer w/o asking
		;; (,(key "M-.") . split-window-vertically)

		;; Tab cycling w/ status in minibuffer
		(,(key "<C-tab>") . wcy-switch-buffer-forward)
		(,(key "<C-S-iso-lefttab>") . wcy-switch-buffer-backward)
		(,(key "<M-tab>") . ibuffer)

		;; -- File/buffer stuff --
		(,(key "M-c") . save-buffers-kill-terminal)
		(,(key "M-b") . switch-to-buffer)
		(,(key "M-f") . find-file)
		(,(key "M-r") . revert-buffer)
		(,(key "M-s") . save-buffer)

		;; -- History --
		;; Consistent undo/redo
		;; C-z is outrighty dangerous otherwise!
		(,(key "C-z") . ,(transient-wrap 'undo "p"))
		(,(key "C-S-z") . ,(transient-wrap 'redo "p"))
		(,(key "M-z") . repeat)

		;; -- Invokation --
		;; Pitiful replacement for xterm but it'll have to do...
		(,(key "C-<return>") . multi-term))
	:group 'fg-scite)



(define-minor-mode fg-scite-term
	"SciTE-like terminal mode bindings."
	:init-value nil
	:lighter "/t"
	:keymap `(
		;; Terminal-safe block-skimming
		(,(key "C-<left>") . ,(iwrap 'term-send-raw-string ";5D"))
		(,(key "C-<right>") . (iwrap 'term-send-raw-string ";5C"))
		(,(key "C-S-<left>") . ,(iwrap 'term-send-raw-string ";6D"))
		(,(key "C-S-<right>") . ,(iwrap 'term-send-raw-string ";6C")))
	:group 'fg-scite)
(set-keymap-parent fg-scite-term-map fg-scite-core-map)



(define-minor-mode fg-scite-aux
	"SciTE-like editing extras (minibuffer-compatible)."
	:init-value nil
	:lighter "/x"
	:keymap `(
		;; These don't play nice w/ terminals
		(,(key "<backspace>") . fg-del-char-backwards)
		(,(key "<delete>") . fg-del-char)
		
		;; Emacs' clipboard was designed by a bunch of certified lunatics ;)
		;; TODO: make sensible ring-buffer controls, so older entries are accessible as well
		(,(key "C-c") . fg-copy)
		(,(key "C-S-c") . fg-copy-paragraph)
		(,(key "C-x") . fg-kill) ; I hate original binding for this key
		(,(key "C-S-x") . fg-kill-whole-paragraph)
		(,(key "C-v") . yank)
		(,(key "C-S-v") . yank-pop)

		;; Line/word ops
		(,(key "C-d") . fg-clone)
		(,(key "C-k") . kill-whole-line)
		(,(key "C-S-k") . fg-del-whole-line)
		(,(key "C-w") . fg-del-word-backwards)
		(,(key "C-u") . fg-kill-line-blank)
		(,(key "C-S-u") . fg-kill-line-backwards)
		(,(key "C-M-u") . fg-kill-line))
	:group 'fg-scite)
(set-keymap-parent fg-scite-aux-map fg-scite-core-map)



(define-minor-mode fg-scite-code
	"SciTE-like full keybindings set (coding extension)."
	:init-value nil
	:lighter "/+"
	:keymap `(
		;; Basic ops
		(,(key "<return>") . fg-newline)
		(,(key "<tab>") . fg-tab)
		(,(key "<backtab>") . fg-untab)
		(,(key "<prior>") . fg-scroll-up) ; pageup
		(,(key "C-<prior>") . ,(iwrap 'move-to-window-line 0))
		(,(key "<next>") . fg-scroll-down) ; pagedown
		(,(key "C-<next>") . ,(iwrap 'move-to-window-line -1))

		;; Block-skimming (emacs' re-definition, jic)
		(,(key "C-<left>") . backward-word)
		(,(key "C-<right>") . forward-word)
		(,(key "C-<up>") . backward-paragraph)
		(,(key "C-<down>") . forward-paragraph)
		(,(key "C-M-<left>") . backward-sexp)
		(,(key "C-M-<right>") . forward-sexp)
		(,(key "C-M-<up>") . up-list)
		(,(key "C-M-<down>") . down-list)

		;; Region ops
		(,(key "M-u") . ,(transient-wrap 'upcase-region "r"))
		(,(key "M-S-u") . ,(transient-wrap 'capitalize-region "r"))
		(,(key "M-l") . ,(transient-wrap 'downcase-region "r"))
		(,(key "C-/") . ,(transient-wrap 'fg-comment "P"))

		;; Skimming ops
		(,(key "<f3>") . isearch-repeat-forward)
		(,(key "C-s") . isearch-forward)
		(,(key "C-S-s") . query-replace)
		(,(key "C-M-s") . replace-regexp)

		;; File/buffer stuff
		(,(key "M-c") . save-buffers-kill-terminal)
		(,(key "M-b") . switch-to-buffer)
		(,(key "M-f") . find-file)
		(,(key "M-r") . revert-buffer)
		(,(key "M-s") . save-buffer)

		;; Metabuffer stuff
		(,(key "C-=") . compare-windows)
		(,(key "M-v") . clone-buffer)
		(,(key "M-<return>") . fg-wtf)
		(,(key "C-<mouse-1>") . ffap-at-mouse)

		;; Lookz
		(,(key "M-w") . ,(iwrap '(setq truncate-lines (not truncate-lines)))))
	:group 'fg-scite)
(set-keymap-parent fg-scite-code-map fg-scite-aux-map)



(define-minor-mode fg-scite-pair
	"Parenthsis auto-insert, feels weird, but..."
	:init-value nil
	:lighter "/p"
	:keymap `(
		(,(key "(") . insert-pair)
		(,(key "[") . insert-pair)
		(,(key "{") . insert-pair)
		(,(key "`") . insert-pair)
		(,(key "'") . insert-pair)
		(,(key "\"") . insert-pair))
	:group 'fg-scite)



;; -- Some isearch mangling --
(defun fg-isearch-beginning-of-buffer ()
  "Move isearch point to the beginning of the buffer."
  (interactive)
  (goto-char (point-min))
  (isearch-repeat-forward))

(defun fg-isearch-end-of-buffer ()
  "Move isearch point to the end of the buffer."
  (interactive)
  (goto-char (point-max))
  (isearch-repeat-backward))

;; TODO: convert this crap to skip PgUp / PgDn
;; (defadvice isearch-update (before fg-isearch-update-noexit activate)
;; 	(sit-for 0)
;; 	(when
;; 		(and
;; 			(not (eq this-command 'isearch-other-control-char)) ; not the scrolling command
;; 			(> (length isearch-string) 0) ; not the emptry string
;; 			(> (length isearch-cmds) 2) ; not the first key (to lazy highlight all matches w/o recenter)
;; 			(let
;; 				((line (count-screen-lines (point)
;; 					(save-excursion (move-to-window-line 0) (point)))))
;; 				(or (> line (* (/ (window-height) 4) 3))
;; 					(< line (* (/ (window-height) 9) 1))))) ; the point is within the given window boundaries
;; 		(let ((recenter-position 0.3))
;; 			(recenter '(4)))))

(define-key isearch-mode-map (key "C-<home>") 'fg-isearch-beginning-of-buffer)
(define-key isearch-mode-map (key "C-<end>") 'fg-isearch-end-of-buffer)

(define-key isearch-mode-map (key "<f3>") 'isearch-repeat-forward)
(define-key isearch-mode-map (key "C-s") 'isearch-repeat-forward)
(define-key isearch-mode-map (key "C-S-s") 'isearch-query-replace)
(define-key isearch-mode-map (key "C-M-s") 'isearch-query-replace-regexp)
(define-key isearch-mode-map (key "C-S-h") 'isearch-occur)
(define-key isearch-mode-map (key "C-M-h") 'isearch-highlight-regexp)

(define-key isearch-mode-map (key "C-<left>") 'isearch-repeat-backward)
(define-key isearch-mode-map (key "C-<right>") 'isearch-repeat-forward)
(define-key isearch-mode-map (key "C-<up>") 'isearch-ring-advance)
(define-key isearch-mode-map (key "C-<down>") 'isearch-ring-retreat)

(define-key isearch-mode-map (key "C-v") 'isearch-yank-word-or-char)
(define-key isearch-mode-map (key "C-S-v") 'isearch-yank-line)



;; -- Compatibility hooks --
(add-hook 'minibuffer-setup-hook 'fg-scite-aux)
;; Code buffers
(defun fg-hook-set-mode ()
	"Turn fg-scite-code mode on explicitly"
	(if buffer-file-name ; nil for system buffers and terminals
		(unless fg-scite-code
			(fg-scite-code))
		(and ; term-mode minors can probably also be set via multi-term
			;; (string=
			;; 	(symbol-name major-mode)
			;; 	"term-mode")
			(eq major-mode 'term-mode)
			(null fg-scite-term)
			(fg-scite-term))))
(add-hook 'find-file-hook 'fg-hook-set-mode)
(add-hook 'after-change-major-mode-hook 'fg-hook-set-mode)
