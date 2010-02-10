;; TODO: add py-mode bindings (like eval-line, pi shell)
;; TODO: customize by-word jumps to stop at newlines, punctuation (aka SciTE)
;; TODO: add some-keyz + num for discrete buffer switching (Alt+NUM Alt+B)

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

(defun define-keys (mode defs)
	(dolist (def defs)
		(multiple-value-bind (keydef sym) def
			(define-key mode (key keydef) sym))))

(defun global-set-keys (defs)
	(dolist (def defs)
		(multiple-value-bind (keydef sym) def
			(global-set-key (key keydef) sym))))



;; Includes
(require 'setnu) ; line numbers mode
(require 'wcy-swbuff) ; buffer cycling thru minibuff
(require 'redo) ; consistent redo, grabbed from XEmacs
(require 'multi-term) ; improved ansi-term


(global-set-keys
	;; Mode setting globals
	'(("M-/" fg-scite-code)
		("M-?" fg-scite-lisp)
		("M-'" fg-scite-aux)
		("M-]" fg-scite-core)
		("C-?" setnu-mode)
	;; Lookz switching should work everywhere as well
		("C-M-/" fg-masq-x-light)
		("C-M-'" fg-masq-x-dark)
		("C-M-]" fg-masq-x-pitch)))


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
		(,(key "M-F") . fg-recentf-prompt)
		(,(key "C-M-f") . recentf-open-files)
		(,(key "M-r") . revert-buffer)
		(,(key "M-s") . save-buffer)
		(,(key "M-S") . write-file)

		;; -- EMMS controls --
		;; TODO: Move optional sections like this one to separate define-keys
		;;  structure w/ exception handling wrapper to handle no-emms case
		(,(key "s-'") . emms-next)
		(,(key "s-;") . emms-previous)
		(,(key "s-:") . emms-stop)
		(,(key "s-\"") . emms-pause)
		(,(key "<XF86AudioPlay>") . emms-pause)

		;; -- History --
		;; Consistent undo/redo
		;; C-z is dangerous otherwise!
		(,(key "C-z") . ,(transient-wrap 'undo "p"))
		(,(key "C-S-z") . ,(transient-wrap 'redo "p"))
		(,(key "M-z") . repeat)

		;; -- Invokation --
		;; Pitiful replacement for xterm but it'll have to do...
		;; (,(key "C-<return>") . multi-term)
		(,(key "C-S-<return>") . multi-term))
	:group 'fg-scite)



(define-minor-mode fg-scite-term
	"SciTE-like terminal mode bindings."
	:init-value nil
	:lighter "/t"
	:keymap `(
		;; Overrides for stupid emacs keyz
		(,(key "C-n") . term-send-raw)
		(,(key "C-c") . term-send-raw)
		(,(key "C-x") . term-send-raw)
		(,(key "C-z") . term-send-raw)
		(,(key "C-r") . term-send-raw)
		(,(key "ESC") . term-send-raw)
		(,(key "C-<return>") . ,(iwrap 'term-send-raw-string (make-string 1 13))) ; for nano

		;; Terminal-safe block-skimming
		(,(key "C-<left>") . ,(iwrap 'term-send-raw-string ";5D"))
		(,(key "C-<right>") . ,(iwrap 'term-send-raw-string ";5C"))
		(,(key "C-S-<left>") . ,(iwrap 'term-send-raw-string ";6D"))
		(,(key "C-S-<right>") . ,(iwrap 'term-send-raw-string ";6C"))

		(,(key "<C-tab>") . multi-term-next)
		(,(key "<C-S-iso-lefttab>") . multi-term-prev))
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
		(,(key "C-c") . fg-copy)
		(,(key "C-S-c") . fg-copy-paragraph)
		(,(key "C-x") . fg-kill) ; I hate original binding for this key
		(,(key "C-S-x") . fg-kill-whole-paragraph)
		(,(key "C-v") . yank)
		(,(key "C-S-v") . yank-pop)

		;; Register set/jump
		(,(key "C-<f2>") . fg-point-to-reg)
		(,(key "<f2>") . fg-point-from-reg)
		(,(key "M-<f2>") . fg-point-from-reg)

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
		(,(key "M-U") . ,(transient-wrap 'capitalize-region "r"))
		(,(key "M-l") . ,(transient-wrap 'downcase-region "r"))
		(,(key "C-/") . ,(transient-wrap 'fg-comment "P"))

		;; Skimming ops
		(,(key "<f3>") . isearch-repeat-forward)
		(,(key "C-s") . isearch-forward)
		(,(key "C-S-s") . query-replace)
		(,(key "C-M-s") . replace-regexp)

		;; Metabuffer stuff
		(,(key "C-=") . compare-windows)
		(,(key "M-v") . clone-buffer)
		(,(key "M-<return>") . fg-wtf)
		(,(key "C-<mouse-1>") . ffap-at-mouse)

		;; Lookz
		(,(key "M-w") . ,(iwrap '(setq truncate-lines (not truncate-lines))))

		;; Metacode ops (emacs stuff)
		(,(key "C-j") . eval-last-sexp) ; > minibuffer
		(,(key "C-S-j") . eval-print-last-sexp))
	:group 'fg-scite)
(set-keymap-parent fg-scite-code-map fg-scite-aux-map)



(define-minor-mode fg-scite-lisp
	"LI+SLiME overlay mode, not suited for REPL buffer."
	:init-value nil
	:lighter "/L"
	:keymap `(
		(,(key "<M-S-iso-lefttab>") . slime-switch-to-output-buffer)
		;; MacroDebugging ops
		(,(key "C-i") . slime-interrupt)
		(,(key "C-S-i") . slime-toggle-trace-fdefinition)
		(,(key "M-i") . slime-macroexpand-1)
		(,(key "M-I") . slime-undefine-function)
		(,(key "C-f") . slime-load-file)
		(,(key "C-S-f") . slime-sync-package-and-default-directory)
		(,(key "M-X") . slime-export-symbol-at-point)
		;; Code inspection
		(,(key "C-;") . slime-list-callers)
		(,(key "C-:") . slime-list-callees)
		(,(key "M-;") . slime-inspect)
		(,(key "C-'") . slime-edit-definition)
		(,(key "C-S-'") . slime-edit-value)
		;; Code evaluation
		(,(key "C-j") . slime-eval-last-expression)
		(,(key "C-S-j") . slime-eval-region)
		(,(key "M-j") . slime-call-defun)
		(,(key "M-J") . slime-compile-defun) ;; not really useful
		(,(key "C-M-j") . slime-pprint-eval-last-expression)
		(,(key "C-M-J") . slime-compile-and-load-file)
		;; Syntax helpers
		(,(key "C-)") . slime-close-all-parens-in-sexp)
		(,(key "C-h C-d") . slime-doc-map)
		(,(key "C-h C-e") . slime-presentation-map)
		(,(key "C-h C-w") . slime-who-map))
	:group 'fg-scite)
(set-keymap-parent fg-scite-lisp-map fg-scite-code-map)



(define-minor-mode fg-scite-pair
	"Parenthesis auto-insert mode, not really useful for lisp."
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



;; -- ISearch/replace mangling --
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

(define-keys isearch-mode-map
	'(("C-<home>" fg-isearch-beginning-of-buffer)
		("C-<end>" fg-isearch-end-of-buffer)

		("<f3>" isearch-repeat-forward)
		("C-s" isearch-repeat-forward)
		("C-S-s" isearch-query-replace)
		("C-M-s" isearch-query-replace-regexp)
		("C-S-h" isearch-occur)
		("C-M-h" isearch-highlight-regexp)

		("C-<left>" isearch-repeat-backward)
		("C-<right>" isearch-repeat-forward)
		("C-<up>" isearch-ring-advance)
		("C-<down>" isearch-ring-retreat)

		("C-w" isearch-delete-char)
		("C-v" isearch-yank-kill)
		("C-S-v" isearch-yank-word)))

(define-keys query-replace-map
	'(("C-<left>" backup)
		("C-<right>" skip)
		("C-<return>" automatic)))


;; -- IBuffer mangling --
(require 'ibuffer)
(defun fg-ibuffer-mark (&optional move)
  "Mark the buffer on this line (or ARG lines), w/o moving the point.
If point is on a group name, this function operates on that group."
	(if
		(looking-at "^>")
		(ibuffer-unmark-forward nil)
		(ibuffer-mark-forward nil))
	(unless move (forward-line -1)))

(define-keys ibuffer-mode-map
	`(("SPC" ,(iwrap 'fg-ibuffer-mark nil))
		("<insert>" ,(iwrap 'fg-ibuffer-mark t))))


;; -- Jabbra submode --
(require 'jabber)
(define-keys jabber-roster-mode-map
	'(("v" jabber-vcard-get)
		("V" jabber-get-version)
		("m" jabber-send-message)))
(define-keys jabber-chat-mode-map
	'(("<return>" fg-newline)
		("C-<return>" jabber-chat-buffer-send)))


;; -- Auto mode-switching --
(defun fg-hook-set-mode ()
	"Turn fg-scite-* minor modes, depending on major."
	;; (message "%s, %s, %s" major-mode buffer-file-name (buffer-name))
	(if buffer-file-name ; nil for system buffers and terminals
		(cond
			((eq major-mode 'lisp-mode)
				(fg-scite-lisp t))
			(t (fg-scite-code t))) ; if it's a file, then it's at least code
		(cond
			((eq major-mode 'term-mode) ; term-mode minors should probably be set via multi-term hooks
				(fg-scite-term t))
			((eq major-mode 'lisp-interaction-mode) ; *scratch*
				(fg-scite-code t))
			((or (eq major-mode 'help-mode)
					(eq major-mode 'slime-repl-mode)) ; TODO: special mode for REPL (what for?)
				(fg-scite-aux t)))))

(add-hook 'jabber-chat-mode-hook 'fg-scite-aux)
(add-hook 'minibuffer-setup-hook 'fg-scite-aux)
(add-hook 'find-file-hook 'fg-hook-set-mode)
(add-hook 'after-change-major-mode-hook 'fg-hook-set-mode)
