;; TODO: add some-keyz + num for discrete buffer switching (Alt+NUM Alt+B)

(defmacro iwrapm (func &rest args)
	"Return interactive-wrapped FUNC, called w/ ARGS, if specified."
	`(lambda () (interactive) (,func ,@args)))

(defun iwrap (func &rest args)
	"Return interactive-wrapped FUNC, called w/ ARGS, if specified."
	`(lambda () (interactive) (,func ,@args)))

(defun key (desc)
	"Return env-dependant (X / term) key sequence.
Obsolete functionality, since keys in newer emacs translate fine in all kinds of terminals."
	(read-kbd-macro desc))

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

(defun define-keys (mode defs)
	(dolist (def defs)
		(multiple-value-bind (keydef sym) def
			(define-key mode (key keydef) sym))))

(defun global-set-keys (defs)
	(dolist (def defs)
		(multiple-value-bind (keydef sym) def
			(global-set-key (key keydef) sym))))



;; Includes
(require 'redo) ; consistent redo, grabbed from XEmacs
(autoload 'setnu "setnu" nil t) ; line numbers mode
(dolist
	(sym '(wcy-switch-buffer-forward wcy-switch-buffer-backward))
	(autoload sym "wcy-swbuff" nil t)) ; buffer cycling thru minibuff
(autoload 'browse-kill-ring "browse-kill-ring" nil t)


;; Key-related settings
;; subword-mode doesn't seem to work with transient mark, can probably be fixed
;; (setq-default global-subword-mode t)


;;;; Actual bindings

(global-set-keys
	;; Mode setting globals
	`(("M-/" fg-scite-code)
		("M-?" fg-scite-lisp)
		("M-'" fg-scite-aux)
		("M-]" fg-scite-core)
		("C-?" setnu-mode)
		("C-M-o" (lambda () (interactive)
			(multiple-value-bind (action msg)
				(if (memq 'delete-trailing-whitespace before-save-hook)
					'(remove-hook "disabled") '(add-hook "enabled"))
				(apply action '(before-save-hook delete-trailing-whitespace))
				(message (format "delete-trailing-whitespace: %s" msg)))))
		("C-M-S-o" delete-selection-mode)

		;; Lookz switching should work everywhere as well
		("C-M-/" fg-masq-x-light)
		("C-M-'" fg-masq-x-dark)
		("C-M-]" fg-masq-x-pitch)

		;; Encoding stuff
		("C-M-[" universal-coding-system-argument)
		("C-M-p" ,(iwrapm fg-revert-buffer-to-enc 'koi8-r))
		("C-M-S-p" ,(iwrapm fg-revert-buffer-to-enc 'cp1251))
		("M-P" ,(iwrapm fg-revert-buffer-to-enc 'utf-8-unix)) ; to show ^M things
		("M-p" ,(iwrapm fg-revert-buffer-to-enc 'undecided)) ; safe bet

		;; Stack-buffer hop
		("C-<return>" fg-stack-buffer)

		;; Jump to often-used ERC buffers with F-keys
		;; local notification channels
		("<f5>" ,(iwrapm fg-find-buffer "#bordercamp"))
		("<S-f5>" ,(iwrapm fg-find-buffer "#snort"))
		;; im and twitter stuff
		("<f6>" ,(iwrapm fg-find-buffer "&bitlbee"))
		("<S-f6>" ,(iwrapm fg-find-buffer "#twitter_bitlbee"))
		("<C-f6>" fg-erc-ddg-define) ; prints the definition of whatever is selected
		;; irc channel shortcuts
		("<f8>" ,(iwrapm fg-find-buffer "#exherbo"))
		("<f9>" ,(iwrapm fg-find-buffer "#tahoe-lafs"))))


;;;; Snippet to rebind stuff online
;; (define-key fg-scite-core-map (key "s-,") 'emms-shuffle)


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
		(,(key "M-<left>") . ,(iwrapm windmove-left 1))
		(,(key "M-<right>") . ,(iwrapm windmove-right 1))
		(,(key "M-<up>") . ,(iwrapm windmove-up 1))
		(,(key "M-<down>") . ,(iwrapm windmove-down 1))

		;; Had to leave C-S-NUM bindings reserved for the tentacled aliens from outta space
		(,(key "C-,") . split-window-horizontally)
		(,(key "C-.") . split-window-vertically)
		(,(key "C-<") . delete-window)
		(,(key "C->") . delete-other-windows)
		(,(key "M-,") . ,(iwrapm kill-buffer nil)) ; kill current buffer w/o asking
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
		(,(key "s-'") . emms-next)
		(,(key "s-;") . emms-previous)
		(,(key "s-:") . emms-stop)
		(,(key "s-\"") . emms-pause)
		(,(key "s-.") . emms)
		(,(key "s->") . emms-shuffle)
		(,(key "s-<") . emms-sort)
		(,(key "M-s-.") . emms-sort)
		(,(key "M-s-<") . emms-uniq)
		(,(key "C-s-,") . emms-add-playlist)
		(,(key "s-,") . fg-emms-add-directory-tree-glob)
		(,(key "M-s-,") . fg-emms-add-file-glob)
		(,(key "s-/") . fg-emms-notify)
		(,(key "M-s-/") . ,(lambda () (interactive)
			(setq fg-emms-scrobble-tracks (not fg-emms-scrobble-tracks))
			(message "Scrobbling %s"
				(if fg-emms-scrobble-tracks "enabled" "disabled"))))
		(,(key "s-?") . emms-playlist-save)
		(,(key "C-s-?") . emms-playlist-mode-clear)
		(,(key "M-s-;") . emms-seek-backward)
		(,(key "M-s-'") . emms-seek-forward)
		(,(key "<XF86AudioPlay>") . emms-pause)

		;; -- Notification controls --
		(,(key "C-n") . fg-track-switch)
		(,(key "M-n") . ,(lambda (arg)
			(interactive "p") (fg-track-switch (- arg))))
		(,(key "C-S-n") . fg-track-reset)
		(,(key "C-M-n") . fg-feeds)
		(,(key "C-M-S-n") . fg-erc-cycle-channels)

		;; -- History --
		;; Consistent undo/redo
		;; C-z is dangerous otherwise!
		(,(key "C-z") . ,(transient-wrap 'undo "p"))
		(,(key "C-S-z") . ,(transient-wrap 'redo "p"))
		(,(key "M-z") . repeat)

		;; -- Invocation --
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
		(,(key "C-<left>") . ,(iwrapm term-send-raw-string ";5D"))
		(,(key "C-<right>") . ,(iwrapm term-send-raw-string ";5C"))
		(,(key "C-S-<left>") . ,(iwrapm term-send-raw-string ";6D"))
		(,(key "C-S-<right>") . ,(iwrapm term-send-raw-string ";6C"))

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
		(,(key "C-M-c") . browse-kill-ring)
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
		(,(key "C-M-u") . fg-kill-line)

		;; Metabuffer stuff
		(,(key "<C-M-return>") . fg-occur)
		(,(key "<M-return>") . fg-wtf))
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
		(,(key "C-<prior>") . ,(iwrapm move-to-window-line 0))
		(,(key "<next>") . fg-scroll-down) ; pagedown
		(,(key "C-<next>") . ,(iwrapm move-to-window-line -1))

		;; Block-skimming (emacs' re-definition, jic)
		(,(key "<C-left>") . backward-word)
		(,(key "<C-right>") . forward-word)
		(,(key "<C-up>") . backward-paragraph)
		(,(key "<C-down>") . forward-paragraph)

		;; git-gutter ops
		(,(key "M-e") . git-gutter:toggle)
		(,(key "C-M-e") . git-gutter:revert-hunk)
		(,(key "<C-M-up>") . git-gutter:previous-hunk)
		(,(key "<C-M-down>") . git-gutter:next-hunk)

		;; Region ops
		(,(key "M-u") . ,(transient-wrap 'upcase-region "r"))
		(,(key "M-U") . ,(transient-wrap 'capitalize-region "r"))
		(,(key "M-l") . ,(transient-wrap 'downcase-region "r"))
		(,(key "C-/") . ,(transient-wrap 'fg-comment "P"))

		;; Skimming ops
		(,(key "<f3>") . isearch-repeat-forward)
		(,(key "C-s") . isearch-forward)
		(,(key "C-M-s") . isearch-forward-regexp)
		(,(key "C-S-s") . query-replace)
		(,(key "C-M-S-s") . query-replace-regexp)

		;; Metabuffer stuff
		(,(key "C-=") . compare-windows)
		(,(key "M-v") . clone-buffer)
		(,(key "C-<mouse-1>") . ffap-at-mouse)

		;; Lookz
		(,(key "M-w") . toggle-truncate-lines)
		(,(key "C-M-w") . develock-mode)

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

(define-minor-mode fg-scite-stack
	"Special mode for editing stack-buffer."
	:init-value nil
	:lighter "/s"
	:keymap `((,(key "<return>") . fg-newline-stack))
	:group 'fg-scite)



(define-minor-mode fg-scite-emms
	"Special keymap with holes for emms bindings."
	:init-value nil
	:lighter "/!"
	:keymap `(
		(,(key "<backspace>") . fg-emms-playlist-mode-del)
		(,(key "<delete>") . fg-emms-playlist-mode-del)
		(,(key "C-c") . fg-emms-playlist-mode-copy)
		(,(key "C-k") . fg-emms-playlist-mode-kill)
		(,(key "C-x") . fg-emms-playlist-mode-kill)
		(,(key "C-v") . emms-playlist-mode-yank)
		(,(key "C-z") . emms-playlist-mode-undo)
		(,(key "C-d") . fg-emms-playlist-mode-clone)
		(,(key "M-f") . emms-add-playlist)
		(,(key "M-s") . emms-playlist-save))
	:group 'fg-scite)
(set-keymap-parent fg-scite-emms-map fg-scite-core-map)



;; -- Basic text-mode overrides (propognates to many other modes) --
(define-keys text-mode-map
	`(("<prior>" fg-scroll-up) ; pageup
		("C-<prior>" ,(iwrapm move-to-window-line 0))
		("<next>" fg-scroll-down) ; pagedown
		("C-<next>" ,(iwrapm move-to-window-line -1))))



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


;; -- Common activity-switch-to/reset routines
(defun fg-track-reset ()
	"Drop annoying status line notifications"
	(interactive)
	(when (fboundp 'fg-jabber-activity-reset)
		(fg-jabber-activity-reset))
	(when (fboundp 'fg-erc-track-reset)
		(fg-erc-track-reset)))

(defun fg-track-switch (arg)
	"Switch to jabbra or erc activity buffers"
	(interactive "p")
	(let ((buff (current-buffer)))
		(when (fboundp 'jabber-activity-switch-to)
			(jabber-activity-switch-to))
		(when
			(and (fboundp 'erc-track-switch-buffer)
				(eq buff (current-buffer)))
			(erc-track-switch-buffer arg))))


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
	`(("+" ,(iwrapm ibuffer-mark-by-file-name-regexp ".*"))
		("-" ,(iwrapm ibuffer-unmark-all ibuffer-marked-char))
		("<prior>" fg-scroll-up) ; pageup
		("<next>" fg-scroll-down) ; pagedown
		("*" ibuffer-toggle-marks)
		("SPC" ,(iwrapm fg-ibuffer-mark nil))
		("<insert>" ,(iwrapm fg-ibuffer-mark t))))


;; -- Jabbra submode --
(eval-after-load "jabber-roster" '(progn
	(define-keys jabber-roster-mode-map
		'(("v" jabber-vcard-get)
			("V" jabber-get-version)
			("m" jabber-send-message)))
	(define-keys jabber-chat-mode-map
		'(("<return>" fg-newline)
			("C-<return>" jabber-chat-buffer-send)))))
;; These fix keybindings for URLs in jabber-chat buffers
(eval-after-load "goto-addr" '(progn
	(define-key goto-address-highlight-keymap (key "C-c RET") nil)
	(define-key goto-address-highlight-keymap (key "C-c") nil)))


;; -- Kill-ring mode fixes --
(eval-after-load "browse-kill-ring-mode" '(progn
	(defalias 'browse-kill-ring-quit 'quit-window)))


;; -- JS/Perl/Go/YAML mode eclectic/electric crap removal --
(mapc
	(lambda (vars)
		(apply
			(lambda (file map keys)
				(eval-after-load file `(progn
					(mapc
						(lambda (key) (define-key ,map key nil)) ,keys))))
			vars))
	'(("js" js-mode-map '("{" "}" "(" ")" ":" ";" ","))
		("perl" perl-mode-map '("{" "}" "(" ")" ":" ";" ","))
		("yaml-mode" yaml-mode-map '(">" "|"))
		("go-mode" go-mode-map '("}" ")" ":" "="))))


;; -- Python mode-specific actions --
(eval-after-load "python" '(progn
	(define-keys python-mode-map
		'(("<C-M-up>" python-beginning-of-block)
			("<C-M-down>" python-end-of-block)
			(":" self-insert-command)))))

;; -- PHP mode-specific actions --
(eval-after-load "php-mode" '(progn
	(define-key php-mode-map (key "C-'") 'fg-php-tag-line)))


;; -- PgUp/PgDown in docview --
(eval-after-load "doc-view" '(progn
	(define-keys doc-view-mode-map
		'(("=" doc-view-enlarge) ; holding shift is a pain! ;)
			("<next>" doc-view-next-page)
			("<prior>" doc-view-previous-page)))))


;; -- yasnippet --
(eval-after-load "yasnippet" '(progn
	(defadvice yas/init-minor-keymap
		(around fg-yas/init-minor-keymap activate)
		ad-do-it
		(define-key ad-return-value (key "C-c &") nil)
		(define-key ad-return-value (key "C-c") nil))))


;; -- w3m keys --
(eval-after-load "w3m" '(progn
	(define-keys w3m-mode-map
		'(("C-c" nil)
			("<M-return>" w3m-close-window)
			("<up>" previous-line) ("<down>" next-line)
			("<left>" backward-char) ("<right>" forward-char)
			("<tab>" w3m-next-anchor) ("<backtab>" w3m-previous-anchor)
			("<prior>" fg-scroll-up) ("<next>" fg-scroll-down)))))


;; -- Lisp modes --
(dolist
	(mode-map
		(list emacs-lisp-mode-map
			lisp-interaction-mode-map
			fg-scite-lisp-map))
	(define-keys mode-map
		;; Arrows to navigate sexp blocks
		'(("C-M-<left>" backward-sexp)
			("C-M-<right>" forward-sexp)
			("C-M-<up>" up-list)
			("C-M-<down>" down-list))))


;; -- ERC + submodes --

(eval-after-load "erc" '(progn
	;; Put mark-line w/o having to type /mark
	(define-key erc-mode-map (key "C-j") 'fg-erc-mark)
	;; Summon prev/next sent (typed) lines from history
	(define-key erc-mode-map (key "<C-M-up>") 'erc-previous-command)
	(define-key erc-mode-map (key "<C-M-down>") 'erc-next-command)
	;; Special "readability" hack, no idea why command is disabled by default
	(define-key erc-mode-map (key "C-f") 'erc-remove-text-properties-region)
	(define-key erc-mode-map (key "<prior>") 'fg-scroll-up)
	(define-key erc-mode-map (key "<next>") 'fg-scroll-down)
	;; Next two are purely because PgUp/PgDn
	;;  are right next to the arrows on acer laptops I use,
	;;  and I frequently butterfinger Fn with Ctrl
	(define-key erc-mode-map (key "<C-prior>") 'fg-beginning-of-line)
	(define-key erc-mode-map (key "<C-next>") 'move-end-of-line)
	(put 'erc-remove-text-properties-region 'disabled nil)))

(eval-after-load "erc-track" '(progn
	;; Get rid of global erc-track C-c bindings
	(define-key erc-track-minor-mode-map (key "C-c C-SPC") nil)
	(define-key erc-track-minor-mode-map (key "C-c C-@") nil)
	(define-key erc-track-minor-mode-map (key "C-c") nil)))


;; -- KMacro (ex)globals --
(global-set-keys '(("<f3>" nil) ("<f4>" nil)))


;; -- Auto mode-switching --
(defun fg-hook-set-mode ()
	"Turn fg-scite-* (and various cosmetic) minor modes, depending on major."
	;; (message "%s, %s, %s" major-mode buffer-file-name (buffer-name))
	(if buffer-file-name ; nil for system buffers and terminals
		(cond
			((eq major-mode 'lisp-mode) (fg-scite-lisp t))
			((eq major-mode 'doc-view-mode) t) ; it has specific bindings
			(t (fg-scite-code t))) ; if it's a file, then it's at least code
		(cond
			((eq major-mode 'occur-mode) (fg-scite-aux t))
			((eq major-mode 'browse-kill-ring-mode) (fg-scite-aux t))
			((eq major-mode 'term-mode) ; term-mode minors should probably be set via multi-term hooks
				(fg-scite-term t))
			((eq major-mode 'lisp-interaction-mode) ; *scratch*
				(fg-scite-code t))
			((memq major-mode
					'(help-mode slime-repl-mode erc-mode))
				(fg-scite-aux t))
			((string-match "^newsticker-treeview-"
					(symbol-name major-mode))
				(fg-scite-aux t)))))

;; Hooks can be added w/o loading var definitions
(add-hook 'erc-mode-hook 'fg-scite-aux)
(add-hook 'jabber-chat-mode-hook 'fg-scite-aux)
(add-hook 'minibuffer-setup-hook 'fg-scite-aux)
(add-hook 'find-file-hook 'fg-hook-set-mode)
(add-hook 'after-change-major-mode-hook 'fg-hook-set-mode)
(add-hook 'emms-playlist-mode-hook
	(lambda () (fg-scite-emms t) (fg-scite-core nil)))
