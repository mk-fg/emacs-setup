;; Initial scratch buffer
;; Useful for common temp snippets to edit/run from there

(defvar fg-scratch
";;;; See fg_looks.el for initial contents here

;; Use this to apply `face'/`type' pcre highlighting to buffers marked in `hi-ibuffer-buffer'
(let
	((face nil) ;; see hi-lock-faces
		;; (face 'hi-blue)
		;; (face 'hi-red-b)
		;; (face 'hi-green-b)
		;; (face 'hi-black-hb)
		(face 'fg-hi-red)
		;; ----
		;; (type 'highlight-regexp)
		(type 'highlight-lines-matching-regexp)
		;; ----
		(pcre (concat
			;; \"(thing1|thing2)\"
			\"\")))

	(hi-ibuffer-pcre pcre type face))

;; (hi-ibuffer-drop)
" "Initial contents of scratch buffer for `fg-scratch-init'.")

(defun fg-scratch-init ()
	"Erase and re-init scratch buffer from `fg-scratch' string."
	(interactive)
	(with-current-buffer "*scratch*"
		(erase-buffer)
		(insert fg-scratch)))

(fg-scratch-init)


;; Highlighting stuff non-interactively (as editing stuff in minibuffer is pain)
;; Useful tricks - https://www.emacswiki.org/emacs/HiLock
;; Pattern storage: hi-lock-interactive-patterns

(require 'hi-lock)

(defvar hi-ibuffer-buffer "*Ibuffer*"
	"Ibuffer buffer to use for marked-buffer (un-)highlighting routines.")

(defmacro fg-hi-faces-init (colors)
	"Init simple highlighting faces with foreground color matching name.
Names are templated as fg-hi-<color> from a list of color symbols."
	`(progn ,@(mapcar (lambda (c)
		`(defface ,(intern (format "fg-hi-%s" c))
			'((t :foreground ,(symbol-name c)))
			,(format "Foreground color highlighting: %s" c)
			:group 'hi-lock-faces)) colors)))
;; (pp-macroexpand-expression '(fg-hi-faces-init (red blue yellow)))

(fg-hi-faces-init (red blue green yellow))

(defun hi-ibuffer-pcre (pcre &optional type face)
	"Highlight PCRE in all buffers marked in `hi-ibuffer-buffer' ibuffer.
TYPE defaults to `highlight-lines-matching-regexp',
and can be replaced by something like `highlight-regexp' if necessary.
FACE defaults to nil, see `hi-lock-faces' for list of these."
	(unless type (setq type 'highlight-lines-matching-regexp))
	(with-current-buffer hi-ibuffer-buffer
		(ibuffer-do-eval `(,type (rxt-pcre-to-elisp ,pcre) ',face))))

(defun hi-ibuffer-drop (&optional regexp)
	"Remove highlighting from all buffers marked in `hi-ibuffer-buffer' ibuffer."
	(interactive)
	(with-current-buffer hi-ibuffer-buffer
		(ibuffer-do-eval '(unhighlight-regexp (or regexp t)))))


;; Encoding
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(defun fg-revert-buffer-to-enc (enc)
	(let
		((coding-system-for-read enc)
			(coding-system-for-write enc))
		(revert-buffer t t)))

(defadvice select-safe-coding-system-interactively
	(around fg-select-safe-coding-system-interactively activate) 'raw-text)


;; Fonts
;; - Always use variable-pitch font for code buffers
;; - There's weird interesting stuff like https://github.com/tonsky/FiraCode for monospace
;; - To debug: "M-` (universal arg, C-u) M-x what-cursor-position" on the weird letter
;; - "describe-fontset" can be useful, and then overriding ranges from there via "set-fontset-font"
;; - use font-ranges-to-elisp.py script for easier copy-pasting from wikipedia
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html
(defun fg-font-init ()
	"Set proper font values for current frame."
	(interactive)
	(when window-system
		(set-frame-font "Liberation SansX-9")
		(set-face-font 'variable-pitch "Liberation SansX-9")
		(set-face-font 'fixed-pitch "Liberation Mono-9")
		(set-fontset-font t '(?А . ?я) "Liberation SansX-9")
		;; Emoji ranges from https://en.wikipedia.org/wiki/Emoji
		(dolist
			(range-cons
				'((#x1F300 . #x1F5FF) (#x1F900 . #x1F9FF) (#x1F600 . #x1F64F)
					(#x1F680 . #x1F6FF) (#x2600 . #x26FF) (#x2700 . #x27BF)))
			(set-fontset-font t range-cons "Symbola-10"))
		;; CJK ranges from https://en.wikipedia.org/wiki/CJK_Unified_Ideographs
		;; Last updated from Unicode 8.0 (2015), with Extension E
		(dolist
			(range-cons
				;; CJK Unified Ideographs
				'((#x4E00 . #x62FF) (#x6300 . #x77FF) (#x7800 . #x8CFF) (#x8D00 . #x9FFF)
					;; CJK-UI Ext A
					(#x3400 . #x4DBF)
					;; CJK-UI Ext B
					(#x20000 . #x215FF) (#x21600 . #x230FF) (#x23100 . #x245FF)
					(#x24600 . #x260FF) (#x26100 . #x275FF) (#x27600 . #x290FF) (#x29100 . #x2A6DF)
					;; CJK-UI Ext A, C, D, E, Compatibility Ideographs - single range for each
					(#x3400 . #x4DBF) (#x2A700 . #x2B73F)
					(#x2B740 . #x2B81F) (#x2B820 . #x2CEAF) (#xF900 . #xFAFF)
					;; Legacy ranges
					(#x3300 . #x33FF) (#xFE30 . #xFE4F) (#xF900 . #xFAFF) (#x2F800 . #x2FA1F)))
			(set-fontset-font t range-cons "IPA-11"))))
(fg-font-init)

(defun fg-char-name ()
	"Print name of character under cusor to minibuffer."
	(interactive)
	(message "%s" (get-char-code-property (following-char) 'name)))


;; Time is critical
(setq-default
	display-time-day-and-date t
	display-time-24hr-format t
	display-time-default-load-average nil
	calendar-date-style 'european
	calendar-latitude [56 50 north]
	calendar-longitude [60 35 east]
	calendar-time-display-form ; "13:05 (YEKT)"
		'(24-hours ":" minutes
			(if time-zone " (") time-zone (if time-zone ")")))
(display-time)

;; Smooth scrolling
(setq-default
	scroll-preserve-screen-position t ; keep vertical pos
	scroll-conservatively 1000000
	;; scroll-margin 5 - cursor tends to get stuck on margins, TODO: fix
	scroll-step 1
	line-move-visual t ; keep horizontal pos
	mouse-wheel-scroll-amount '(1 ((shift) . 1))
	mouse-wheel-progressive-speed nil
	mouse-wheel-follow-mouse t
	isearch-allow-scroll t) ; alas, it doesn't work for custom PgUp/PgDn, TODO: extend

;; Assorted minor tweaks
(setq-default
	inhibit-startup-screen t
	split-height-threshold nil ; split only from-up-to-down
	split-width-threshold nil
	frame-title-format
		'((:eval
			(if (buffer-file-name)
				(format "emacs: %s" (abbreviate-file-name (buffer-file-name)))
				"emacs: %b")))
	show-paren-style 'mixed ; mark the area in the direction of far parenthesis
	visible-bell t) ; flash on weird stuff

;; Cosmetic minor modes
(global-font-lock-mode t) ; syntax highlighting (funny name for it)
(show-paren-mode t) ; show matching parenthesis
(column-number-mode t) ; column numbers at the bottom
(display-battery-mode t) ; show the death clock

;; (which-function-mode t) ; show which function pointer is in
;; Seem to be causing a lot of heavy parsing in some modes, not that important

;; Rodent banishment (if any)
(when (and (display-mouse-p) (require 'avoid))
	(mouse-avoidance-mode 'exile))

;; Character display tweaks
(or standard-display-table (setq standard-display-table (make-display-table)))
;; Display page delimiter ^L as a horizontal line (plus "^L")
(aset standard-display-table ?\f (vconcat (make-vector 64 ?-) "^L"))
;; Tab indentation guides
(aset standard-display-table ?\t (vconcat "˙ "))
;; More distinct char for commas vs periods - an option for non-special fonts
;; It's best to pick one from default font via gucharmap, to avoid mixing these
; (aset standard-display-table ?, (vconcat "˾"))
; (aset standard-display-table ?. (vconcat "❟"))

;; git-gutter
(when
	(load-library-safe "git-gutter")
	(global-git-gutter-mode t)
	(setq-default
		git-gutter:update-interval 2
		git-gutter:window-width 2
		git-gutter:modified-sign "xx"
		git-gutter:added-sign "++"
		git-gutter:deleted-sign "--"
		git-gutter:lighter ""
		git-gutter:always-show-gutter t
		git-gutter:diff-option "-w"))

;; doc-view
(setq-default doc-view-continuous t)

;; find-file tweaks
(setq-default
	find-file-run-dired nil
	find-file-visit-truename t
	find-file-existing-other-name t)

;; Disable vc-* modes, which slow stuff down pointlessly, esp. on sshfs and such
(eval-after-load "vc" '(progn
	(remove-hook 'find-file-hooks 'vc-find-file-hook)
	(remove-hook 'find-file-hooks 'vc-refresh-state)))
(setq-default vc-handled-backends ())


;; buffer color depending on filename/buffer-name
;; idea: http://deliberate-software.com/emacs-project-tip/
;; TODO: picking of lab color values in a way that maximizes color distance from prev ones
(require 'color)

(defun* fg-buffer-bg-tweak (&optional seed (min-shift 2) (max-shift '(3 5 5)))
	"Adjust buffer bg color based on (md5 of-) SEED.
MIN-SHIFT / MAX-SHIFT and SEED are passed to `fg-color-tweak'.
If SEED is nil or an empty string, bg color is restored to default face bg."
	(interactive)
	(buffer-face-set
		(list :background
			(fg-color-tweak
				(plist-get (custom-face-attributes-get 'default (selected-frame)) :background)
				seed min-shift max-shift))))

(defun fg-buffer-bg-tweak-name (&optional name)
	"Adjust buffer bg color based on buffer filename or name (if not a file).
NAME can also be passed explicitly as an argument."
	(interactive)
	(unless name
		(set 'name (or buffer-file-name (buffer-name))))
	(fg-buffer-bg-tweak name))

(defun fg-buffer-bg-tweak-name-all ()
	(interactive)
	(dolist (buff (buffer-list))
		(with-current-buffer buff
			(fg-buffer-bg-tweak-name))))


;; Local modes
(load-library-safe "develock-py")
(autoload 'rainbow-mode
	"rainbow-mode" "Color code highlighting mode" t)
(autoload 'yaml-mode
	"yaml-mode" "Mode for editing YAML files" t)
(autoload 'go-mode
	"go-mode" "Mode for editing Go sources" t)
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(autoload 'gfm-mode "markdown-mode" "Major mode for github-flavored-markdown files" t)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(autoload 'edje-mode "edje-mode" "Major mode for editing Edje files" t)
(autoload 'jinja2-mode "jinja2-mode" "Major mode for Jinja2 templates" t)
(load-library-safe (concat fg-path "/tuareg/tuareg-site-file"))

(add-to-list 'load-path (concat fg-path "/extz/rust"))
(autoload 'rust-mode "rust-mode" "Major mode for Rust" t)

(add-to-list 'load-path (concat fg-path "/extz/erlang"))
(autoload 'erlang-mode "erlang-start" "Major mode for Erlang" t)
(add-to-list 'load-path (concat fg-path "/extz/emacs-elixir"))
(autoload 'elixir-mode "elixir-mode" "Major mode for Erlang Elixir" t)

(add-to-list 'load-path (concat fg-path "/extz/kotlin-mode"))
(autoload 'kotlin-mode "kotlin-mode" "Major mode for Kotlin" t)

(add-to-list 'load-path (concat fg-path "/extz/nim-mode"))
(autoload 'nim-mode "nim-mode" "Major mode for Nim language" t)

;; CSV mode
(autoload 'csv-mode "csv-mode" "Major mode for editing CSV files." t)
(eval-after-load "csv-mode" '(progn
	(custom-set-variables `(csv-separators '("," "\t" ";") t))
	;; Emacs tabulation thing works weirdly with
	;;  long header names unless plenty of padding is added.
	(setq-default csv-align-padding 5)))


;; recentf mode tweaks

(setq-default
	recentf-max-saved-items 256
	recentf-max-menu-items 10
	recentf-menu-filter 'recentf-arrange-by-rule
	recentf-save-file (concat fg-path-spool "recentf")
	recentf-arrange-rules
		`(("py (%d)" ".\\.py\\'") ("js (%d)" ".\\.js\\'")
			("sh (%d)" ".\\.sh\\'") ("lua (%d)" ".\\.lua\\'")
			("go (%d)" ".\\.go\\'") ("rust (%d)" ".\\.\\(rs\\|rlib\\)\\'") ("nim (%d)" ".\\.nim\\'")
			("OCAML (%d)" ".\\.mli?\\'") ("C (%d)" ".\\.\\(cc?\\|cxx\\|h\\)\\'")
			("erlang (%d)" ".\\.\\([eh]rl\\|ex\\)\\'") ("kotlin (%d)" ".\\.\\(kt\\|gradle\\)\\'")
			("perl (%d)" ".\\.pl[0-9]?\\'") ("sql (%d)" ".\\.sql\\'")
			("web/tpl (%d)" ".\\.\\(html\\|css\\|scss\\|jade\\|htm\\|tpl\\)\\'")
			("(e)lisp/scheme (%d)" ".\\.\\(el\\|cl\\|lisp\\|scm\\|rkt\\|ss\\|jl\\)\\'")
			("conf (%d)"
				,(concat
					".\\.\\(c\\(onf\\|fg\\|f\\)\\|\\(ya?ml\\)\\|vol\\|path"
						"\\|service\\|target\\|socket\\|mount\\|device\\|swap\\)"
					"\\(\\.\\(sample\\|example\\|dist\\|documented\\|in\\)\\)?\\'"))))


;; Auto-mode tweaks

(delq (assoc-string "\\.inc\\'" auto-mode-alist) auto-mode-alist)
(setq-default auto-mode-alist
	(-distinct (-concat auto-mode-alist
		`(("/\\(PKG\\|APK\\)BUILD$" . sh-mode) ("\\.\\(kt\\|gradle\\)$" . kotlin-mode)
			("\\.[eh]rl$" . erlang-mode) ("\\.exs?$" . elixir-mode)
			("\\.jl$" . lisp-mode) ("\\.rkt$" . scheme-mode)
			("/polkit\\(-1/rules\\.d\\)?/[^/]+\\.rules$" . js-mode) ("\\.ts$" . js-mode)
			("\\.ya?ml$" . yaml-mode) ("\\.edc$" . edje-mode)
			("\\.\\(text\\|markdown\\|md\\|mkd\\|mdown\\|mkdn\\|mdwn\\)$" . markdown-mode)
			("\\.lua$" . lua-mode) ("\\.\\(rs\\|rlib\\)$" . rust-mode)
			("\\.go$" . go-mode) ("\\.nim$" . nim-mode)
			("\\.scss$" . css-mode) ("\\.jade$" . jade-mode) ("\\.j2$" . jinja2-mode)
			("\\.svg$" . xml-mode) ("\\.\\(fwd\\|rev4\\|rev6\\)$" . zone-mode)
			(,(concat
				".\\.\\(c\\(onf\\|fg\\|f\\|nf\\)\\|\\(ya?ml\\)\\|vol"
					"\\|service\\|target\\|socket\\|mount\\|device\\|swap\\)"
				"\\(\\.\\(sample\\|example\\|dist\\|documented\\|in\\)\\)?$") . conf-mode)))))

;; Blacklist modes from auto-mode-alist via earlier overrides
(setq-default auto-mode-alist
	;; blacklist sieve-mode - buggy, locks-up too often
	(delete '("\\.s\\(v\\|iv\\|ieve\\)\\'" . sieve-mode) auto-mode-alist))


;; Crosshair highlighting modes
;; (load-library-safe "column-marker")
(when
	(and
		(load-library-safe "vline")
		(load-library-safe "col-highlight"))

	(setq-default
		vline-use-timer t
		vline-idle-time 0.1
		col-highlight-vline-face-flag nil
		col-highlight-period 2)

	(defun vline-post-command-hook ()
		(when (and vline-mode (not (minibufferp))) (flash-column-highlight)))
	(defalias 'vline-timer-callback 'vline-post-command-hook)

	(defvar col-highlight-flash-timer (timer-create)
		"Timer for an active column highlihting in `vline-mode'.")
	(defadvice col-highlight-flash
		(around fg-col-highlight-flash-cleanup activate)
		"Flash current column, cancelling previous flash-timer,
	stored in `col-highlight-flash-timer'.
	See `col-highlight-flash-set' for details."
		(cancel-timer col-highlight-flash-timer)
		(setq col-highlight-flash-timer ad-do-it)))


;; Ibuffer tweaks
(require 'ibuffer)
(require 'ibuf-ext)

; Extended filename-and-process column to also print ERC topics
(define-ibuffer-column fg-fn-proc-topic
	(:name "File / Proc / ERC")
	(let
		((proc (get-buffer-process buffer))
			(fn (ibuffer-make-column-filename buffer mark))
			(topic (when
				(and (eq major-mode 'erc-mode) erc-channel-topic)
				(erc-controls-interpret (fg-string-before erc-channel-topic "\C-o")))))
		(cond
			(proc
				(concat
					(propertize
						(format "(%s %s)" proc (process-status proc))
						'font-lock-face 'italic 'ibuffer-process proc)
					(if (> (length fn) 0) (format " %s" fn) "")))
			(topic
				(let*
					((net (symbol-name (erc-networks--id-symbol erc-networks--id)))
						(net (if (member (fg-string-or net "nil") '("nil" "-")) "<erc>" net)))
					(format "%s :: %s" net (or (fg-string-or topic) ""))))
			(t fn))))

(setq-default
	ibuffer-formats
		'((mark modified read-only
			" " (name 28 28 :left :elide)
			" " (size 9 -1 :right)
			" " (mode 11 11 :left :elide)
			" " fg-fn-proc-topic)
		(mark
			" " (name 16 -1)
			" " filename))
	ibuffer-filter-groups-global () ; applied via fg-ibuffer-apply-locals or ibuffer-mode-hook
	ibuffer-show-empty-filter-groups nil)

(defadvice ibuffer-insert-filter-group
	(before fg-ibuffer-insert-filter-group-sep activate)
	(insert "\n"))

(add-hook 'ibuffer-mode-hook 'fg-ibuffer-apply-locals)

(eval-after-load "erc" '(progn
	;; Adds ibuffer filter groups for various ERC buffer types

	(define-ibuffer-filter erc-chan
		"Limit current view to ERC channel buffers."
		(:description "erc chan buffer" :reader nil)
		(and
			(eq 'erc-mode (buffer-local-value 'major-mode buf))
			(not (erc-server-buffer-p buf))))

	(define-ibuffer-filter erc-server-buffer
		"Limit current view to ERC server buffers."
		(:description "erc server buffer" :reader nil)
		(erc-server-buffer-p buf))

	(setq-default ibuffer-filter-groups-global
		'(("files" (filename . ""))
			("erc" (erc-chan))
			("erc-servers" (erc-server-buffer))))
	(fg-ibuffer-reset-filters)))

(define-ibuffer-sorter recency
	"Don't sort the buffers at all, keeping (buffer-list) order.
This allows to re-order them by b-key easily, burying them to the bottom.
Used to be default before emacs-28, was changed to use `buffer-display-time' there.
Shouldn't work with reverse-order (which emacs-28 fix addressed), but I don't use it.
See also: emacs bug 30129, emacs-mirror/emacs#d3cb07d7."
	(:description "recency"))


;; Mask for X (inits are bg-agnostic colors)
;; TODO: rewrite it as a single theme,
;;  with colors derived from `frame-background-mode'
;; See also `frame-set-background-mode'

(defvar fg-color-fg-core)
(defvar fg-color-fg-fade15)
(defvar fg-color-fg-fade30)
(defvar fg-color-fg-fade50)
(defvar fg-color-bg-core)
(defvar fg-color-bg-hl)
(defvar fg-color-bg-hl2)
(defvar fg-color-fg-modeline "firebrick")
(defvar fg-color-spell-dupe "Gold3")
(defvar fg-color-spell-err "OrangeRed")
(defvar fg-color-irrelevant "medium sea green")
(defvar fg-color-irrelevant-xtra "sea green")
(defvar fg-color-comment "DeepSkyBlue4")
(defvar fg-color-kw "dark green")
(defvar fg-color-func "gold")
(defvar fg-color-func-modeline fg-color-func)
(defvar fg-color-type "dark slate gray")
(defvar fg-color-key "MistyRose4")
(defvar fg-color-var "Coral")
(defvar fg-color-static "olive drab")

(defface fg-fade15-face () "Default face with 15% faded fg color")
(defface fg-fade30-face () "Default face with 30% faded fg color")
(defface fg-fade50-face () "Default face with 50% faded fg color")

;; (face-all-attributes 'region)

(defun fg-masq-x ()
	"CSS-like binding."
	(custom-set-faces
		`(default
			((,(and
					(boundp 'fg-color-fg-core)
					(boundp 'fg-color-bg-core))
				(:foreground ,fg-color-fg-core
					:background ,fg-color-bg-core))))
		`(region ((t (:foreground ,fg-color-fg-core :background ,fg-color-bg-hl))))
		;; Py
		`(py-builtins-face ((t (:foreground ,fg-color-kw))))
		`(py-decorators-face ((t (:foreground ,fg-color-kw))))
		`(py-pseudo-keyword-face ((t (:foreground ,fg-color-kw))))
		;; Develock
		`(develock-reachable-mail-address
			((t (:foreground ,fg-color-irrelevant :background ,fg-color-bg-hl2))))
		;; FlySpell
		`(flyspell-duplicate ((t (:foreground ,fg-color-spell-dupe :underline t :weight normal))))
		`(flyspell-incorrect ((t (:foreground ,fg-color-spell-err :underline t :weight normal))))
		;; Vline
		`(vline ((t (:background ,fg-color-bg-hl))))
		`(vline-visual ((t (:background ,fg-color-bg-hl))))
		;; minimap
		`(minimap-highlight-line-face
			((t (:background ,fg-color-bg-hl :foreground ,fg-color-fg-core))))
		`(minimap-active-region-background ((t (:background ,fg-color-bg-hl2))))
		;; Jabber
		`(jabber-title-large ((t (:weight bold :height 1.5))))
		`(jabber-title-medium ((t (:weight bold :height 1.2))))
		`(jabber-roster-user-online
			((t (:foreground ,fg-color-fg-core :slant normal :weight bold))))
		`(jabber-roster-user-away
			((t (:foreground ,fg-color-irrelevant :slant italic :weight normal))))
		`(jabber-roster-user-xa
			((t (:foreground ,fg-color-irrelevant-xtra :slant italic :weight normal))))
		;; ERC
		`(erc-timestamp-face ((t (:foreground ,fg-color-comment :weight normal))))
		`(erc-keyword-face ((t (:foreground ,fg-color-kw :weight bold))))
		`(erc-nick-default-face ((t (:weight normal))))
		`(erc-current-nick-face ((t (:foreground ,fg-color-kw :weight bold))))
		`(erc-notice-face ((t (:foreground ,fg-color-fg-modeline))))
		`(erc-prompt-face ((t (:foreground ,fg-color-fg-core :background nil))))
		;; Newsticker
		`(newsticker-treeview-face ((t (:foreground ,fg-color-fg-core))))
		`(newsticker-treeview-immortal-face
			((t (:inherit newsticker-treeview-face :slant italic :weight bold))))
		`(newsticker-treeview-selection-face ((t (:background ,fg-color-bg-hl))))
		;; git-gutter
		`(git-gutter:added ((t (:foreground "dark green"))))
		`(git-gutter:modified ((t (:foreground "saddle brown"))))
		`(git-gutter:deleted ((t (:foreground "dark red"))))
		;; Misc
		`(yaml-tab-face ((t (:inherit default :background ,fg-color-bg-hl))))
		`(w3m-current-anchor ((t (:inherit w3m-anchor :underline t))))
		`(rst-level-1-face ((t (:background ,fg-color-bg-hl))))
		`(rst-level-2-face ((t (:background ,fg-color-bg-hl))))
		`(rst-level-3-face ((t (:background ,fg-color-bg-hl))))
		`(which-func ((t
			(:inherit font-lock-function-name-face
				:foreground ,fg-color-func-modeline t))))
		`(sh-heredoc ((t (:foreground "olive drab"))))
		;; Fades
		`(fg-fade15-face ((t (:foreground ,fg-color-fg-fade15))))
		`(fg-fade30-face ((t (:foreground ,fg-color-fg-fade30))))
		`(fg-fade50-face ((t (:foreground ,fg-color-fg-fade50))))
		;; Defaults
		`(font-lock-comment-face ((t (:foreground ,fg-color-comment))))
		`(font-lock-function-name-face ((t (:foreground ,fg-color-func))))
		`(font-lock-keyword-face ((t (:foreground ,fg-color-kw))))
		`(font-lock-type-face ((t (:foreground ,fg-color-type))))
		`(font-lock-variable-name-face ((t (:foreground ,fg-color-var))))
		`(font-lock-builtin-face ((t (:foreground ,fg-color-key))))
		`(font-lock-string-face ((t (:foreground ,fg-color-static))))
		`(menu
			((,(and window-system (boundp 'fg-color-fg-core))
				(:background "light slate gray"
					:foreground ,fg-color-fg-core
					:box (:line-width 2 :color "grey75" :style released-button)))))
		`(mode-line ((t (:foreground "black" :background "light slate gray")))))
	(tool-bar-mode -1)
	(menu-bar-mode -1)
	(scroll-bar-mode -1)
	(horizontal-scroll-bar-mode -1)
	(when (boundp 'fg-color-fg-core)
		(set-cursor-color fg-color-fg-core)
		(set-foreground-color fg-color-fg-core)
		(setq-default term-default-fg-color fg-color-fg-core))
	(when (boundp 'fg-color-bg-core)
		(set-background-color fg-color-bg-core)
		(setq-default term-default-bg-color fg-color-bg-core))
	(fg-buffer-bg-tweak-name-all))

;;;; Rainbow mode seem to need a kick here, not sure why
;; (progn (rainbow-mode t) (rainbow-turn-on))

(setq
	blink-cursor-interval 0.3
	blink-cursor-blinks 30)

(defvar blink-cursor-colors '("#ff00d5")
	"On each blink the cursor will cycle to the next color in this list.
Used by custom `blink-cursor-timer-function'.")

(defun blink-cursor-timer-function ()
	"Changes cursor color on each blink, according to `blink-cursor-colors' list."
	(if (internal-show-cursor-p) (internal-show-cursor nil nil)
		(let*
			((color-ns (length blink-cursor-colors))
				(color-n (and (> color-ns 0)
					(% (setq blink-cursor-blinks-done (1+ blink-cursor-blinks-done)) color-ns))))
			(set-cursor-color (nth color-n blink-cursor-colors))
			(internal-show-cursor nil t))
		(when (and (> blink-cursor-blinks 0)
				(> blink-cursor-blinks-done blink-cursor-blinks))
			(blink-cursor-suspend)
			(add-hook 'post-command-hook #'blink-cursor-check))))

(defun fg-masq-x-dark ()
	"Translucent text on dark background."
	(interactive)
	(let*
		((fg-color-fg-core "#6ad468")
			(fg-color-fg-fade15 "#5db85b")
			(fg-color-fg-fade30 "#4f9d4e")
			(fg-color-fg-fade50 "#3d783c")
			(fg-color-bg-core "#101c10")
			(fg-color-bg-hl "DarkGreen")
			(fg-color-bg-hl2 "#182403")
			(fg-color-key "MistyRose2")
			(fg-color-kw "springgreen")
			(fg-color-type "SlateGrey")
			(fg-color-func-modeline "yellow")
			(fg-color-fg-modeline "tomato")
			(fg-color-comment "SteelBlue1"))
		(customize-set-variable 'frame-background-mode 'dark)
		(fg-masq-x))
	(set-cursor-color "#ff00d5")
	(setq blink-cursor-colors (s-split " " (concat
		"#ffffff #02ecfe #ffa2cf #cac900 #fca46a #1bd472 #b3beff"
		" #88ffe1 #c5c48b #ffdc84 #e6afff #4cccbf #f5ff73 #68f032 #94ffaa"))))

(defun fg-masq-x-light ()
	"Black text on white background."
	(interactive)
	(let*
		((fg-color-fg-core "black")
			(fg-color-fg-fade15 "#222424")
			(fg-color-fg-fade30 "#434847")
			(fg-color-fg-fade50 "#707876")
			(fg-color-bg-core "#e0f0ed")
			(fg-color-bg-hl "lavender blush")
			(fg-color-bg-hl2 "#d1e7dd")
			(fg-color-func "saddle brown")
			(fg-color-func-modeline "red4")
			(fg-color-var "IndianRed4"))
		(customize-set-variable 'frame-background-mode 'light)
		(fg-masq-x))
	(set-cursor-color "#890373")
	(setq blink-cursor-colors (s-split " " (concat
		"#000000 #002b89 #363d00 #b02943 #420033 #603800 #1b6392 #992600"
		" #3d6913 #6c001b #000039 #5b007b #5854a4 #705c1a #004519 #9f376e"))))

(defun fg-masq-x-pitch ()
	"Toggle fixed/variable pitch in current buffer."
	(interactive)
	(if
		(and (boundp 'buffer-face-mode-face)
			(eq buffer-face-mode-face 'fixed-pitch))
		(setq buffer-face-mode-face (buffer-face-set nil))
		(buffer-face-set 'fixed-pitch)))

;; Mask 4 no-X, uniform static dark-only
(defun fg-masq-nox ()
	(interactive)
	(custom-set-faces
		'(default ((t (:foreground "wheat" :background "black"))))
		'(font-lock-comment-face ((t (:foreground "magenta"))))
		'(font-lock-function-name-face ((t (:foreground "red"))))
		'(font-lock-keyword-face ((t (:foreground "green"))))
		'(font-lock-type-face ((t (:foreground "blue"))))
		'(font-lock-string-face ((t (:foreground "cyan"))))
		'(font-lock-variable-name-face ((t (:foreground "blue"))))
		'(menu
			((((type x-toolkit))
				(:background "white"
					:foreground "black"
					:box (:line-width 2 :color "grey75" :style released-button)))))
		'(modeline ((t (:foreground "blue" :background "white")))))
	(set-cursor-color "blue")
	(set-foreground-color "white")
	(set-background-color "black")
	(set-face-foreground 'default "white")
	(set-face-background 'default "black"))


;; Lisp highlights
(when
	(require 'highlight-parentheses nil t)
	(setq-default hl-paren-colors
		'("red1" "OrangeRed1" "orange1" "DarkOrange1")))


;; Lisp namespace (ns) mode

(defvar fg-ns-regex nil
	"A regular expression matching namespace-prefix to shorten.")
(make-variable-buffer-local 'fg-ns-regex)

(defvar fg-ns-symbol "-ns-"
	"String to use as short replacement.")
;; (setq fg-ns-symbol "-ns-")

(defun fg-ns-fontify (beg end)
	(save-excursion
		(goto-char beg)
		(while (re-search-forward fg-ns-regex end t)
			(let* ((o-list (overlays-at (match-beginning 0))) (o (car o-list)))
				(unless (and o (eq (overlay-get o 'type) 'fg-ns))
					(when (cdr o-list) (setq o (seq-some
						(lambda (o) (eq (overlay-get o 'type) 'fg-ns)) (cdr os))))
					(unless o
						(setq o (make-overlay (match-beginning 0) (match-end 0)))
						(overlay-put o 'type 'fg-ns)
						(overlay-put o 'evaporate t)
						(overlay-put o 'display fg-ns-symbol)))))))

(defun fg-ns-unfontify (beg end)
	(mapc
		(lambda (o)
			(when (eq (overlay-get o 'type) 'fg-ns) (delete-overlay o)))
		(overlays-in beg end)))

(define-minor-mode lisp-ns-mode
	"Visually shorten namespace prefix in lisp files."
	:lighter " LP"
	(if (not lisp-ns-mode)
		(progn ; disable
			(jit-lock-unregister 'fg-ns-fontify)
			(fg-ns-unfontify (point-min) (point-max)))
		(setq fg-ns-regex (concat
			(s-chop-suffix ".el"
				(file-name-nondirectory buffer-file-name)) "-"))
		(fg-ns-fontify (point-min) (point-max))
		(jit-lock-register 'fg-ns-fontify)))



;; Set masq depending on time-of-the-day
(require 'solar)
(defvar fg-sunset-timer)
(defvar fg-sunrise-timer)

(defun fg-smart-looks (&optional frame)
	"Automatically switch to dark background after sunset
and to light background after sunrise.
Note that `calendar-latitude' and `calendar-longitude'
should be set before calling the `solar-sunrise-sunset'."
	(interactive)
	(if (and calendar-latitude calendar-longitude calendar-time-zone)
		(let*
			((l (solar-sunrise-sunset (calendar-current-date)))
				(sunrise-string (apply 'solar-time-string (car l)))
				(sunset-string (apply 'solar-time-string (car (cdr l))))
				(current-time-string (format-time-string "%H:%M")))
			(if
				(or (string-lessp current-time-string sunrise-string)
					(string-lessp sunset-string current-time-string))
				(fg-masq-x-dark)
				(fg-masq-x-light))
			(when (and (boundp 'fg-sunset-timer) (timerp fg-sunset-timer))
				(cancel-timer fg-sunset-timer))
			(when (and (boundp 'fg-sunrise-timer) (timerp fg-sunrise-timer))
				(cancel-timer fg-sunrise-timer))
			(setq fg-sunset-timer (run-at-time sunset-string (* 60 60 24) 'fg-masq-x-dark))
			(setq fg-sunrise-timer (run-at-time sunrise-string (* 60 60 24) 'fg-masq-x-light)))))

;; Disabled fg-smart-looks here in favor of always-dark-mode
;; Light more seem to be rarely needed, and more of a nuisance these days
(if window-system
	(fg-masq-x-dark)
	;; (progn
	;; 	(fg-smart-looks)
	;; 	(add-to-list 'after-make-frame-functions 'fg-smart-looks))
	(fg-masq-nox)) ; time-of-the-day independent, since terms should be plain black



;; Auto looks switching
(defun fg-hook-set-looks ()
	"Enable stuff like trailing spacez or fixed-width face."
	(fg-buffer-bg-tweak-name)
	(if buffer-file-name ; nil for system buffers and terminals
		(setq show-trailing-whitespace t)
		(when
			(memq major-mode
				'(term-mode jabber-roster-mode ibuffer-mode
					gnus-group-mode gnus-summary-mode))
			(buffer-face-set 'fixed-pitch))))

(add-hook 'find-file-hook 'fg-hook-set-looks)
(add-hook 'ibuffer-hook 'fg-hook-set-looks)
(add-hook 'after-change-major-mode-hook 'fg-hook-set-looks)
