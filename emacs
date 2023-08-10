;;;; Hack to show lines where Compile-Log warnings happen on emacs startup
;; (defun dont-delay-compile-warnings (fun type &rest args)
;; 	(if (eq type 'bytecomp)
;; 		(let ((after-init-time t)) (apply fun type args))
;; 		(apply fun type args)))
;; (advice-add 'display-warning :around #'dont-delay-compile-warnings)

(package-initialize) ; package.el wants this

(defvar fg-path
	(expand-file-name "~/.emacs.d")
	"root for all emacs-related crap")

(defcustom fg-emacs-exit-hook ()
	"Called before exiting emacs to run any cleanups and confirmation-killing stuff."
	:type 'hook)

(defun fg-emacs-exit ()
	"Run `fg-emacs-exit-hook' to cleanup stray pid/tasks and exit emacs."
	(interactive)
	(run-hooks 'fg-emacs-exit-hook)
	(save-buffers-kill-terminal))

;; Removes "buffer "* temp*" has process associated with it" query
;; No idea why it pops-up on some boots, can't reproduce without crash/reboots
;; See also: process-query-on-exit-flag
(setq kill-buffer-query-functions
	(delq 'process-kill-buffer-query-function kill-buffer-query-functions))


;; Extend include path
(add-to-list 'load-path (concat fg-path "/core"))
(add-to-list 'load-path (concat fg-path "/extz"))
(add-to-list 'load-path (concat fg-path "/extz/yasnippet"))


;; Basic loading macros
(defmacro load-library-safe (name &optional msg)
	"Condition-wrapped inclusion of unsafe el code"
	`(condition-case err
		(load-library ,name)
		(error (progn (message ,(or msg "Failed to load %s: %s") ,name err) nil))))
(setq-default vc-follow-symlinks t) ;; it's just a noise anyway


(load-library "fg_macros")


;; Auth data
(load-library "fg_sec")
(condition-case err
	(save-excursion
		(find-file (concat fg-path "/auth.el.ghg"))
		(eval-buffer)
		(kill-buffer))
	(file-error))
(setq-default
	auth-sources (list (concat fg-path "/authrc.el.ghg")))


;; Temp/spool path init
(setq-default
	temporary-file-directory
		(concat fg-path "/tmp/"))
(make-directory temporary-file-directory t)
(set-file-modes temporary-file-directory #o700)


(load-library "fg_stack")
(load-library "fg_looks")
(load-library "fg_lisp")
(load-library "fg_diff")
(load-library "fg_remote")


; External and non-critical
(autoload 'multi-term "fg_shell" nil t)
(dolist
	(sym '(fg-jabber-activity-reset jabber-activity-switch-to
		jabber-connect jabber-connect-all))
	(autoload sym "fg_jabbra" nil t))
(dolist
	(sym
		'(emms emms-stop emms-pause emms-shuffle
			emms-next emms-previous
			fg-emms-playlist-new fg-emms-playlist-cycle
			fg-emms-add-file-glob fg-emms-add-directory-tree-glob
			emms-add-playlist emms-playlist-save
			emms-playlist-mode-clear fg-emms-notify))
	(autoload sym "fg_emms" nil t))
(dolist
	(sym
		'(erc erc-tls erc-bol fg-erc
			fg-erc-track-reset erc-track-switch-buffer))
	(autoload sym "fg_erc" nil t))
(autoload 'gnus "fg_gnus" nil t)
(dolist
	(sym '(w3m w3m-region w3m-goto-url w3m-browse-url))
	(autoload sym "fg_w3m" nil t))
(dolist
	(sym '(fg-feeds newsticker-start
		newsticker-start-ticker newsticker-show-news))
	(autoload sym "fg_newsticker" nil t))
(autoload 'notifications-notify "notifications" nil t)
(eval-after-load "python-mode" (load-library "fg_py"))
(autoload 'mmap "minimap" nil t)


;; Compositing stuff
(load-library "fg_style")
(load-library "fg_keys") ; must be the last one of fg_* stuff

;; ELPA
(load-library "fg_elpa")

;; "customize" settings (if any) - should be after everything else, ideally
(setq custom-file (concat fg-path "/customize.el"))
(when (file-readable-p custom-file) (load custom-file))


;; Adjust tmp path and use it for all backup and autosave files
(require 'saveplace)
(desktop-save-mode)

(setq-default
	; annoying .#filename symlinks on file modification
	create-lockfiles nil
	; autosave - default-disabled, but configured a bit
	auto-save-default nil
	auto-save-list-file-prefix
		(concat temporary-file-directory "bakz-")
	auto-save-file-name-transforms
		(list (cons ".*" (list temporary-file-directory t)))
	; vc integration - hopefully disabled
	;vc-handled-backends nil
	; backups - don't do these
	make-backup-files nil
	backup-inhibited t
	backup-directory-alist nil
	; save-place-in-file
	save-place t
	save-place-file
		(concat temporary-file-directory "placez")
	; buffer list storage
	desktop-dirname temporary-file-directory
	desktop-path (list temporary-file-directory)
	desktop-base-file-name "bufferz"
	desktop-load-locked-desktop t ; bogus check
	desktop-save t)

;; Obligatory timer to save desktop every now and then - crashes do happen
(defvar fg-desktop-autosave-timer
	(run-at-time t 600 ;; 10 min
		 (lambda ()
				(setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
				(desktop-save-in-desktop-dir)))
	"Repetitive timer calling `desktop-save-in-desktop-dir', suppressing any queries.")

(add-hook 'desktop-after-read-hook 'ibuffer)


;; Default behavior tweaks
(fset 'yes-or-no-p 'y-or-n-p) ; use y or n instead of yes or no

(delete-selection-mode t) ; delete active selection w/ transient-mode
(mouse-wheel-mode t) ; ...in case I plug the rodent in
(recentf-mode t) ; TODO: bind keys to use it

(setq-default
	message-log-max 1000 ; *Messages* scrollback
	next-line-add-newlines nil ; don't move past eof
	x-select-enable-clipboard t ; shared clipboard should always be enabled
	compare-windows-sync t) ; advance point in both buffers on comparison


;; Vars not declared "safe" by modes, invoking hack-local-variables-confirm
;; Simple way to expand this list is "!" on confirm and fishing them from customize.el
(add-to-list 'ignored-local-variables 'test-case-name)
(add-to-list 'ignored-local-variables 'jinja2)
(add-to-list 'safe-local-variable-values '(encoding . utf-8))
(setq-default enable-local-variables :safe) ;; auto-ignore all others


;; Misc hooks
(defadvice kill-buffer (around fg-kill-buffer-persistent-scratch activate)
	"Bury *scratch* buffer instead of killing it."
	(if
		(equal (or (ad-get-arg 0) (buffer-name)) "*scratch*")
		(bury-buffer)
		ad-do-it))

;; Emacs server (client is bound to zsh ec/ece aliases)
(server-start)

;; Starting layout setup
(split-window-horizontally)

(unless
	(or
		(not (eq window-system 'x))
		(eq (frame-parameter nil 'fullscreen) 'maximized))
	(set-frame-parameter nil 'fullscreen 'maximized))
