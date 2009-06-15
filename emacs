(defvar fg-path
	(expand-file-name "~/.emacs.d")
	"root for all emacs-related crap")

;; Extend include path
(add-to-list 'load-path fg-path)
(add-to-list 'load-path (concat fg-path "/extz"))
(add-to-list 'load-path (concat fg-path "/core"))


;; Includes
(load-library "fg_lisp") ; core language tweaks, should be first
(load-library "fg_macroz")
(load-library "fg_lookz")
(load-library "fg_shell")
(load-library "fg_style")
(load-library "fg_keyz") ; must be the last one


;; Adjust tmp path and use it for all backup and autosave files
(require 'saveplace)
(desktop-save-mode)
(setq-default
	temporary-file-directory
		(concat fg-path "/tmp/")
	; autosave
	auto-save-list-file-prefix
		(concat temporary-file-directory "bakz-")
	auto-save-file-name-transforms
		(list (cons ".*" (list temporary-file-directory t)))
	; backups
	backup-directory-alist
		(list (cons "." temporary-file-directory))
	backup-by-copying t
	delete-old-versions t
	version-control t
	kept-new-versions 6
	kept-old-versions 2
	; save-place-in-file
	save-place t
	save-place-file
		(concat temporary-file-directory "placez")
	; buffer list storage
	desktop-dirname temporary-file-directory
	desktop-path (list temporary-file-directory)
	desktop-base-file-name "bufferz"
	desktop-load-locked-desktop t ; bogus
	desktop-save t)

(make-directory temporary-file-directory t)


;; Default behavior tweaks
(fset 'yes-or-no-p 'y-or-n-p) ; use y or n instead of yes or no

(delete-selection-mode) ; delete active selection w/ transient-mode
(mouse-wheel-mode)
(auto-image-file-mode)
(recentf-mode) ; TODO: bind keys to use it

(setq-default
	next-line-add-newlines nil ; don't move past eof
	x-select-enable-clipboard t ; shared clipboard should always be enabled
	compare-windows-sync t ; advance point in both buffers on comparison
	; find-file tweaks
	find-file-run-dired nil
	find-file-visit-truename t
	find-file-existing-other-name t)


;; Self-composition
(defun autocompile ()
  "Compile itself if ~/.emacs or .emacs.d include."
  (interactive)
  (require 'bytecomp)
  (if
		(string-match
			"/\\.?emacs\\(\\.d/.*\\.el\\)?$"
			(buffer-file-name))
		(byte-compile-file (buffer-file-name))))
(add-hook 'after-save-hook 'autocompile)

;; Bury *scratch* buffer instead of killing it
(defadvice kill-buffer (around fg-kill-buffer-persistent-scratch activate)
	(if
		(equal (or (ad-get-arg 0) (buffer-name)) "*scratch*")
		(bury-buffer)
		ad-do-it))

;; Auto lookz/keyz switching
(add-hook 'minibuffer-setup-hook 'fg-scite-aux)
(defun fg-hook-set-mode ()
	"Turn fg-scite-code mode on explicitly"
	(if buffer-file-name ; nil for system buffers and terminals
		(unless fg-scite-code
			(fg-scite-code)
			(setq show-trailing-whitespace t))
		(or
			(when
				(and (eq major-mode 'term-mode) ; term-mode minors can probably also be set via multi-term
					(null fg-scite-term))
				(fg-scite-term)
				(buffer-face-set 'fixed-pitch))
			(when
				(and (eq major-mode 'help-mode)
					(null fg-scite-aux))
				(fg-scite-aux)))))
(add-hook 'find-file-hook 'fg-hook-set-mode)
(add-hook 'after-change-major-mode-hook 'fg-hook-set-mode)

;; TODO: look out for (server-start) function
;; TODO: look at http://infolab.stanford.edu/~manku/dotemacs.html
