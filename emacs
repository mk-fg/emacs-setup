(defvar fg-path
	(expand-file-name "~/.emacs.d")
	"root for all emacs-related crap")

;; Extend include path
(add-to-list 'load-path fg-path)
(add-to-list 'load-path (concat fg-path "/extz"))
(add-to-list 'load-path (concat fg-path "/core"))


;; Includes
(load-library "fg_macroz"); should be first
(load-library "fg_lookz")
(load-library "fg_lisp")
(load-library "fg_shell")
(load-library "fg_style")
(load-library "fg_keyz") ; must be the last one


;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)
;; Shared clipboard should always be enabled
(setq x-select-enable-clipboard t)
;; Keep vertical cursor pos during PgUp / PgDn
(setq scroll-preserve-screen-position t)

;; Adjust tmp path and use it for all backup and autosave files
; (require ’saveplace)
(setq
	temporary-file-directory
		(concat fg-path "/tmp/")
	; autosave
	auto-save-list-file-prefix
		(concat temporary-file-directory "/bakz-")
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
		(concat fg-path "/tmp/placez"))

(make-directory temporary-file-directory t)

;; Self-composition
(defun autocompile nil
  "compile itself if ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (if (string= (buffer-file-name) (expand-file-name (concat
		default-directory ".emacs")))
      (byte-compile-file (buffer-file-name))))
(add-hook 'after-save-hook 'autocompile)

;; TODO: look out for (server-start) function
;; TODO: look at http://infolab.stanford.edu/~manku/dotemacs.html
