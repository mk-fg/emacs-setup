;; Extend include path
(setq load-path (cons "~/.emacs.d" load-path))


;; Supress stupid save behavior
(setq make-backup-files nil)
(setq auto-save-mode nil)


;; Lisp stuff
(load-library "appearance")
(load-library "lisp")
(load-library "shell")
(load-library "code_style")
(load-library "keymap") ; must be the last one


;; Always end a file with a newline
(setq require-final-newline t)

;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)


;; tmp / bakz
(setq
	backup-by-copying t ; don't clobber symlinks
	backup-directory-alist
	'(("." . "~/.emacs.d/bakz"))
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	version-control t) ; use versioned backups
(setq temporary-file-directory "~/tmp")


;; Self-composition
(defun autocompile nil
  "compile itself if ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (if (string= (buffer-file-name) (expand-file-name (concat
		default-directory ".emacs")))
      (byte-compile-file (buffer-file-name))))
(add-hook 'after-save-hook 'autocompile)
