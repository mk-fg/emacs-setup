;; Extend include path
(setq load-path (cons "~/.emacs.d" load-path))


;; Supress stupid save behavior
(setq make-backup-files nil)
(setq auto-save-mode nil)


;; Lisp stuff
(load-library "appearance")
(load-library "lisp")
(load-library "shell")
(load-library "keymap")



;; Text and the such
;; Use colors to highlight commands, etc.
;(global-font-lock-mode t)
;; Disable the welcome message
;(setq inhibit-startup-message t)
;; Format the title-bar to always include the buffer name
;(setq frame-title-format "emacs - %b")
;; Display time
;(display-time)
;; Make the mouse wheel scroll Emacs
;(mouse-wheel-mode t)
;; Always end a file with a newline
;(setq require-final-newline t)
;; Stop emacs from arbitrarily adding lines to the end of a file when the
;; cursor is moved past the end of it:
;(setq next-line-add-newlines nil)
;; Flash instead of that annoying bell
;(setq visible-bell t)
;; Remove icons toolbar
;(if (> emacs-major-version 20)
;(tool-bar-mode -1))
;; Use y or n instead of yes or not
;(fset 'yes-or-no-p 'y-or-n-p)