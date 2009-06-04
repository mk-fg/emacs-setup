;; Tabs appearance and formatting basics
(setq indent-tabs-mode t)
(setq default-indent-tabs-mode t)
(setq default-tab-width 4)
(setq tab-width 2)
(setq fill-column 80)
(setq basic-indent 2)
(setq tab-always-indent t)

;; Stop emacs from arbitrarily adding lines to the end of a file when the
;; cursor is moved past the end of it:
(setq next-line-add-newlines nil)

;; Fix the worst part about emacs: indentation hell
;;	1. TAB is TAB, not a whole bunch of fucking spaces
;;	2. Don't automatically indent the line I am editing.
;;	3. When I hit C-j, I always want a newline, plus enough tabs to put me on
;;		the same column I was at before.
;;	4. When I hit the BACKSPACE key to the right of a TAB character, I want the
;;		TAB character deleted-- not replaced with tabwidth-1 spaces.
(defun newline-and-indent-relative ()
	"Insert a newline, then indent relative to the previous line."
	(interactive "*") (newline) (indent-relative))
(defun indent-according-to-mode () ())
(defalias 'newline-and-indent 'newline-and-indent-relative)
(defun my-c-hook ()
	(defalias 'c-electric-backspace 'delete-backward-char)
	(defun c-indent-command () (interactive "*") (self-insert-command 1)))
(add-hook 'c-mode-common-hook 'my-c-hook)

(defun indent-region-with-tab ()
	(interactive)
	(save-excursion
	(if (< (point) (mark)) (exchange-point-and-mark))
	(let ((save-mark (mark)))
		(if (= (point) (line-beginning-position)) (previous-line 1))
		(goto-char (line-beginning-position))
		(while (>= (point) save-mark)
		(goto-char (line-beginning-position))
		(insert "\t")
		(previous-line 1)))))

(defun unindent-region-with-tab ()
	(interactive)
	(save-excursion
	(if (< (point) (mark)) (exchange-point-and-mark))
	(let ((save-mark (mark)))
		(if (= (point) (line-beginning-position)) (previous-line 1))
		(goto-char (line-beginning-position))
		(while (>= (point) save-mark)
		(goto-char (line-beginning-position))
		(if (= (string-to-char "\t") (char-after (point))) (delete-char 1))
		(previous-line 1)))))

; (global-set-key [?\C-x tab] 'indent-region-with-tab)
; (global-set-key [f4] 'indent-region-with-tab)

(global-set-key (kbd "TAB") 'self-insert-command)
