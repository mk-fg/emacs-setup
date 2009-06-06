;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'setnu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Duplicate line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duplicate-line ()
	(interactive)
	(let
		((pos (point)))
		(progn
			(kill-whole-line)
			(yank) (yank)
			(goto-char pos))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nuke buffer sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nuke-some-buffers (&optional list)
	"For each buffer in LIST, kill it silently if unmodified. Otherwise ask.
LIST defaults to all existing live buffers."
	(interactive)
	(if (null list)
		(setq list (buffer-list)))
	(while list
		(let* ((buffer (car list))
			(name (buffer-name buffer)))
		(and (not (string-equal name ""))
			(not (string-equal name "*Messages*"))
			; (not (string-equal name "*Buffer List*"))
			(not (string-equal name "*buffer-selection*"))
			(not (string-equal name "*Shell Command Output*"))
			(not (string-equal name "*scratch*"))
			(/= (aref name 0) ? )
			(if (buffer-modified-p buffer)
				(if (yes-or-no-p
					(format "Buffer %s has been edited. Kill? " name))
					(kill-buffer buffer))
				(kill-buffer buffer))))
		(setq list (cdr list))))

(defun nuke-all-buffers ()
	"Nuke all buffers, leaving *scratch* only"
	(interactive)
	(mapcar (lambda (x) (kill-buffer x))
		(buffer-list))
	(delete-other-windows))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation descrimination (tab-only) stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smart-tab (prefix)
	"Needs `transient-mark-mode' to be on. This smart tab is
	minibuffer compliant: it acts as usual in the minibuffer.

	In all other buffers: if PREFIX is \\[universal-argument], calls
	`smart-indent'. Else if point is at the end of a symbol,
	expands it. Else calls `smart-indent'."
	(interactive "P")
	(labels
		((smart-tab-must-expand (&optional prefix)
			(unless
				(or (consp prefix) mark-active)
				(looking-at "\\_>"))))
		(cond
			((minibufferp)
				(minibuffer-complete))
			((smart-tab-must-expand prefix)
				(hippie-expand prefix))
			((smart-indent prefix)))))

(defun smart-untab (prefix)
	"Reverse of smart-tab (inverts prefix)"
	(interactive "P")
	(smart-tab (or (and prefix (* -1 prefix)) -1)))

(defun smart-indent (&optional num)
	"Indents region if mark is active, or current line otherwise."
	(interactive "P")
	(if mark-active
		; indent-region is too dumb: can't take NUM argument
		(nazi-tab
			(region-beginning)
			(region-end)
			prefix)
		(indent-for-tab-command)))

(defun nazi-tab (start end &optional num)
	"Tab-only variant of indent-rigidly.
Indent all lines in the region by NUM tabs (\t).
Can be used by indent-region, since NUM defaults to 1."
	(interactive "r\np")
	(save-excursion
		(goto-char end)
		(setq end (point-marker))
		(goto-char start)
		(move-beginning-of-line nil)
		(while (< (point) end)
			(let
				((indent (current-indentation))
					eol-flag
					deactivate-mark)
				(save-excursion
					(skip-chars-forward " \t")
					(setq eol-flag (eolp)))
				(or eol-flag
					(indent-to (max 0 (+ indent (* (or num 1) tab-width)))))
				(delete-region (point) (progn (skip-chars-forward " \t") (point))))
			(forward-line 1))))

