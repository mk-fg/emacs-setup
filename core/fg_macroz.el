;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'setnu)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line/word ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-line (&optional arg)
	"Copy current line into ring buffer.
ARG is interpreted as in kill-whole-line."
	(interactive "p")
	(or arg (setq arg 1))
	(if
		(and (> arg 0)
			(eobp)
			(save-excursion (forward-visible-line 0) (eobp)))
		(signal 'end-of-buffer nil))
	(if
		(and (< arg 0)
			(bobp)
			(save-excursion (end-of-visible-line) (bobp)))
		(signal 'beginning-of-buffer nil))
	(unless (eq last-command 'kill-region)
		(kill-new "")
		(setq last-command 'kill-region))
	(if (< arg 0) (setq arg (1+ arg)))
	(save-excursion
		(copy-region
			(progn (forward-visible-line 0) (point))
			(progn (forward-visible-line arg) (point)))))

(defun duplicate-line ()
	"Clone line at cursor, leaving the latter intact."
	(interactive)
	(save-excursion
		(let (deactivate-mark)
			(copy-line)
			(forward-line 1)
			(if (eobp) (newline))
			(yank))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart kill/delete ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sdel-word (arg)
	"Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times.
Negative arg reverses direction."
	(interactive "p")
	(delete-region (point) (progn (forward-word arg) (point))))

(defun sdel-word-backwards (arg)
	"Remove chars before point to until the beginning of a word.
Safe for read-only buffer parts (like prompts). See also delete-word."
	(interactive "p")
	(save-excursion
		(let
			((kill-read-only-ok t) deactivate-marker)
			(sdel-word (- arg)))))

(defun sdel-char (arg)
	(interactive "p")
	(if mark-active
		(delete-region
			(region-beginning)
			(region-end))
		(delete-char arg)))

(defun sdel-char-backwards (arg)
	(interactive "p")
	(sdel-char (- arg)))


(defun skill-line-backwards ()
	"Blank current line."
	(interactive)
	(execute-kbd-macro (kbd "<home>")) ; mode-safe
	(or (eolp) (kill-line)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Region ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-region (start end)
	"Same as copy-region-as-kill but doesn't deactivate the mark."
	(interactive "r")
	(if (eq last-command 'kill-region)
		(kill-append (filter-buffer-substring start end) (< end start))
		(kill-new (filter-buffer-substring start end))))



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

(defun smart-tab (arg)
	"Needs `transient-mark-mode' to be on. This smart tab is
	minibuffer compliant: it acts as usual in the minibuffer.

	In all other buffers: if ARG is \\[universal-argument], calls
	`smart-indent'. Else if point is at the end of a symbol,
	expands it. Else calls `smart-indent'."
	(interactive "p")
	(labels
		((smart-tab-must-expand (&optional arg)
			(unless
				(or (consp arg) mark-active)
				(looking-at "\\_>"))))
		(cond
			((minibufferp)
				(minibuffer-complete))
			((smart-tab-must-expand arg)
				(or (hippie-expand arg)
					(smart-indent arg)))
			(t
				(smart-indent arg)
				(skip-chars-forward " \t")))))

(defun smart-untab (arg)
	"Reverse of smart-tab (just inverts arg)."
	(interactive "p")
	(smart-tab (- arg)))

(defun smart-indent (arg)
	"Indents region if mark is active, or current line otherwise."
	(interactive "p")
	(if mark-active
		; indent-region is too dumb: can't take ARG
		(nazi-tab-region
			(region-beginning)
			(region-end)
			arg)
		(nazi-tab arg)))

(defun nazi-tab-region (start end &optional arg)
	"Tab-only variant of indent-rigidly.
Indent all lines in the region by ARG tabs (\t).
Can be used by indent-region, since ARG defaults to 1."
	(interactive "r\np") ; TODO: learn what that means ;)
	(save-excursion
		(goto-char end)
		(setq end (point-marker))
		(goto-char start)
		(move-beginning-of-line nil)
		(while (< (point) end)
			(nazi-tab-indent arg t)
			(forward-line 1))))

(defun nazi-tab (arg)
	"Indent current line regardless of point position."
	(interactive "p")
	(save-excursion
		(move-beginning-of-line nil)
		(nazi-tab-indent arg)))

(defun nazi-tab-indent (arg &optional check-eol)
	"Insert ARG tabs w/o deactivating mark if point is in the indent zone.
If check-eol is set and line is just indent zone, it'll be blanked."
	(interactive "p")
	(let
		((indent (current-indentation))
			deactivate-mark)
		(if check-eol
			(save-excursion
				(skip-chars-forward " \t")
				(setq arg (if (eolp) 0 arg))))
		(indent-to (max 0 (+ indent (* arg tab-width))))
		(delete-region (point) (progn (skip-chars-forward " \t") (point)))))
