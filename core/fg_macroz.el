;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'setnu)

;; TODO: steal upper/lower case from 'misc



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line/word ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fg-copy-line (&optional arg)
	"Copy current line into ring buffer.
ARG is interpreted as in kill-whole-line, nil is parsed to 1."
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
		(fg-copy-region
			(progn (forward-visible-line 0) (point))
			(progn (forward-visible-line arg) (point)))))

(defun fg-clone (arg)
	"If no region is active - clone current line.
If only part of a single line is selected - clone it inline.
Otherwise, clone all lines, tainted (even partly) by region.
ARG specifies the number of copies to make."
	(interactive "p")
	(save-excursion
		(let (deactivate-mark)
			(if mark-active
				(let
					((start (region-beginning))
						(end (region-end)))
					(if (= (count-lines start end) 1)
						(fg-copy-region start end)
						(fg-copy-region
							(progn
								(goto-char start)
								(forward-line 0)
								(point))
							(progn
								(goto-char end)
								(if (/= (current-column) 0) (forward-line 1))
								(if (eobp) (newline))
								(point)))))
				(progn
					(fg-copy-line)
					(forward-line 1)
					(if (eobp) (newline))))
			(while (> arg 0)
				(yank)
				(setq arg (1- arg))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart kill/delete ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fg-del-word (arg)
	"Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times.
Negative arg reverses direction."
	(interactive "p")
	(delete-region (point) (progn (forward-word arg) (point))))

(defun fg-del-word-backwards (arg)
	"Remove chars before point to until the beginning of a word.
Safe for read-only buffer parts (like prompts). See also delete-word."
	(interactive "p")
	(save-excursion
		(let
			((kill-read-only-ok t) deactivate-marker)
			(fg-del-word (- arg)))))

(defun fg-del-char (arg)
	(interactive "p")
	(if mark-active
		(delete-region
			(region-beginning)
			(region-end))
		(delete-char arg)))

(defun fg-del-char-backwards (arg)
	(interactive "p")
	(fg-del-char (- arg)))


(defun fg-kill-line-blank ()
	"Blank current line, mode-safe."
	(interactive)
	(execute-kbd-macro (kbd "<home>"))
	(or (eolp) (kill-line)))

(defun fg-kill-line-backwards ()
	"Remove text before cursor, mode-safe."
	(interactive)
	(or (bolp)
		(kill-region
			(point)
			(progn
				(execute-kbd-macro (kbd "<home>"))
				(point)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skimming ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: sort this mess out

; (defadvice fg-page-up (around fga-page-up first act)
  ; "Keep cursor in the same column."
  ; (let ((col (current-column)))
    ; ad-do-it
    ; (move-to-column col)))

; (defadvice fg-page-down (around fga-page-down first act)
  ; "Keep cursor in the same column."
  ; (let ((col (current-column)))
    ; ad-do-it
    ; (move-to-column col)))

(defun fg-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
	(interactive)
	(let ((oldpos (point)))
		(back-to-indentation)
		(and (= oldpos (point))
			(beginning-of-line))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Region ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fg-copy-region (start end)
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

(defun fg-tab (arg)
	"Needs `transient-mark-mode' to be on. This smart tab is
	minibuffer compliant: it acts as usual in the minibuffer.

	In all other buffers: if ARG is \\[universal-argument], calls
	`smart-indent'. Else if point is at the end of a symbol,
	expands it. Else calls `smart-indent'."
	(interactive "p")
	(labels
		((fg-tab-must-expand (&optional arg)
			(unless
				(or (consp arg) mark-active)
				(looking-at "\\_>"))))
		(cond
			((minibufferp)
				(minibuffer-complete))
			((fg-tab-must-expand arg)
				(or (hippie-expand arg)
					(fg-indent arg)))
			(t
				(fg-indent arg)
				(skip-chars-forward " \t")))))

(defun fg-untab (arg)
	"Reverse of fg-tab (just inverts arg)."
	(interactive "p")
	(fg-tab (- arg)))

(defun fg-indent (arg)
	"Indents region if mark is active, or current line otherwise."
	(interactive "p")
	(if mark-active
		; indent-region is too dumb: can't take ARG
		(fg-indent-region
			(region-beginning)
			(region-end)
			arg)
		(fg-indent-line arg)))

(defun fg-indent-region (start end &optional arg)
	"Tab-only variant of indent-rigidly.
Indent all lines in the region by ARG tabs (\t).
Can be used by indent-region, since ARG defaults to 1."
	(interactive "r\np") ; TODO: learn what that means ;)
	(save-excursion
		(goto-char end)
		(setq end (point-marker))
		(goto-char start)
		(forward-line 0)
		(while (< (point) end)
			(fg-indent-command arg t)
			(forward-line 1))))

(defun fg-indent-line (arg)
	"Indent current line regardless of point position."
	(interactive "p")
	(save-excursion
		(forward-line 0)
		(fg-indent-command arg)))

(defun fg-indent-command (arg &optional check-eol)
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
