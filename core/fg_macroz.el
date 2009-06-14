;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy/Cut/Paste ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fg-copy-region (start end)
	"Same as `copy-region-as-kill' but doesn't deactivate the mark."
	(interactive "r")
	(if (eq last-command 'kill-region)
		(kill-append (filter-buffer-substring start end) (< end start))
		(kill-new (filter-buffer-substring start end))))


(defun fg-taint (&optional func whole-lines-only)
	"Smart region interpreter.
If nothing is marked, work on the whole current line.
If part of a single line is marked, apply FUNC to this part, unless second
argument is set.
Otherwise, apply FUNC to all lines, tainted by the region.
If FUNC isn't specified, return (START END) of tainted zone.
Point is moved to the end of affected zone before the call."
	(setq func (or func 'list))
	(if (use-region-p)
		(let
			((start (region-beginning))
				(end (region-end)))
			(if
				(unless whole-lines-only
					(= (count-lines start end) 1))
				(funcall func
					start
					(progn
						(goto-char end)
						(point)))
				(funcall func
					(progn
						(goto-char start)
						(line-beginning-position))
					(progn
						(goto-char end)
						(if (/= (current-column) 0) (forward-line 1))
						(point)))))
		(progn
			(funcall func
				(line-beginning-position)
				(progn
					(forward-line 1)
					(point))))))


(defun fg-copy ()
	"Push selected region or current line into ring-buffer."
	(interactive)
	(save-excursion
		(let (deactivate-mark)
			(if (use-region-p)
				(fg-copy-region
					(region-beginning)
					(region-end))
				(fg-taint 'fg-copy-region)))))


(defun fg-copy-paragraph ()
	"Copy full paragraph at the point."
	(interactive)
	(unless 
		(and (not (bobp))
			(looking-at paragraph-start))
		(backward-paragraph))
	(fg-copy-region
	 (point)
	 (progn (forward-paragraph) (point))))


(defun fg-clone (arg)
	"If no region is active - clone current line.
If only part of a single line is selected - clone it inline.
Otherwise, clone all lines, tainted (even partly) by the region.
ARG specifies the number of copies to make."
	(interactive "p")
	(save-excursion
		(let (deactivate-mark)
			(fg-taint 'fg-copy-region)
			(if (eobp) (newline))
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
Safe for read-only buffer parts (like prompts). See also `fg-del-word'."
	(interactive "p")
	(save-excursion
		(let
			((kill-read-only-ok t) deactivate-marker)
			(fg-del-word (- arg)))))

(defun fg-del-char (arg)
	"Delete-key-function."
	(interactive "p")
	(if (region-active-p)
		(delete-region
			(region-beginning)
			(region-end))
		(delete-char arg)))

(defun fg-del-char-backwards (arg)
	"Backspace-key-function."
	(interactive "p")
	(fg-del-char (- arg)))


(defun fg-del-whole-line ()
	"Like `kill-whole-line', but w/o ring-buffer."
	(interactive)
	(delete-region
		(line-beginning-position)
		(progn (forward-line 1) (point))))
	

(defun fg-kill ()
	"Kill region or line."
	(interactive)
	(if (use-region-p)
		(kill-region
			(region-beginning)
			(region-end))
		(kill-whole-line)))

(defun fg-kill-line-blank ()
	"Blank current line, mode-safe."
	(interactive)
	(execute-kbd-macro (kbd "<home>"))
	(or (eolp) (kill-line)))

(defun fg-kill-line ()
	"Remove text after cursor, mode-safe."
	(interactive)
	(or (bolp)
		(kill-region
			(point)
			(progn
				(execute-kbd-macro (kbd "<end>"))
				(point)))))

(defun fg-kill-line-backwards ()
	"Remove text before cursor, mode-safe."
	(interactive)
	(or (bolp)
		(kill-region
			(point)
			(progn
				(execute-kbd-macro (kbd "<home>"))
				(point)))))


(defun fg-kill-whole-paragraph ()
	"Remove full paragraph at the point."
	(interactive)
	(unless 
		(and (not (bobp))
			(looking-at paragraph-start))
		(backward-paragraph))
	(kill-paragraph nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skimming ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fg-scroll-up (arg)
	"Scroll or move cursor ARG pages up."
	(interactive "^p")
	(if
		(/= (window-start) (point-min))
		(scroll-down) ; named for convenience, obviously
		(let (deactivate-mark)
			(goto-char (point-min)))))

(defun fg-scroll-down (arg)
	"Scroll or move cursor ARG pages down."
	(interactive "^p")
	(if
		(/= (window-end) (point-max))
		(scroll-up) ; named for convenience, obviously
		(let (deactivate-mark)
			(goto-char (point-max)))))

(defun fg-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
	(interactive "^")
	(let ((oldpos (point)))
		(back-to-indentation)
		(and (= oldpos (point))
			(beginning-of-line))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nuke buffer sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fg-nuke-some (&optional list)
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

(defun fg-nuke-all ()
	"Nuke all buffers, leaving *scratch* only"
	(interactive)
	(mapc (lambda (x) (kill-buffer x))
		(buffer-list))
	(delete-other-windows))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fg-wtf ()
	"Find whatever I'm pointing at."
	(interactive)
	(cond
		((null (eq (variable-at-point) 0))
			(describe-variable (variable-at-point)))
		((function-called-at-point)
			(describe-function (function-called-at-point)))
		(t (find-file-at-point))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation descrimination (tab-only) stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq hippie-expand-try-functions-list
  '(try-complete-file-name-partially
    try-complete-file-name
    try-expand-all-abbrevs
    try-expand-line
    try-expand-dabbrev
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-expand-list ; it is a disaster w/ large lists, hence the place
    try-complete-lisp-symbol-partially
    try-complete-lisp-symbol))

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
				(or (consp arg) (use-region-p))
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
	"Reverse of `fg-tab' (just inverts arg)."
	(interactive "p")
	(fg-tab (- arg)))

(defun fg-indent (arg)
	"Indents region if mark is active, or current line otherwise."
	(interactive "p")
	(if (use-region-p)
		; indent-region is too dumb: can't take ARG
		(fg-indent-region
			(region-beginning)
			(region-end)
			arg)
		(fg-indent-line arg)))

(defun fg-indent-region (start end &optional arg)
	"Tab-only variant of `indent-rigidly'.
Indent all lines in the region by ARG tabs (\t).
Can be used by `indent-region', since ARG defaults to 1."
	(interactive "r\np")
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
If CHECK-EOL is set and line is just indent zone, it'll be blanked."
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

(defun fg-newline ()
	"Mode-safe version of newline-and-indent."
  (interactive)
  (or buffer-read-only (minibufferp) (delete-horizontal-space t))
  (newline)
  (or buffer-read-only (minibufferp) (indent-according-to-mode)))

;; Comment-tabulata
(defun fg-comment (arg)
	(interactive "*P")
	(let
		((start-m (or (use-region-p) (point-marker)))
			(taint (fg-taint nil t)))
		(push-mark (car taint) t t)
		(goto-char (car (last taint)))
		(comment-dwim arg)
		(if (markerp start-m) (progn (deactivate-mark) (goto-char (marker-position start-m))))))

