;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metacode stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fg-apply-partially-append (fun &rest args)
	"Like `apply-partially', but appends arguments to a wrapped call."
	(lexical-let ((fun fun) (args1 args))
		(lambda (&rest args2) (apply fun (append args2 args1)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy/Cut/Paste ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice push-mark (before fg-push-mark-silence activate)
	"Supress minibuffer 'Mark set' messages on kill-ring ops."
	(ad-set-arg 1 t))


(defun fg-copy-region (start end)
	"Same as `copy-region-as-kill' but doesn't deactivate the mark."
	(interactive "r")
	(if (eq last-command 'kill-region)
		(kill-append (filter-buffer-substring start end) (< end start))
		(kill-new (filter-buffer-substring start end))))


(defun* fg-taint (&key call whole-lines-only)
	"Smart region interpreter.
If nothing is marked, work on the whole current line.
If part of a single line is marked, apply CALL to this part, unless second
argument is set.
Otherwise, apply CALL to all lines, tainted by the region.
If CALL isn't specified, return (START END) of tainted zone.
Point is moved to the end of affected zone before the call."
	(setq call (or call 'list))
	(if (use-region-p)
		(let
			((start (region-beginning))
				(end (region-end)))
			(if
				(unless whole-lines-only
					(= (count-lines start end) 1))
				(funcall call
					start
					(progn
						(goto-char end)
						(point)))
				(funcall call
					(progn
						(goto-char start)
						(line-beginning-position))
					(progn
						(goto-char end)
						(if (/= (current-column) 0) (forward-line 1))
						(point)))))
		(progn
			(funcall call
				(line-beginning-position)
				(progn
					(forward-line 1)
					(point))))))


(defun fg-copy (&optional whole-lines-only)
	"Push selected region or current line into ring-buffer."
	(interactive)
	(save-excursion
		(let (deactivate-mark)
			(if (and (use-region-p) (not whole-lines-only))
				(fg-copy-region
					(region-beginning)
					(region-end))
				(fg-taint
					:call 'fg-copy-region
					:whole-lines-only whole-lines-only)))))


(defun fg-copy-paragraph (&optional arg)
	"Copy full paragraph at the point."
	(interactive "p")
	(mark-paragraph arg)
	(forward-line 1) ; skip past blank opening line
	(fg-copy))


(defun fg-clone (arg)
	"If no region is active, clone current line.
If only part of a single line is selected, clone it inline.
Otherwise, clone all lines, tainted (even partly) by the region.
ARG specifies the number of copies to make.
Doesn't pollute the kill-ring, respects x-clip-buffer."
	(interactive "p")
	(save-excursion
		(let
			((x-kill
				(and interprogram-paste-function
					(funcall interprogram-paste-function))))
			(when x-kill
				(push x-kill kill-ring)
				(setq kill-ring-yank-pointer kill-ring)))
		(let
			(deactivate-mark
				kill-ring kill-ring-yank-pointer)
			(fg-taint :call 'fg-copy-region)
			(if (unless (bolp) (eobp)) (newline))
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

(defun fg-kill-whole-paragraph (&optional arg)
	"Remove full paragraph at the point."
	(interactive "p")
	(mark-paragraph arg)
	(fg-kill))



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

(defun fg-beginning-of-line (&optional force-to-indent)
  "Move point to first non-whitespace character or beginning-of-line.
Generic way to do this is via `back-to-indentation' or `beginning-of-line',
but special checks are in place for non-standard buffers like SLIME or ERC,
which invoke functions like `slime-repl-bol' or `erc-bol' instead."
	(interactive "^")
	(case major-mode
		('slime-repl-mode (slime-repl-bol))
		('erc-mode (erc-bol))
		(t (let ((oldpos (point)))
			(back-to-indentation)
			(when
				(and
					(= oldpos (point))
					(not force-to-indent))
				(beginning-of-line))))))

(defun fg-end-of-line ()
	"Move point to the last non-whitespace character in line or bol."
	(interactive)
	(end-of-line)
	(skip-syntax-backward " " (line-beginning-position)))

(defun fg-point-to-reg (arg)
	"Store current buffer `point' position to register, announcing register id."
	(interactive "^p")
	(message "Set register %d" arg)
	(point-to-register arg))

(defun fg-point-from-reg (arg)
	"Restore current buffer `point' position from register, announcing register id."
	(interactive "^p")
	(message "Restored from register %d" arg)
	(jump-to-register arg))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fg-wtf ()
	"Find info on whatever I'm pointing at."
	(interactive)
	(when (eq major-mode 'help-mode) (view-mode-exit))
	(cond
		((and
				(eq major-mode 'python-mode)
				(functionp 'pylookup-lookup))
			(pylookup-lookup
				(if (use-region-p)
					(buffer-substring
						(region-beginning) (region-end))
					(thing-at-point 'word))))
		((not (eq (variable-at-point) 0))
			(describe-variable (variable-at-point)))
		((function-called-at-point)
			(describe-function (function-called-at-point)))
		(t (find-file-at-point))))

(defun fg-nuke-all ()
	"Nuke all buffers, leaving *scratch* only"
	(interactive)
	(mapc (lambda (x) (kill-buffer x))
		(buffer-list))
	(delete-other-windows))

(defun fg-recentf-prompt ()
	"Completion prompt of recentf list in minibuffer, using only files' basename."
	(interactive)
	(let*
		((tocpl
				(mapcar (lambda (x) (cons (file-name-nondirectory x) x)) recentf-list))
			(fname (completing-read "Recent file: " tocpl nil nil)))
		(when fname
			(find-file (cdr (assoc-string fname tocpl))))))


;; These are to check if X window is active, result is cached since shell-call
;;  is blocking and will disrupt emacs activity on frequent invocation
(defvar fg-xactive-check-interval 60)
(defvar fg-xactive-check-time (float-time))
(defvar fg-xactive-check-result nil)

(defun fg-xactive-check (&optional force)
	(let ((time (float-time)))
		(when
			(or force (> time
				(+ fg-xactive-check-time fg-xactive-check-interval)))
			(setq fg-xactive-check-result
				(and
					(eq window-system 'x)
					(=
						(string-to-number
							(shell-command-to-string
								"exec xdotool getactivewindow getwindowpid"))
						(emacs-pid)))
				fg-xactive-check-time time))
		fg-xactive-check-result))

(defun fg-idle-time ()
	(if (eq window-system 'x)
		(string-to-number (shell-command-to-string "exec xprintidle")) 0))

(defun fg-pixmap-path (name)
	(block :loop
		(dolist
			(path
				(list (concat fg-path "/pixmaps/" name)
					(concat fg-path "/" name)
					(concat (expand-file-name "~/.pixmaps/") name)
					name)
				name)
			(progn
				(when (file-exists-p (concat path ".png"))
					(return-from :loop (concat path ".png")))
				(when (file-exists-p path)
					(return-from :loop path))))))


(defvar fg-notify-never-escape nil
	"Never escape html entities in notification functions")

(defun* fg-notify
	(header message &key pixmap urgency strip dont-escape)
	"Send desktop notification about event.
PIXMAP specifies an icon to use.
URGENCY can be set to 'low or 'critical.
STRIP can be specified to trim whitespace chars from text.
DONT-ESCAPE inhibits escaping html entities in messages."
	(let ((cli (list "notify-send")))
		(when urgency
			(nconc cli (list "-u" (symbol-name urgency))))
		(when pixmap
			(nconc cli (list "-i" (fg-pixmap-path pixmap))))
		(when strip
			(setq header (fg-string-strip-whitespace header))
			(setq message (fg-string-strip-whitespace message)))
		(unless (or dont-escape fg-notify-never-escape)
			(setq header (fg-string-escape-html header))
			(setq message (fg-string-escape-html message)))
		(nconc cli (list header message))
		(apply 'start-process "Notification" nil cli)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation descrimination (tab-only) stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq hippie-expand-try-functions-list
  '(try-complete-file-name-partially
    try-complete-file-name
    try-expand-all-abbrevs
    try-expand-dabbrev
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
		try-expand-slime-symbol
    try-complete-lisp-symbol-partially
    try-complete-lisp-symbol
    try-expand-line
    try-expand-list)) ; it is a disaster w/ large lists, hence the place


;; hippie expand for slime
(defun he-slime-symbol-beg ()
	(let ((p (slime-symbol-start-pos))) p))

(defun try-expand-slime-symbol (old)
	(condition-case ex
		(progn
			(unless  old
				(he-init-string (he-slime-symbol-beg) (point))
				(setq he-expand-list
					(sort
						(car (slime-simple-completions
							(buffer-substring-no-properties (slime-symbol-start-pos) (slime-symbol-end-pos))))
						'string-lessp)))
			(while
				(and he-expand-list
					(he-string-member (car he-expand-list) he-tried-table))
				(setq he-expand-list (cdr he-expand-list)))
			(if (null he-expand-list)
				(progn (when old (he-reset-string)) nil)
				(he-substitute-string (car he-expand-list))
				(setq he-expand-list (cdr he-expand-list))
				t))
		('error nil)))

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
				(hippie-expand arg))
			(t (fg-indent arg)))))

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
		(progn
			(fg-indent-line arg)
			(skip-chars-forward " \t"))))

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
		(indent-to (max 0 (+ indent (* arg tab-width)))) ; max is to drop negative indent to 0
		(delete-region (point) (progn (skip-chars-forward " \t") (point)))))

(defun fg-newline ()
	"Mode-safe version of newline-and-indent.
Used to call indent-according-to-mode, but it fucked up way too often."
	(interactive)
	(let ((indent (unless (or buffer-read-only (minibufferp)) (current-indentation))))
		(when indent (delete-horizontal-space t))
		(newline)
		(when indent (indent-to indent))))

;; Comment-tabulata
(defun fg-comment (arg)
	(interactive "*P")
	(let
		((start-m (or (use-region-p) (point-marker)))
			(taint (fg-taint :whole-lines-only t)))
		(push-mark (car taint) t t)
		(goto-char (car (last taint)))
		(comment-dwim arg)
		(when (markerp start-m)
			(deactivate-mark)
			(goto-char (marker-position start-m)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processing / conversion / data mangling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fg-string-replace-pairs (string pairs)
	"Replace regex-replacement pairs in string."
	(mapc
		(lambda (arg)
			(setq string (replace-regexp-in-string (car arg) (cadr arg) string)))
		pairs)
	string)

(defun fg-string-escape-html (string)
	"Encode html entities in STRING, returning \"escaped\" version."
	(fg-string-replace-pairs string
		'(("&" "&amp;")
			("<" "&lt;")
			(">" "&gt;"))))

(defun fg-string-strip-whitespace (string)
	"Remove whitespace characters from STRING margins, returns the resulting string."
	(replace-regexp-in-string "\\(^[[:space:]\n]+\\|[[:space:]\n]+$\\)" "" string))

(defun fg-product (list1 list2)
	"Return a list of the Cartesian product of two lists."
	(mapcan (lambda (x) (mapcar (lambda (y) (list x y)) list2)) list1))

