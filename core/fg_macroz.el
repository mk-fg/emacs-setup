;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REFERENCE - 'cause elisp is for aliens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For most stuff, see:
;;  https://github.com/magnars/s.el
;;  https://github.com/magnars/dash.el
;;  https://github.com/Wilfred/ht.el
;; TODO: dedup old macros with this stuff
(require 's)
(require 'dash)
(require 'dash-functional)
(require 'ht)

;;;; str

; py:
;   str.find(s, sub[, start[, end]])
;   str.rfind(s, sub[, start[, end]])
;   str1 in str2
;   str1 == str2
;   bool(str)
;   len(str)
; el:
;   ??? (position SUB S [:start N] [:end N] [:from-end t])
;   ??? (member STR1 STR2)
;   (string= STR1 STR2)
;   (> (length STR) 0)
;   (length STR)

; py:
;   str.strip(s[, chars])
;   str.lstrip(s[, chars])
;   str.rstrip(s[, chars])
; el:
;   (fg-string-strip-whitespace S)
;   (fg-string-strip-chars S CHARS [:from WHERE])
;   (fg-string-strip S SUB... [:from WHERE])

; py:
;   str.startswith(s, prefix)
;   str.endswith(s, suffix)
; el:
;   (string-prefix-p PREFIX S [IGNORE-CASE])
;   (fg-string-suffix-p SUFFIX S [IGNORE-CASE])

; py:
;   str.split(s[, sep[, maxsplit]])
;   str.rsplit(s[, sep[, maxsplit]])
;   str.join(words[, sep])
; el:
;   (fg-string-split S [:sep SEP-RE]
;     [:omit-nulls t] [:from WHERE] [:limit MAXSPLIT])
;   (fg-string-join SEP WORDS...)

; py:
;   string.replace(s, old, new[, maxreplace])
; el:
;   (replace-regexp-in-string OLD-RE NEW-RE S)
;   (fg-string-replace-pairs S (OLD-RE NEW-RE)...)

; py: str.upper -> el: upcase
; py: str.lower -> el: downcase
; py: string.capitalize -> el: capitalize

;;;; list

; py:
;   list.index(v)
;   v in list
; el:
;   (memq V LIST)
;   (memql V LIST)
;   (member V LIST)

; py: list[n]
; el: (nth N LIST)

; py: list[n:m]
; el: (subseq LIST N M)

; py: list_a + list_b
; el: (append LIST_A LIST_B)

; py: a = list.pop()
; cl: (let* ((A (car (last LIST)))) (nbutlast LIST) ...)

; py: list.append(a)
; el: (push A LIST)

; py: for a in list:
; el: (dolist (A LIST [RES]) ...)
; cl: (loop for A in LIST ...)

; py: reversed(list)
; el: (nreverse LIST)

; py: filter(func, list)
; el: (fg-keep-when FUNC LIST)
; el: (remove-if NOT-FUNC LIST)

; py-1: if a not in some_list: some_list.append(a)
; py-2: if (a . b) not in some_alist: some_alist[a] = b
; el-1: (add-to-list 'SOME_LIST A [append nil] [compare-fn 'equal])
; el-2: (add-to-list 'SOME_ALIST (A . B) ...)

;;;; misc

; py:
;   try: stuff
;   except Exception as err: pass
; el:
;   (condition-case err stuff ('error nil))

; py: map(func, list)
; el: (mapcar func list)

; py: for a in list: func(a)
; el: (mapc func list)

; py: len(some_dict)
; el: (hash-table-count SOME-DICT)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metacode stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fg-apply-partially-append (fun &rest args)
	"Like `apply-partially', but appends arguments to a wrapped call."
	(lexical-let ((fun fun) (args1 args))
		(lambda (&rest args2) (apply fun (append args2 args1)))))

(defun fg-keys-from-rest (args)
	"Remove keywords and their values from ARGS.
Useful for &rest + &key + &allow-other-keys in `defun*'."
	(let (res drop)
		(dolist (v args)
			(when (not drop)
				(if (keywordp v) (set 'drop t)
					(setq res (cons v res) drop nil))))
		(nreverse res)))

(defun fg-apply-macro (macro &rest args)
	"Same as `apply', but can apply macro instead of a function."
	(let ((args (append (nbutlast args) (car (last args)))))
		(eval (macroexpand-all `(,macro ,@args)))))

(defun fg-real-function-name (function)
	"Return name of FUNCTION or name it is an alias of.
Based on `describe-function-1'."
	(let*
		((advised
				(and (symbolp function)
					(featurep 'advice)
					(ad-get-advice-info function)))
			(real-function
				(or
					(and advised
						(let
							((origname (cdr (assq 'origname advised))))
							(and (fboundp origname) origname)))
					function))
			(def
				(if (symbolp real-function)
					(symbol-function real-function)
					function))
			(aliased (symbolp def))
			(real-function
				(if aliased
					(let ((f def))
						(while
							(and (fboundp f) (symbolp (symbol-function f)))
							(set 'f (symbol-function f)))
						f)
					real-function)))
		(format "%s" real-function)))



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

(defadvice forward-word (around fg-forward-word activate)
	"Make `forward-word' stop at newlines as well."
	(if (eolp) ;; already at the EOL
		ad-do-it
		(let ((line (line-number-at-pos)))
			(save-excursion
				ad-do-it
				(setq line (= line (line-number-at-pos))))
			(if line ;; EOL was not crossed
				ad-do-it
				(move-end-of-line nil)))))

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
	"Restore current buffer `point' position from register,
announcing register id."
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

(defun fg-occur (arg)
	"Display `occur' info for whatever I'm pointing at.
Prefix argument limits the context (passed as NLINES to `occur'), if set."
	(interactive "p")
	(let
		((thing
			(if (use-region-p)
				(buffer-substring
					(region-beginning) (region-end))
				(thing-at-point 'word))))
		(if thing
			(occur (regexp-quote thing) arg)
			(call-interactively 'occur))))

(defun fg-nuke-all ()
	"Nuke all buffers, leaving *scratch* only"
	(interactive)
	(mapc (lambda (x) (kill-buffer x))
		(buffer-list))
	(delete-other-windows))

(defun fg-recentf-prompt ()
	"Completion prompt of recentf list in minibuffer,
using only files' basename."
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
	"Returns whether emacs window is visible (or 'active').
Check result is cached for a small period of time,
because is performed via external tools.
FORCE option allows to bypass this caching."
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


(defvar fg-find-buffer-state nil
	"Stores '(buffer-from buffer-to) information for `fg-find-buffer'.")

(defun* fg-find-buffer
	(name &key error-if-not-found (switch-back t))
	"Switch to named buffer, without creating it if it doesn't exists.
SWITCH-BACK allows to reverse the operation with
the subsequent call with the same NAME in the same window.
ERROR-IF-NOT-FOUND signals error if named buffer doesn't exist."
	(interactive)
	(let
		((buffer-from (current-buffer))
			(buffer-to (get-buffer name)))
		(if buffer-to
			(multiple-value-bind
				(buffer-x-from buffer-x-to)
				fg-find-buffer-state
				(if
					(and switch-back
						(eq buffer-from buffer-to)
						(eq buffer-x-to buffer-from))
					(fg-find-buffer buffer-x-from :switch-back nil)
					(switch-to-buffer buffer-to)
					(setq fg-find-buffer-state (list buffer-from buffer-to))))
			(funcall
				(if error-if-not-found 'error 'message)
				"No such buffer: %s" name))))

(defun fg-list-useful-buffer-names (&optional pattern buffers)
	"Return a list of non-private buffers that contain string PATTERN.
If BUFFERS is passed, only these will be considered."
	(--filter
		(and
			(or (not pattern) (s-contains? pattern it t))
			(not (s-matches? "^\\s-*\\*" it)))
		(-map 'buffer-name (or buffers (buffer-list)))))

(defun fg-get-useful-buffer (&optional pattern buffers)
	"Returns buffer name for unique PATTERN match,
according to `fg-list-useful-buffer-names'.
If more than one match is returned, error gets signaled."
	(let ((names (fg-list-useful-buffer-names pattern buffers)))
		(if (-contains? names pattern)
			pattern ; exact match
			(unless (= (length names) 1)
				(error (concat "Failed to uniquely match"
					" buffer by `%s', matches: %s") pattern (s-join ", " names)))
			(car names))))

(defadvice desktop-create-buffer (around fg-desktop-create-buffer activate)
	(let ((filename (ad-get-arg 1)) (buffname (ad-get-arg 2)))
		(condition-case-unless-debug err
			ad-do-it
			('error (message
				"Failed to restore buffer %S (file: %S): %s"
				buffname filename err)))))

(require 'notifications) ;; using vars from there, loads dbus as well

(defvar fg-notify-never-escape nil
	"Never escape html entities in notification functions")

(defun* fg-notify
	(header message &key pixmap urgency strip dont-escape)
	"Send desktop notification about event.
PIXMAP specifies an icon to use.
URGENCY can be set to 'low or 'critical.
STRIP can be specified to trim whitespace chars from text.
DONT-ESCAPE inhibits escaping html entities in messages.

Uses async dbus call and does not return notification id."
	(let ((hints '()))
		(when urgency
			(add-to-list 'hints
				(list :dict-entry "urgency"
					(list :variant :byte
						(case urgency (low 0) (critical 2) (t 1)))) t))
		(when pixmap
			(add-to-list 'hints (list :dict-entry "image-path"
				(list :variant :string (concat "file://" (fg-pixmap-path pixmap)))) t))
		(when strip
			(setq header (fg-string-strip-whitespace header))
			(setq message (fg-string-strip-whitespace message)))
		(unless (or dont-escape fg-notify-never-escape)
			(setq header (fg-string-escape-html header))
			(setq message (fg-string-escape-html message)))

	(dbus-call-method-asynchronously :session
		notifications-service
		notifications-path
		notifications-interface
		notifications-notify-method
		(lambda (note-id) nil)
		;; :timeout <-- might be useful for controlling whatever lag errors
		:string notifications-application-name
		:uint32 0 ;; replaces-id
		:string notifications-application-icon
		:string (or header "")
		:string (or message "")
		(list :array) ;; actions
		(or hints '(:array :signature "{sv}"))
		:int32 -1)) ;; timeout

	;; We don't return notification id syncronously anymore
	nil)

(defun fg-time-string (&optional ts)
	(unless ts (set 'ts (current-time)))
	(concat (current-time-string ts) " " (cadr (current-time-zone ts))))


(defun fg-product (list1 list2)
	"Return a list of the cartesian product of two lists."
	(mapcan (lambda (x) (mapcar (lambda (y) (list x y)) list2)) list1))

(defun fg-keep-when (pred seq)
	"Return only elements from SEQ for which PRED returns non-nil."
	(let ((del (make-symbol "del")))
		(remove del (mapcar (lambda (el) (if (funcall pred el) el del)) seq))))

(defun* fg-hex (val &optional (offset 0) len)
	"Parse hex from a VAL string, or substring (OFFSET / LEN) of VAL."
	(when (/= 0 offset) (set 'val (substring val offset)))
	(when len (set 'val (substring val 0 len)))
	(string-to-number val 16))

(defun* fg-xor (a b)
	"Easy XOR logic function. Force-converts A and B to t or nil."
	(let ((a (and a t)) (b (and b t))) (not (eq a b))))


(require 'color)

(defvar fg-color-tweak-debug nil)

;; No idea if L*a*b* coord ranges for a*/b* and sRGB bounds are actually correct
(defun* fg-color-tweak
	(color &optional seed min-shift max-shift
		(clamp-rgb-after 20)
		(lab-ranges '((0 100) (-86.185 98.254) (-107.863 94.482))))
	"Adjust COLOR based on (md5 of-) SEED and MIN-SHIFT / MAX-SHIFT lists.

COLOR can be provided as a three-value (0-1 float)
R G B list, or a string suitable for `color-name-to-rgb'.

MIN-SHIFT / MAX-SHIFT can be:
 * three-value list (numbers) of min/max offset on L*a*b* in either direction
 * one number - min/max cie-de2000 distance
 * four-value list of offsets and distance, combining both options above
 * nil for no-limit

SEED can be number, string or nil.
Empty string or nil passed as SEED will return the original color.

CLAMP-RGB-AFTER defines how many attempts to make in picking
L*a*b* color with random offset that translates to non-imaginary sRGB color.
When that number is reached, last color will be `color-clamp'ed to fit into sRGB.

Returns color plus/minus offset as a hex string.
Resulting color offset should be uniformly distributed between min/max shift limits."
	(interactive)
	(when (< clamp-rgb-after 1) (error "clamp-rgb-after must be >1"))
	(let*

		((parse-ciede2k
				(lambda (shift-spec)
					(when shift-spec
						(if (numberp shift-spec) shift-spec
							(when (= (length shift-spec) 4) (nth 3 shift-spec))))))
			(parse-offsets
				(lambda (shift-spec) (and shift-spec (listp shift-spec) shift-spec)))

			(min-ciede2k (funcall parse-ciede2k min-shift))
			(max-ciede2k (funcall parse-ciede2k max-shift))
			(min-offs (funcall parse-offsets min-shift))
			(max-offs (funcall parse-offsets max-shift))

			(color0
				(apply #'color-srgb-to-lab
					(if (stringp color) (color-name-to-rgb color) color)))
			color1-srgb-valid-first ; skipped on ciede2k diff mismatch
			color1-srgb-fallback) ; any color1, used as a last resort

		(apply #'color-rgb-to-hex
			(loop
				for n from 0 to clamp-rgb-after
				do
					(let*

						((color1
								(block :color-shift
									(if (numberp seed)
										(set 'seed (concat "###" (number-to-string seed)))
										(when (or (not seed) (equal seed "")) (return-from :color-shift color0)))
									(set 'seed (md5 seed))
									(loop
										for n from 0 to 2
										collect
											(let*
												((o (* n 4))
													(sign (if (> (fg-hex seed o 1) 7) '+ '-))
													(rnd (/ (fg-hex seed (+ o 1) 3) 4095.0))
													(c (nth n color0))
													(n-min (or (and min-offs (nth n min-offs)) 0))
													(n-max (and max-offs (nth n max-offs))))
												(if n-max
													; "c1 = c +/- (rnd * (max-min) + min)"
													(funcall sign c (+ (* rnd (- n-max n-min)) n-min))
													(let*
														; c1 in [a, b], where a/b are picked from rnd and pos/neg spans
														((lab-range (nth n lab-ranges))
															(c-pos-a (cadr lab-range))
															(c-pos-b (min c-pos-a (+ c n-min)))
															(c-pos-span (abs (- c-pos-a c-pos-b)))
															(c-neg-a (car lab-range))
															(c-neg-b (max c-neg-a (- c n-min)))
															(c-neg-span (abs (- (abs c-neg-a) (abs c-neg-b))))
															(rnd-ratio (/ c-neg-span (+ c-neg-span c-pos-span))))
														(if (> rnd rnd-ratio)
															(setq a c-pos-a b c-pos-b k c-pos-span rnd (- rnd rnd-ratio))
															(setq a c-neg-a b c-neg-b k (- c-neg-span)))
														(when (> (abs a) (abs b)) (let ((ax a)) (setq a b b ax)))
														(if (= a b) a (+ c (* rnd k) a))))))))
							(color1-srgb (apply #'color-lab-to-srgb color1))
							(color1-srgb-valid
								(loop
									for c in color1-srgb
									do (when (or (> c 1) (< c -0.06)) (return nil))
									collect (color-clamp c))))

						(when color1-srgb-valid
							(unless color1-srgb-valid-first
								(set 'color1-srgb-valid-first color1-srgb-valid))
							(let
								((ciede2k (color-cie-de2000 color0 color1)))
								(when
									(and
										(or (not min-ciede2k) (> ciede2k min-ciede2k))
										(or (not max-ciede2k) (< ciede2k max-ciede2k)))
								(when fg-color-tweak-debug
									(message "fg-color-tweak: %s -> %s, n=%d [%.2f]"
										color (apply #'color-rgb-to-hex color1-srgb) n ciede2k))
								(return color1-srgb-valid))))

						(unless color1-srgb-fallback
							(set 'color1-srgb-fallback color1-srgb)))

				finally
					(let
						((color1-srgb
							(mapcar #'color-clamp
								(or color1-srgb-valid-first color1-srgb-fallback))))
						(when fg-color-tweak-debug
							(message "fg-color-tweak: %s -> %s, n=%d [FALLBACK]"
								color (apply #'color-rgb-to-hex color1-srgb) n))
						(return color1-srgb))))))

;; (setq fg-color-tweak-debug t)
;; (loop for n from 0 to 10 do (fg-color-tweak "#000" n 80))


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
			(unless old
				(he-init-string (he-slime-symbol-beg) (point))
				(setq he-expand-list
					(sort
						(car (slime-simple-completions
							(buffer-substring-no-properties
								(slime-symbol-start-pos)
								(slime-symbol-end-pos))))
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
	(cl-labels
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
;; Processing / conversion / string mangling
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

(defun fg-string-reverse (s) (concat (nreverse (string-to-list s))))

(defun fg-string-suffix-p (suffix s &optional ignore-case)
	(string-prefix-p (fg-string-reverse suffix) (fg-string-reverse s) ignore-case))

(defun* fg-string-split (string &key sep omit-nulls limit (from 'left))
	"Same as `split-string', but with optional split-limit and direction keys."
	(when (not (eq from 'left)) (error "Only [:from 'left] is supported now."))
	(if (and limit (/= limit 0))
		(let
			((keep-nulls (not (if sep omit-nulls t)))
				(rexp (or sep split-string-default-separators))
				(start 0) notfirst (list nil))
			(block 'limited
				(while
					(and
						(string-match rexp string
							(if (and notfirst (= start (match-beginning 0))
								(< start (length string))) (1+ start) start))
						(< start (length string)))
					(set 'notfirst t)
					(when (or keep-nulls (< start (match-beginning 0)))
						(set 'list (cons (substring string start (match-beginning 0)) list))
						(when (and limit (<= (set 'limit (1- limit)) 0))
							(set 'list (cons (substring string (match-end 0)) list))
							(return-from 'limited)))
					(set 'start (match-end 0)))
				(when (or keep-nulls (< start (length string)))
					(set 'list (cons (substring string start) list))))
			(nreverse list))
		(list string)))

(defun fg-string-join (sep &rest strings) (mapconcat 'identity strings sep))

(defun* fg-string-strip (string &rest frags &key (from 'both) &allow-other-keys)
	"Remove substrings (e.g. characters) from STRING margins.
FROM can be one of '(both l left r right), 'both being a default.
Returns the resulting string."
	(let*
		((frags (apply 'fg-string-join "\\|"
				(mapcar 'regexp-quote (fg-keys-from-rest frags))))
			regexp)
		(when (memq from '(both r right))
			(set 'regexp (cons (format "\\(%s\\)+$" frags) regexp)))
		(when (memq from '(both l left))
			(set 'regexp (cons (format "^\\(%s\\)+" frags) regexp)))
		(replace-regexp-in-string (apply 'fg-string-join "\\|" regexp) "" string)))

(defun* fg-string-strip-chars (string chars &key (from 'both) &allow-other-keys)
	(apply 'fg-string-strip string :from from (mapcar 'char-to-string "asddsa")))

(defun fg-string-strip-whitespace (string)
	"Remove whitespace characters from STRING margins, returns the resulting string."
	(replace-regexp-in-string "\\(^[[:space:]\n]+\\|[[:space:]\n]+$\\)" "" string))

(defadvice wildcard-to-regexp (around fg-wildcard-to-regexp activate)
	"Make `wildcard-to-regexp' not fail if square brackets are present in the filename."
	(ad-set-arg 0
		(fg-string-replace-pairs (ad-get-arg 0)
			'(("_\\(-+\\)_" "_--\\1_") ("\\[" "_-_") ("\\]" "_--_"))))
	;; Will fail if original func will add "_-+_", to the resulting regexp, but shouldn't happen
	ad-do-it
	(set 'ad-return-value
		(fg-string-replace-pairs ad-return-value
			'(("_-_" "\\\\[") ("_--_" "\\\\]") ("_--\\(-+\\)_" "_\\1_")))))

(defmacro fg-string-case (expr &rest conds)
	"Same as `case', but uses `string='."
	`(cond
			,@(--map
				(let ((match (car it)) (body (cdr it)))
					`(,(if (eq match t) t `(string= ,expr ,match)) ,@body))
				conds)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbers and math
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-float (&optional max-digits)
	"Returns random `float' in [0, 1] range with MAX-DIGITS decimal digits.
Not sure if distribution fits whatever advanced purposes (e.g. crypto),
but should be good enough for simple random checks.
More info on such naive random: http://mumble.net/~campbell/2014/04/28/uniform-random-float"
	(let ((random-max (expt 10 (or max-digits 6))))
		(/ (float (1+ (random random-max))) (- random-max 1))))
