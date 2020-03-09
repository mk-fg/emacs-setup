(require 'erc)
(require 'tls)


(defvar fg-erc-connect-last 0
	"Timestamp when last irc net was connected,
to make sure there are delays between these.
Used to skip redundant `fg-erc-connect-next' calls in `fg-erc'.")

(defvar fg-erc-connect-lag 10
	"Timeout for waiting on irc connection to complete properly.
Used for misc sloppy time matching purposes as well.")

(defun fg-erc ()
	"Connect to IRC servers.
Uses up all the connection commands destructively,
so can only be called once.
Enables notifications only after connecting to the last server,
to avoid spamming them with MOTD entries and notices."
	(interactive)
	(if fg-erc-links (fg-erc-connect-loop) (fg-erc-update-networks)))

(defun fg-erc-connect-loop (&rest ignored)
	"Add itself to `erc-after-connect' hook and schedule
`fg-erc-connect-next' to run right after that and after `fg-erc-connect-lag' timeout,
which will then call this func again to schedule next connection.
Idea is to chain `fg-erc-connect-next' that way until `fg-erc-links' is empty
and call `fg-erc-connect-done' after that."
	(if (not fg-erc-links)
		(progn
			(remove-hook 'erc-after-connect 'fg-erc-connect-loop)
			(run-with-timer 1 nil 'fg-erc-connect-done))
		(add-hook 'erc-after-connect 'fg-erc-connect-loop)
		(run-with-timer (* 1.5 fg-erc-connect-lag) nil 'fg-erc-connect-next t)
		(run-with-timer 1 nil 'fg-erc-connect-next)))

(defun fg-erc-connect-next (&optional timeout-hook)
	"Pulls first tuple from `fg-erc-links' and connects there, if any,
calling `fg-erc-connect-loop' afterwards.
Checks `fg-erc-connect-last' to abort if something else connected within
`fg-erc-connect-lag' interval already, as that'd should already schedule
next call without the need to stampede it here as well."
	(unless
		(and timeout-hook
			(< (- (float-time) fg-erc-connect-last) fg-erc-connect-lag))
		(let ((link (car fg-erc-links)))
			(when link
				(setq
					fg-erc-links (cdr fg-erc-links)
					fg-erc-connect-last (float-time))
				(apply (car link) (cdr link)))
			(fg-erc-connect-loop))))

(defun fg-erc-connect-done ()
	"Called after all irc networks are connected or timed-out.
Maybe multiple times by some laggy timers."
	(fg-erc-update-networks)
	(add-hook 'erc-insert-post-hook 'fg-erc-notify)
	(unless fg-erc-track-save-timer
		(setq fg-erc-track-save-timer
			(run-with-timer fg-erc-track-save-interval
				fg-erc-track-save-interval 'fg-erc-track-save))))

(defun fg-erc-update-networks ()
	"Update `erc-network' values from `erc-determine-network'
in all erc buffers and run `erc-update-mode-line' there."
	(interactive)
	(dolist (buff (erc-buffer-list))
		(with-current-buffer buff
			(unless
				(and (erc-network-name)
					(not (member (erc-network-name) '("Unknown" "nil"))))
				(erc-with-server-buffer
					(setq erc-network (erc-determine-network))))
			(erc-update-mode-line))))

(defun fg-erc-quit (&optional reason)
	"Disconnect (/quit) from all connected IRC servers immediately
and unset `process-query-on-exit-flag' on all `erc-server-process' subprocesses.
This doesn't wait for when subprocesses exit and run their disconnect hooks,
and intended to be synchronous best-effort 'cleanup before exit' thing."
	(interactive)
	(erc-with-all-buffers-of-server nil
		#'erc-open-server-buffer-p
		(erc-quit-server reason)
		(set-process-query-on-exit-flag erc-server-process nil)))

(add-hook 'fg-emacs-exit-hook 'fg-erc-quit)


;; Common erc message processing routines

(defvar fg-erc-post-hook-strip-regexp "^\\[[0-9]\\{2\\}:[0-9]\\{2\\}\\(:[0-9]\\{2\\}\\)?\\]\\s-*"
	"Regexp for `fg-erc-get-hook-msg', to replace match with nothing when using
message with `erc-insert-post-hook', intended to match/remove timestamps.
Setting to nil avoids making any replacements.")

(defvar fg-erc-post-hook-pick-regexp fg-erc-post-hook-strip-regexp
	"Regexp for `fg-erc-get-hook-msg', to pick last matching line with,
when using message with `erc-insert-post-hook', if narrowed buffer has multiple lines.
If nil, or no match is found, simply last line get passed in these cases.")

(defun fg-erc-get-hook-msg (&optional text)
	"Used in `erc-insert-pre-hook', `erc-insert-modify-hook' or `erc-insert-post-hook' funcs,
to get text cleaned-up of timestamps, and indication of what kind of hook it is.
Only 'pre' hook passes message text,
otherwise it has to be grabbed from (presumably narrowed) buffer,
and (optionally) cleaned-up from timestamps, text-props, control chars, etc."
	(let ((hook-type (if text 'pre 'post)))
		(when (eq hook-type 'post)
			(setq text (buffer-substring-no-properties (point-min) (point-max))))
		(let (line-last line-match)
			(dolist (line (-map 'erc-controls-strip (s-lines text)))
				(setq line-last line)
				(when
					(and fg-erc-post-hook-pick-regexp
						(string-match fg-erc-post-hook-pick-regexp line))
					(setq line-match line)))
			(setq text (or line-match line-last)))
		(when fg-erc-post-hook-strip-regexp
			(setq text (replace-regexp-in-string fg-erc-post-hook-strip-regexp "" text)))
		(setq text (s-trim-right text))
		(list text hook-type)))


;; --- Local feature: erc-track state preservation feature
;; Idea is to have list of unread stuff dumped to some file on timer,
;;  so that sudden system crash or emacs kill won't loose any important msgs

(defcustom fg-erc-track-save-path (concat fg-path "/tmp/erc-track-state")
	"Path to save `erc-modified-channels-alist' state to."
	:group 'erc-track :type 'string)

(defcustom fg-erc-track-save-interval 400
	"Interval between saving `erc-modified-channels-alist' state,
so that it can be preserved in an event of emacs getting killed."
	:group 'erc-track :type 'number)

(defcustom fg-erc-track-save-copies 4
	"Copies of old `erc-modified-channels-alist' states to keep."
	:group 'erc-track :type 'number)

(defvar fg-erc-track-save-timer nil
	"Repeating timer for `fg-erc-track-save'.")

(defvar fg-erc-track-save-seed (format "%d" (random))
	"Seed for identifying emacs instance for `fg-erc-track-save'.")

(defun fg-erc-track-save-dump ()
	(apply 'fg-string-join "\n"
		(append
			(list
				fg-erc-track-save-seed
				""
				(format "%s (%.0f)" (fg-time-string) (float-time))
				"")
			(let (res)
				(nreverse
					(dolist (el erc-modified-channels-alist res)
						(push (format "%s %d"
							(buffer-name (car el)) (cadr el)) res))))
			(list ""))))

(defun fg-erc-track-save-bak-name (n)
	(format "%s.%d" fg-erc-track-save-path n))

(defun fg-erc-track-save ()
	"Save `erc-modified-channels-alist' to a file,
making sure to preserve a copies from a few last runs."
	(let
		((curr-lines
			(with-temp-buffer
				;; Check if current seed matches the one in the file
				(condition-case ex
					(progn
						(insert-file-contents fg-erc-track-save-path)
						(split-string (buffer-string) "\n" t))
					('error nil)))))
		;; Rotate backup copies, if any
		(when
			(not (string= (first curr-lines) fg-erc-track-save-seed))
			(let (fns)
				(dotimes (n (- fg-erc-track-save-copies 1) fns)
					(multiple-value-bind (src dst)
						(mapcar 'fg-erc-track-save-bak-name
							(list
								(- fg-erc-track-save-copies n 1)
								(- fg-erc-track-save-copies n)))
						(when (file-exists-p src) (rename-file src dst t)))))
			(when (file-exists-p fg-erc-track-save-path)
				(rename-file fg-erc-track-save-path
					(fg-erc-track-save-bak-name 1) t)))
		;; Save
		(with-temp-buffer
			(insert (fg-erc-track-save-dump))
			(write-region (point-min) (point-max) fg-erc-track-save-path))))

;; Make erc-track only count visible messages
(defadvice erc-track-modified-channels
	(around fg-erc-track-discard-invisible-msgs activate)
	(unless (erc-string-invisible-p (buffer-substring (point-min) (point-max))) ad-do-it))


;; --- Local feature: blocking/modifying msgs matched by a bunch of props

(defcustom fg-erc-msg-block-plists ()
	"Block messages by matching any of
channel, network, nick or message vs regexp plists.

These offer a subset of `fg-erc-msg-modify-plists' functionality,
but handled specially, as they can be applied on later hooks.

List of plists with any number of following keys (in each):
	:net - regexp to match network.
	:chan - regexp to match erc-target (e.g. channel or nick).
	:nick - nickname regexp for `fg-erc-msg-block-pattern'.
	:msg - message regexp for `fg-erc-msg-block-pattern'."
	:group 'erc :type '(repeat sexp))

(defcustom fg-erc-msg-modify-plists ()
	"Modify messages by matching any of
channel, network, nick or message vs regexp plists,
and applying specified (as :func) function to it.

Function won't be passed any args, and is expected to work
as if it was called from `erc-insert-modify-hook' (and it probably is).
I.e. work with narrowed buffer, grab msg from there, alter it there.

List of plists with any number of following keys (in each):
	:net - regexp to match network.
	:chan - regexp to match erc-target (e.g. channel or nick).
	:nick - nickname regexp for `fg-erc-msg-block-pattern'.
	:msg - message regexp for `fg-erc-msg-block-pattern'.
	:func - function to apply to the message (see above)."
	:group 'erc :type '(repeat sexp))

;; These are to allow easy setq for fg-erc-msg-*-plists, without having to merge/dedup these
(defvar fg-erc-msg-block-plists-base ()
	"Persistent global part of `fg-erc-msg-block-plists' from the repo.")
(defvar fg-erc-msg-block-plists-local ()
	"Site-local `fg-erc-msg-block-plists', for easier merging with global list.")
(defvar fg-erc-msg-modify-plists-local ()
	"Site-local `fg-erc-msg-modify-plists', for easier merging with global list.")

(defun fg-erc-msg-block-pattern (nick msg)
	"Build proper pattern for regular channel messages
(including ZNC-buffered messages) from specified NICK
and MSG regexp patterns. MSG can have $ at the end."
	(concat
		"^\\(?:\\s-*\\[[0-9:]+\\]\\)?\\s-*[<-]" nick
		"[->]\\(?:\\s-+\\[[0-9:]+\\]\\)?\\s-*" msg))

(defun fg-erc-re (string) (concat "^" (regexp-quote string) "$"))


;; --- Modules
(customize-set-variable 'erc-modules
	(-union
		(-difference erc-modules '(
			;; Fill-mode doesn't play nice with variable pitch
			;; Note that it can't seem to be disabled globally via erc-fill-mode var
			fill
			;; These are useless and only hinder ops like copy-paste
			button
			;; Disabled by default, but I'd hate to bump into these
			;; keep-place sounds nice, but puts first-line-to-read
			;;   at the bottom of the buffer, maybe should be fixed.
			smiley sound keep-place))
		'(log truncate autoaway dcc)))
(erc-update-modules)


;; --- Filtering lists are split from other opts here to modify and re-apply parts easier

;; net+chan+nick+msg ignore-patterns
;; See fg-erc-msg-block-pattern for how nick/msg parts are used
(setq-default fg-erc-msg-block-plists-base
	`((:chan "^&bitlbee$" :net "^BitlBee$" :nick "root"
			:msg ,(concat "gtalk - \\("
				"Error: Error while reading from server"
				;; "62 seconds" is used to match only first reconnect, making noise on others
				"\\|Signing off\\.\\." "\\|Reconnecting in 62 seconds\\.\\."
				"\\|Logging in: \\(" "Connecting" "\\|Logged in"
					"\\|Connected to server, logging in" "\\|Converting stream to TLS"
					"\\|Server changed session resource string to `Indirect[0-9A-F]+'"
					"\\|Authentication finished" "\\|Authenticated, requesting buddy list" "\\)" "\\)"))
		(:chan "^#" :net "^BitlBee$" :msg ,(concat "^ *\\*\\*\\* \\("
			"\\(You have been kicked off channel\\|Topic for\\|Users on\\) #"
			"\\|#\\S-+: topic set by " "\\)"))))

(setq-default fg-erc-msg-block-plists
	(append fg-erc-msg-block-plists-local fg-erc-msg-block-plists-base))

;; ;; This is just an example of how to implement modify-func to work for blocking
;; ;; Use fg-erc-msg-block-plists for this exact thing instead
;; (setq-default fg-erc-msg-modify-plists (append fg-erc-msg-modify-plists-local
;; 	;; Filter-out spammy stuff on bitlbee reconnects
;; 	`((:chan "^#" :net "^BitlBee$" :func ,(lambda ()
;; 		(-let [(line hook-type) (fg-erc-get-hook-msg text)]
;; 			(when
;; 				(string-match
;; 					(concat "^ *\\*\\*\\* \\("
;; 							"\\(You have been kicked off channel\\|Topic for\\|Users on\\) #"
;; 							"\\|#\\S-+: topic set by "
;; 						"\\)") line)
;; 				(erc-put-text-property (point-min) (point-max) 'invisible t (current-buffer)))))))))
(setq-default fg-erc-msg-modify-plists fg-erc-msg-modify-plists-local)


;; ---=== Main configuration ===---

(setq-default
	erc-server "irc.fraggod.net"

	;; erc-port 6667
	;; erc-nick '("freenode")

	erc-user-full-name "Mike Kazantsev"
	erc-email-userid "mk.fraggod_at_gmail_com"

	erc-prompt
		(lambda () (erc-propertize (concat "~erc/"
			(if (and (boundp 'erc-default-recipients) (erc-default-target))
				(erc-default-target) "limbo") "%")
			'read-only t 'rear-nonsticky t 'front-nonsticky t 'intangible t))
	erc-minibuffer-notice nil

	erc-quit-reason 'erc-quit-reason-various
	erc-quit-reason-various-alist
		'(("home" "Heading home...")
			("" "o//"))
	erc-part-reason 'erc-part-reason-various
	erc-part-reason-various-alist erc-quit-reason-various-alist

	erc-anonymous-login nil
	erc-join-buffer 'bury

	erc-interpret-controls-p t ;; for otr
	erc-interpret-mirc-color nil
	erc-beep-p nil
	erc-encoding-coding-alist
		'(("#debian-ru" . cyrillic-koi8))

	;; Custom log-friendly datestamping, includes erc-insert-timestamp-left
	;; Note that default erc-insert-timestamp-function is "...-right"
	erc-insert-timestamp-function 'fg-erc-timestamp-with-datestamps
	erc-datestamp-format " === [%Y-%m-%d %a] ===\n"

	erc-timestamp-only-if-changed-flag nil
	erc-timestamp-format "[%H:%M:%S] "

	;; These are only used with erc-insert-timestamp-left-and-right, i.e. not used here
	erc-timestamp-format-left erc-timestamp-format
	erc-timestamp-format-right nil

	erc-pcomplete-nick-postfix ","
	erc-pcomplete-order-nickname-completions t

	erc-log-insert-log-on-open nil ;; very messy
	erc-log-channels-directory (concat fg-path "/tmp/erc")
	erc-max-buffer-size 120000
	erc-max-buffer-size-to-act 150000 ;; for custom truncation, not used by default ERC
	erc-fill-column 2048 ;; in case that dumb module gets activated somehow

	erc-track-showcount t
	erc-track-exclude-types ;; join/part/nickserv + all the crap on connect
		'("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477")
	erc-track-enable-keybindings nil

	erc-hide-list '("JOIN" "PART" "MODE" "MODE-nick" "QUIT") ;; careful, these are completely ignored

	;; erc-ignore-list ;; global ignore-everywhere list - can be useful for common CI irc bots
	;; 	'("^CIA-[[:digit:]]+!~?[cC][iI][aA]@"
	;; 		"^fdo-vcs!~?kgb@\\sw+\\.freedesktop\\.org$"
	;; 		"^KGB[^!]+!~?Debian-kgb@.*\\.kitenet\\.net$"
	;; 		"^travis-ci!~?travis-ci@.*\\.amazonaws\\.com$"
	;; 		"^irker[[:digit:]]+!~?irker@"
	;; 		"^GitHub[[:digit:]]+!~?GitHub[[:digit:]]+@.*\\.github\\.com$")

	erc-server-auto-reconnect t
	erc-server-reconnect-attempts t
	erc-server-reconnect-timeout 10

	erc-pals nil
	erc-fools nil
	erc-notify-list nil

	erc-notify-signon-hook nil
	erc-notify-signoff-hook nil)

;; Autoaway is only useful when based on X idle time, not emacs/irc
(when (eq window-system 'x)
	(setq-default
		erc-auto-set-away nil
		erc-autoaway-message "AFK (%is), later..."
		erc-autoaway-idle-method 'x
		erc-autoaway-idle-seconds (* 30 60)))



;; --- Custom timestamping

(make-variable-buffer-local
	(defvar erc-last-datestamp nil))

(defun fg-erc-timestamp-with-datestamps (string)
	"Insert date as well as timestamp if it changes between events.
Makes ERC buffers a bit more log-friendly."
	(erc-insert-timestamp-left string)
	(let ((datestamp (erc-format-timestamp (current-time) erc-datestamp-format)))
		(unless (string= datestamp erc-last-datestamp)
			(erc-insert-timestamp-left datestamp)
			(setq erc-last-datestamp datestamp))))


;; --- Custom buffer truncation
;; Idea is to remove invisible lines first and only then start on visible ones
;; This is useful for discord #monitor channels, where most lines are discarded

(defun erc-truncate-buffer ()
	"Truncates current buffer to `erc-max-buffer-size'.
Not on every new line (as in vanilla version), but only if
buffer is larger than `erc-max-buffer-size-to-act'.
Runs `fg-erc-truncate-invisible-lines' first and
then also `erc-truncate-buffer-to-size' if still necessary.
Appending to logs is handled in `erc-truncate-buffer-to-size'.
Meant to be used in hooks, like `erc-insert-post-hook'."
	(interactive)
	(when (> (buffer-size) erc-max-buffer-size-to-act)
		(fg-erc-truncate-invisible-lines erc-max-buffer-size)
		(when (> (buffer-size) erc-max-buffer-size)
			(erc-truncate-buffer-to-size erc-max-buffer-size))))

(defun fg-erc-truncate-invisible-lines (size)
	"Truncates current buffer to SIZE
by removing invisible lines (if any) from top-down.
Resulting buffer can be longer than SIZE if there isn't enough such lines."
	(interactive)
	(save-restriction
		(widen)
		(buffer-disable-undo)
		(let ((inhibit-read-only t) ls)
			(goto-char (point-min))
			(cl-block erc-trunc-loop
				(while (> (buffer-size) size)
					(setq ls (point))
					(forward-line)
					(if (equal ls (point)) (cl-return-from erc-trunc-loop))
					(when (erc-string-invisible-p (buffer-substring ls (point)))
						(delete-region ls (point))))))
		(buffer-enable-undo)))



;; --- Local filtering framework implementation
;; See fg-erc-msg-block-plists and fg-erc-msg-modify-plists above

(defun fg-erc-msg-content-filter (&optional text)
	"`erc-insert-modify-hook' or `erc-insert-pre-hook' function
to match message against `fg-erc-msg-block-plists' ruleset
and discard/hide the message if any rule in either matches it.

Depending on whether TEXT is passed (and/or returned from `fg-erc-get-hook-msg'),
if match is found, either `erc-insert-this' is used to discard ('pre' hook),
or (presumably narrowed) buffer is updated with invisible text-props to hide the thing.

Will also apply `fg-erc-msg-modify-plists' changes if used as non-pre hook."
	(condition-case-unless-debug ex
		(-let [(text hook-type) (fg-erc-get-hook-msg text)]
			;; (message "ERC filter test for: %S" text)
			;; (message "ERC msg props: %S"
			;; 	`(:net ,(or (symbol-name (erc-network)) "")
			;; 		:host ,(or erc-session-server "")
			;; 		:chan ,(or (erc-default-target) "")
			;; 		:msg-and-line ,(fg-string-strip-whitespace text)))
			;; (message "ERC filter test result: %s"
			;; 	(or
			;; 		(and (erc-list-match fg-erc-msg-block text) "fg-erc-msg-block")
			;; 		(dolist (rule fg-erc-msg-block-plists)
			;; 			(when (fg-erc-msg-match-rule rule text)
			;; 				(return-from nil "fg-erc-msg-block-plists")))
			;; 		(dolist (rule fg-erc-msg-modify-plists)
			;; 			(let ((rule-res (fg-erc-msg-match-rule rule text)))
			;; 				(when (s-contains? "waka waka" text)
			;; 					(message " - rule result: %s -- %s" rule-res rule))
			;; 				(when rule-res (return-from nil "fg-erc-msg-modify-plists"))))))
			(if
				;; check fg-erc-msg-block-plists
				(dolist (rule fg-erc-msg-block-plists)
					(when (fg-erc-msg-match-rule rule text) (return-from nil t)))
				(if (eq hook-type 'pre)
					(set 'erc-insert-this nil)
					(erc-put-text-property (point-min) (point-max) 'invisible t (current-buffer)))

				(-if-let ;; try to modify msg, if it's not blocked
					(func (and
						(eq hook-type 'post)
						(dolist (rule fg-erc-msg-modify-plists)
							(when (fg-erc-msg-match-rule rule text)
								(return-from nil (plist-get rule :func))))))
					(funcall func))))
		(t (warn "Error in ERC filter: %s" ex))))

(defun fg-erc-msg-match-rule (rule msg)
	"Match RULE against MSG.
Must be called from an ERC channel buffer, as it also matches
channel/network parameters."
	(let*
		((net (plist-get rule :net))
			(host (plist-get rule :host))
			(chan (plist-get rule :chan))
			(nick (plist-get rule :nick))
			(msg-pat-raw (plist-get rule :msg))
			(line (plist-get rule :line))
			(msg-pat (when (or nick msg-pat-raw)
				(fg-erc-msg-block-pattern (or nick "[^>]+") (or msg-pat-raw ""))))
			(msg (fg-string-strip-whitespace msg)))
		;; (message "--- erc-msg: %S" msg)
		(and
			(or (not net) (string-match net (or (symbol-name (erc-network)) "")))
			(or (not host) (string-match host (or erc-session-server "")))
			(or (not chan) (string-match chan (or (erc-default-target) "")))
			(or (not msg-pat) (string-match msg-pat msg))
			(or (not line) (string-match line msg)))))

;;;; This is most useful and simple way to test blocking - just erc-display-line in chan buffer
;; (with-current-buffer "#somechan"
;; 	(erc-display-line "-nick- some actual line" (current-buffer)))

;; (with-current-buffer (erc-get-buffer "#ccnx")
;; 	(let
;; 		((msg "[11:49:11]<someuser> some test msg")
;; 			(fg-erc-msg-block-plists
;; 				'((:nick "someuser" :net "Hype" :chan "cc"))))
;; 		(dolist (rule fg-erc-msg-block-plists)
;; 			(when (fg-erc-msg-match-rule rule msg) (return-from nil t)))))

;; Put the hook *before* erc-add-timestamp by remove/add dance
;; This only makes datestamp not be made invisible
;;  when it happens to be attached to filtered-out msg.
(let ((datestamp-hook (-contains? erc-insert-modify-hook 'erc-add-timestamp)))
	(when datestamp-hook
		(remove-hook 'erc-insert-modify-hook 'erc-add-timestamp))
	(add-hook 'erc-insert-modify-hook 'fg-erc-msg-content-filter)
	(when datestamp-hook
		(add-hook 'erc-insert-modify-hook 'erc-add-timestamp)))
;; (remove-hook 'erc-insert-modify-hook 'fg-erc-msg-content-filter)


;; --- Clears out annoying erc-track-mode stuff when I don't care
(defun fg-erc-track-reset ()
	(interactive)
	(setq erc-modified-channels-alist nil)
	(erc-modified-channels-display)
	(force-mode-line-update t))


;; --- New message notification hook
;; There is stock module for this these days, but this works too

(defvar fg-erc-notify-check-inivisible t
	"Whether to use `erc-string-invisible-p' on messages and skip unes that match.
Should only work with `erc-insert-post-hook', as that's when
these text-properties should be applied reliably (e.g. in `erc-insert-modify' hooks).")

(defun fg-erc-notify (&optional text)
	"`erc-insert-post-hook'  or `erc-insert-pre-hook' function
to send desktop notification about inserted message.
TEXT argument is processed by `fg-erc-get-hook-msg'."
	(-let [(text hook-type) (fg-erc-get-hook-msg text)]
		(when
			(and (eq hook-type 'post)
				fg-erc-notify-check-inivisible
				(erc-string-invisible-p (buffer-substring (point-min) (point-max))))
			(setq text nil))
		(when text
			(let*
				((buffer (current-buffer))
					(channel
						(or (erc-default-target) (buffer-name buffer)))
					(net (erc-network)))
				(when
					(and
						erc-session-server
						(or (not net) (string= net "") (string= net "Unknown")))
					(set 'net erc-session-server))
				(when
					(and (buffer-live-p buffer)
						(or
							(not (erc-buffer-visible buffer))
							(not (fg-xactive-check))))
					(condition-case-unless-debug ex
						(fg-notify (format "erc: %s [%s]" channel net) text :pixmap "erc" :strip t)
						(error
							(message "ERC notification error: %s" ex)
							(ding t))))))))


;; --- Local erc-highlight-nicknames mods
;; idea: from #erc
;; source: http://www.emacswiki.org/emacs/ErcNickColors
;; TODO: also check color-diff vs opposite bg, make sure color is visible on both kinds

(require 'color)

(defun* fg-erc-get-color-for-nick (nick &optional (min-delta 40))
	(fg-color-tweak
		(plist-get (custom-face-attributes-get 'default (selected-frame)) :background)
		(downcase nick) min-delta))

(defcustom fg-erc-highlight-name-lowercase t
	"Lowercase all highlighted nicks, to make them somewhat easier to read."
	:group 'erc :type 'boolean)

(defvar-local fg-erc-highlight-name-set nil
	"Nicks picked up from channel messages via regexps to be highlighted.
Filled by `fg-erc-highlight-nicknames', cleaned-up by `fg-erc-highlight-name-set-cleanup'.")

(defvar fg-erc-hlnsc-min-size 100
	"Min `fg-erc-highlight-name-set' size to trigger cleanup.
Intervals and chances are not checked if table is under this size.")

(defvar fg-erc-hlnsc-min-interval (* 2 60)
	"Minimal interval between `fg-erc-highlight-name-set' cleanups.
Intended to avoid slowing-down any kind of backlog dumps.")

(defvar fg-erc-hlnsc-max-interval (* 3 3600)
	"Interval after which `fg-erc-highlight-name-set-cleanup' triggers.")

(defvar fg-erc-hlnsc-chance (/ 1.0 300)
	"Chance for `fg-erc-highlight-name-set-cleanup' to trigger randomly between min/max intervals.")

(defvar fg-erc-hlnsc-nick-timeout (* 15 3600)
	"Remove nicks older than this on `fg-erc-highlight-name-set' cleanup.")

(defun fg-erc-highlight-nicknames-cleanup (name-set)
	"Cleanup NAME-SET according to fg-erc-hlnsc-* limits."
	(let*
		((ts (float-time))
			(ts-check-last (ht-get name-set 'last-cleanup ts)))
		(when
			(and
				(> (ht-size name-set) fg-erc-hlnsc-min-size)
				(< ts-check-last (- ts fg-erc-hlnsc-min-interval))
				(or
					(< ts-check-last (- ts fg-erc-hlnsc-max-interval))
					(< (random-float) fg-erc-hlnsc-chance)))
			(ht-set! name-set 'last-cleanup ts)
			(let ((ts-nick-min (- ts fg-erc-hlnsc-nick-timeout)))
				(ht-each (lambda (nick ts-nick)
					(when (< ts-nick ts-nick-min) (ht-remove! name-set nick))) name-set)))))

(defun fg-erc-highlight-nicknames ()
	"Hook to colorize nicknames in channel messages.
Uses both `fg-erc-highlight-name-set' and `erc-channel-users'.
Normalizes case for known nicks as well, if `fg-erc-highlight-name-lowercase' is set."
	(unless
		(condition-case nil (ht? fg-erc-highlight-name-set) (error nil))
		(setq-local fg-erc-highlight-name-set (ht-create)))
	(condition-case-unless-debug ex
		(save-excursion
			(goto-char (point-min))
			(let ((match-n 0))
				(while (re-search-forward "[^\s-]+" nil t) ; nick-like thing within msg line
					(setq match-n (1+ match-n))
					(let*
						((bounds (cons (match-beginning 0) (point)))
							(nick (buffer-substring-no-properties (car bounds) (cdr bounds)))
							(nick-self (erc-current-nick)))
						(when
							(and
								(< match-n 4) ;; should be somewhere at the beginning of line
								(string-match "^<\\(.*\\)>$" nick)) ;; must match regexp above as well
							(setq
								nick (match-string 1 nick)
								bounds (cons (1+ (car bounds)) (1- (cdr bounds))))
							(ht-set! fg-erc-highlight-name-set (downcase nick) (float-time))
							(fg-erc-highlight-nicknames-cleanup fg-erc-highlight-name-set))
						(when
							(and
								(or
									(and (erc-server-buffer-p) (erc-get-server-user nick))
									(and erc-channel-users (erc-get-channel-user nick))
									(ht-contains? fg-erc-highlight-name-set (downcase nick)))
								(not (string-equal nick nick-self)))
							(when fg-erc-highlight-name-lowercase
								(downcase-region (car bounds) (cdr bounds)))
							(put-text-property
								(car bounds) (cdr bounds) 'face
								(cons 'foreground-color (fg-erc-get-color-for-nick nick))))))))
		(error
			(message "ERC highlight error: %s" ex)
			(ding t))))

(add-hook 'erc-insert-modify-hook 'fg-erc-highlight-nicknames)


;; --- Local feature: putting a mark-lines into the buffers, via some hotkey

(defun fg-erc-mark-put (buffer)
	(erc-display-line " *** -------------------- ***" buffer))

(defun erc-cmd-MARK ()
	"Put a horizontal marker-line into a buffer. Purely aesthetic."
	(fg-erc-mark-put 'active))

(defun fg-erc-mark ()
	"Put a horizontal marker-line into a current buffer."
	(interactive)
	(when (eq major-mode 'erc-mode) (fg-erc-mark-put (current-buffer))))


;; --- Auto mark-lines
;; source: http://www.emacswiki.org/emacs/ErcBar

(defvar fg-erc-bar-threshold 1
	"Display bar when there are more than erc-bar-threshold unread messages.")

(defvar fg-erc-bar-overlay-color "dark red"
	"Color of the overlay line.")

(defvar fg-erc-bar-overlay nil
	"Overlay used to set bar.")

(defun fg-erc-bar-move-back (n)
	"Moves back n message lines. Ignores wrapping, and server messages."
	(interactive "nHow many lines ? ")
	(re-search-backward "^.*<.*>" nil t n))

(defun fg-erc-bar-update-overlay ()
	"Update the overlay for current buffer,
based on the content of erc-modified-channels-alist.
Should be executed on window change."
	(interactive)
	(let*
		((info (assq (current-buffer) erc-modified-channels-alist))
			(count (cadr info)))
		(if (and info (> count fg-erc-bar-threshold))
			(save-excursion
				(end-of-buffer)
				(when (fg-erc-bar-move-back count)
					(let ((inhibit-field-text-motion t))
						(move-overlay fg-erc-bar-overlay
							(line-beginning-position)
							(line-end-position)
							(current-buffer)))))
			(delete-overlay fg-erc-bar-overlay))))

(setq fg-erc-bar-overlay (make-overlay 0 0))
(overlay-put fg-erc-bar-overlay 'face `(:underline ,fg-erc-bar-overlay-color))

;; Put the hook *before* erc-modified-channels-update by remove/add dance
(defadvice erc-track-mode
	(after fg-erc-bar-setup-hook (&rest args) activate)
	(remove-hook 'window-configuration-change-hook 'fg-erc-bar-update-overlay)
	(add-hook 'window-configuration-change-hook 'fg-erc-bar-update-overlay))

(add-hook 'erc-send-completed-hook (lambda (str) (fg-erc-bar-update-overlay)))


;; --- Local feature: func to iterate over all erc channel buffers, for a hotkey

(defvar fg-erc-cycle-channels-return-buffer nil
	"Non-erc buffer to return to after going full-cycle over buffers.")
(defvar fg-erc-cycle-channels-pos-start nil)

(defun fg-erc-cycle-channels ()
	"Iterate (switch-to) over all erc channel buffers,
returning to the original one in the end."
	(interactive)
	(let*
		;; List of all channel buffers
		((buffer (current-buffer))
			(channel-buffers
				(sort*
					;; Don't cycle over already-visible buffers
					(remove-if
						(lambda (buff)
							(and
								(not (eq buffer buff))
								(erc-buffer-visible buff)))
						(erc-channel-list nil))
					;; Sort by buffer (=channel) name,
					;;  so they'll always be iterated over in roughly the same order
					'string-lessp :key 'buffer-name))
			(pos (position buffer channel-buffers)))
		(when (numberp pos) (setq pos (+ pos 1)))
		(when (or (not pos) (>= pos (length channel-buffers)))
			(unless pos
				;; Set return-buffer and reset pos-start
				(setq
					fg-erc-cycle-channels-pos-start nil
					fg-erc-cycle-channels-return-buffer buffer))
			(setq pos 0))
		(if
			(and
				fg-erc-cycle-channels-pos-start
				fg-erc-cycle-channels-return-buffer
				(buffer-live-p fg-erc-cycle-channels-return-buffer)
				(= fg-erc-cycle-channels-pos-start pos))
			;; Full cycle over buffers is complete, switch back to return-buffer
			(progn
				(setq
					buffer fg-erc-cycle-channels-return-buffer
					fg-erc-cycle-channels-pos-start nil
					fg-erc-cycle-channels-return-buffer nil)
				(switch-to-buffer buffer))
			;; Switch to some channel buffer
			(unless
				(and
					fg-erc-cycle-channels-pos-start
					fg-erc-cycle-channels-return-buffer)
				;; Starting a new cycle
				(setq fg-erc-cycle-channels-pos-start pos))
			(switch-to-buffer (nth pos channel-buffers)))))


;; --- Fix for reconnection loops, not sure if still needed or even works
;; Quick fail right after connection
;;   (like "password incorrect") will trigger infinite zero-delay reconnection loop by default.
;; This code fixes the problem, raising error for too fast erc-server-reconnect calls
(defvar fg-erc-reconnect-time 0
	"Timestamp of the last `erc-server-reconnect' run.
Prevents idiotic zero-delay reconnect loops from hanging emacs.")

(defadvice erc-server-reconnect (around fg-erc-server-reconnect-delay activate)
	(let*
		((time (float-time))
			(delay (- erc-server-reconnect-timeout (- time fg-erc-reconnect-time))))
		(if (> delay 0)
			(progn
				(message "Skipping erc-server-reconnect (for %d more secs)" delay)
				(error "erc-server-reconnect loop detected"))
			(setq fg-erc-reconnect-time time)
			ad-do-it)))


;; --- Local feature: func for fg-erc-msg-modify-plists to garble msgs instead of full muting
;; see also - Lunicode.js, ttf-zalgo on AUR, MetroWind/1401473 gist, etc

(defvar fg-erc-zalgo-overlay-count '(2 . 4)
	"Default OVERLAY-COUNT argument for `fg-erc-zalgo', if omitted.")
;; (setq fg-erc-zalgo-overlay-count '(2 . 4))

(defvar fg-erc-zalgo-overlay-ranges
	;; Glyphs should be picked from current font and produce roughly same length with overlay
	'( ; common punctuation and misc marks
		(#x00A1 . #x00BF) (#x02B9 . #x02FF) (#x2010 . #x2022) #x2026 (#x2032 . #x2034)
		#x2044 #x221A #x2211 #x2215 #x2229 #x2248 #x2261 (#x2320 . #x2321)
		(#x2550 . #x256A) #x2591 (#x25CA . #x25CC) (#x25CF . #x25D8) #x25E6
		#x263C #x2640 #x2642 (#x2669 . #x266B) #x266F #x2E17 (#xA717 . #xA721) (#xA788 . #xA78A)
		(#x03E2 . #x03EF) ; coptic
		(#x1F30 . #x1F3F) (#x1FBD . #x1FC1) (#x1FCD . #x1FDF) (#x1FED . #x1FEF) ; greek
		(#x0591 . #x05F4) (#xFB1D . #xFB4F) ; hebrew
		; latin
		(#x0268 . #x026D) (#x0279 . #x02A2) (#x02AD . #x02E4) (#x1D00 . #x1EF9)
		#x207F (#x2090 . #x2094) #x214E #x2184 (#x2C60 . #x2C77) #xA78B #xA78C)
	"List of (MIN . MAX) cons cells for ranges to pick overlay chars from.
Note that range is picked first, then char from it,
so distribution is not uniform among all source chars.")

(defun fg-erc-zalgo-char (c n)
	"Compose N random overlays (can be '(MIN . MAX) cons) onto char C."
	(if (memql (char-syntax c) '(32 ?. ?_)) (format "%c" c) ;; skip spaces/punctuation
		(when (consp n)
			(let ((a (car n)) (b (cdr n)))
				(setq n (+ a (random (- b a -1))))))
		(let
			((z (format "%c" c))
				(iter-n 0) (iter-n-limit 10) ;; in case font checks fail
				(ascii-font (car (internal-char-font nil ?x)))
				(rn (length fg-erc-zalgo-overlay-ranges)))
			(while (not (or (>= (length z) n) (>= iter-n iter-n-limit)))
				(let ((co (nth (random rn) fg-erc-zalgo-overlay-ranges)))
					(unless (numberp co)
						(let ((a (car co)) (b (cdr co)))
							(setq co (+ a (random (- b a -1))))))
					;; Note: can also use char-displayable-p here or something
					(if (eq ascii-font (car (internal-char-font nil co)))
						(setq z (format "%s%c" z co)) (setq iter-n (1+ iter-n)))))
			(compose-string z))))

(defun fg-erc-zalgo (text &optional overlay-count)
	"Return TEXT with random chars composed on top of it.
OVERLAY-COUNT can be either a number or
(MIN . MAX) cons to pick random number from range for each char,
and defaults to `fg-erc-zalgo-overlay-count'.
Overlay chars are picked from `fg-erc-zalgo-overlay-ranges'."
	(let ((n (or overlay-count fg-erc-zalgo-overlay-count 0)))
		(unless (equal 0 n)
			(cl-flet ((zalgo (c) (fg-erc-zalgo-char c n)))
				(setq text (apply #'concat (seq-map #'zalgo text))))))
	text)

;; Simple test: (insert (format "\n%s" (fg-erc-zalgo "zalgo lives" '(1 . 4))))
