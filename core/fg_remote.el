;; Stuff to call via emacsclient

(setq-default
	server-use-tcp nil
	server-host "0.0.0.0"
	server-port 6002)

(ignore-errors
	(require 'dns)
	(setq-default
		server-host ;; ipv6 stuff doesn't seem to be supported yet
			(dns-query-cached (concat system-name ".v4c"))))


(defun fg-remote (data)
	"Writes stringified DATA to a /tmp/.ece.remote.XXXX temp-file and returns filename.
Path and filename prefix are fixed and independent of e.g. `temporary-file-directory'.
If DATA is a list, it will be considered to be a list of (to-be-stringified) lines.
Final newline is ensured in DATA after converting it to string.
If DATA is nil, file won't be created.

Intended use is to call this wrapper on something that returns string
from emacsclient, match returned filename, read file contents, remove it."
	(when data
		(let*
			((temporary-file-directory "/tmp/")
				(tmp (make-temp-file ".ece.remote."))
				(data
					(s-join "\n"
						(--map (format "%s" it)
							(if (listp data) data (list data))))))
			(unless (s-ends-with? "\n" data) (set 'data (concat data "\n")))
			(write-region data nil tmp)
			tmp)))

(defun fg-remote-help (&optional cmd)
	"Return a list of remote commands
and all their aliases or info on specified CMD."
	(let ((pre "fg-remote-"))
		(if cmd
			(let ((func (intern (concat pre cmd))))
				(require 'help-fns)
				(format "%S\n\n%s"
					(help-make-usage func (help-function-arglist func))
					(documentation func)))
			(apply 'list
				"Available commands:"
				(--map
					(concat "  " it)
					(let (names)
						(mapatoms
							(lambda (sym)
								(let ((sym-name (format "%s" sym)))
									(when
										(and (fboundp sym)
											(s-starts-with? pre sym-name))
										(push
											(cons
												(s-chop-prefix pre
													(fg-real-function-name sym))
												(s-chop-prefix pre sym-name))
											names))))
							obarray)
						(-sort 'string<
							(--map
								(let ((cmd (car it)) (aliases (cdr it)))
									(format "%s%s" cmd
										(if (= (length aliases) 1) ""
											(format " (aliases: %s)"
												(s-join ", " (-sort 'string< (--filter
													(not (string= cmd it)) (-map 'cdr aliases))))))))
								(-group-by 'car names)))))))))

(defalias 'fg-remote-h 'fg-remote-help)
(defalias 'fg-remote-rtfm 'fg-remote-help)
(defalias 'fg-remote-wat 'fg-remote-help)


(defun fg-remote-switch-sockets (&optional tcp-or-unix)
	"Switch server between listening on tcp or unix socket, force-killing all clients.
Optional argument can be 'tcp or 'unix,
otherwise socket type is toggled based on `server-use-tcp'."
	(interactive)
	(set 'server-use-tcp
		(cond
			((eq tcp-or-unix 'tcp) t)
			((eq tcp-or-unix 'unix) nil)
			((not tcp-or-unix) (not server-use-tcp))
			(t (error "Invalid value for TCP-OR-UNIX: %s" tcp-or-unix))))
	(server-force-stop)
	(server-start)
	nil) ; clients will never see the result

(defun fg-remote-get-socket-type ()
	"Return current socket type in use as a 'tcp or 'unix sym."
	(if server-use-tcp 'tcp 'unix))


(defun* fg-remote-buffer (&optional pattern buffers &key list-hide-mod-mark)
	"Depending on whether PATTERN is specified, return
results of `fg-list-useful-buffer-names' as a newline-delimited string,
with '* ' prefix for ones that are marked as modified unless LIST-HIDE-MOD-MARK is set,
or text contents of a buffer with name matching (via `fg-get-useful-buffer') PATTERN."
	(if (not pattern)
		(let ((names (fg-list-useful-buffer-names nil buffers)))
			(if list-hide-mod-mark names
				(--map (concat (if (buffer-modified-p (get-buffer it)) "* " "  ") it) names)))
		(with-current-buffer (fg-get-useful-buffer pattern buffers)
			(buffer-substring-no-properties (point-min) (point-max)))))

(defalias 'fg-remote-buff 'fg-remote-buffer)
(defalias 'fg-remote-b 'fg-remote-buffer)

(defun* fg-remote-buffer-names (&optional pattern buffers)
	"Same as `fg-remote-buffer', but always returns a list of names,
optionally filtered by PATTERN. Uses `fg-list-useful-buffer-names' for filtering."
	(fg-list-useful-buffer-names pattern buffers))

(defalias 'fg-remote-bn 'fg-remote-buffer-names)

(defun* fg-remote-buffer-save (pattern &optional overwrite buffers)
	"Saves the buffer matching (via `fg-get-useful-buffer') PATTERN.
Any queries during save (e.g. file was modified by something else) will signal error,
unless OVERWIRITE is specified and matches one of the following:
* 'mod' - answer 'y' to 'has changed since visited or saved' query, if any.
* 'exists' - answer 'y' to 'file exists, overwrite?' query, if any.
* 'all', 'y', 't', 'yes' - answer 'y' to all of the queries listed above, but not any unknown ones.
* 'force' - answer 'y' on all queries asked in process, including any unmatched (unknown) ones."
	(let ((suppress-all '(mod exists force)) suppress)
		(when (and overwrite (not (symbolp overwrite)))
			(set 'overwrite (intern overwrite)))
		(if (-contains? suppress-all overwrite)
			(set 'suppress (cons overwrite suppress))
			(if (-contains? '(all y t yes) overwrite) ; special composite values
				(set 'suppress (-cons* 'mod 'exists suppress))
				(when overwrite
					(error "Unrecognized value for OVERWRITE: %s" overwrite))))
		(with-current-buffer (fg-get-useful-buffer pattern buffers)
			(flet
				((check-tag (prompt &optional tag prompt-pat)
						(when (or (not prompt-pat) (s-match prompt-pat prompt))
							(if
								(or (-contains? suppress 'force) (and tag (-contains? suppress tag)))
								t (error "File save canceled on prompt (tag: %s): %s" tag prompt))))
					(ask-user-about-supersession-threat (fn)
						(unless (-contains? suppress 'mod)
							(error "File save canceled on supersession-threat (use 'mod' tag to override)")))
					(y-or-n-p (prompt)
						(cond ;; see `basic-save-buffer'
							((check-tag prompt 'mod
								" has changed since visited or saved. +Save anyway\\? *$") t)
							((check-tag prompt 'exists " exists; overwrite\\? *$") t)
							((check-tag prompt) t)
							(t (error "Bug in `fg-remote-buffer-save'")))))
				(basic-save-buffer)))))

(defalias 'fg-remote-buff-save 'fg-remote-buffer-save)
(defalias 'fg-remote-bs 'fg-remote-buffer-save)

(defun* fg-remote-buffer-kill (pattern &optional buffers)
	"Kill buffer matching (via `fg-get-useful-buffer') PATTERN.
It doesn't matter if buffer is modified or if there's
an associated process, it will still be killed without any prompt.
Obviously dangerous to any possible unsaved changes."
	(with-current-buffer (fg-get-useful-buffer pattern buffers)
		(let (kill-buffer-query-functions)
			(-when-let
				(proc (get-buffer-process (current-buffer)))
				(delete-process proc))
			(set-buffer-modified-p nil)
			(kill-buffer)))
	nil)

(defalias 'fg-remote-buff-kill 'fg-remote-buffer-kill)
(defalias 'fg-remote-bk 'fg-remote-buffer-kill)


(defun fg-remote-erc (&optional pattern)
	"WIthout PATTERN, displays last erc activity in '<n> <chan>' (per line) format.
Otherwise, same as `fg-remote-buffer', gets specified erc buffer contents,
picking only from buffers with activity (i.e. the ones that are disaplayed without PATTERN)."
	(when (boundp 'erc-version-string)
		(if (not pattern)
			(when erc-modified-channels-alist
				(let*
					((digits (-max (-map 'cadr erc-modified-channels-alist)))
						(fmt (format "%%0%dd %%s" (or
							(multiple-value-bind
								(digits overflow) (round* (log digits 10))
								(if (> overflow 0) (1+ digits) digits)) ""))))
					(--map
						(format fmt (cadr it) (buffer-name (car it)))
						erc-modified-channels-alist)))
			(with-current-buffer
				(fg-get-useful-buffer pattern (-map 'car erc-modified-channels-alist))
				(buffer-substring-no-properties (point-min) (point-max))))))

(defalias 'fg-remote-e 'fg-remote-erc)

(defun fg-remote-erc-names (&optional pattern)
	"Same as `fg-remote-buffer-names', but only considers erc buffers.
Unlike `fg-remote-erc', considers all erc buffers, not just ones with activity."
	(fg-remote-buffer-names pattern (erc-buffer-list)))

(defalias 'fg-remote-en 'fg-remote-erc-names)

(defun fg-remote-erc-mark (pattern)
	"put /mark to a specified erc chan and resets its activity track.
Unlike `fg-remote-erc', considers all erc buffers, not just ones with activity."
	(when (boundp 'erc-version-string)
		(with-current-buffer (fg-get-useful-buffer pattern (erc-buffer-list))
			(fg-erc-mark)
			(erc-modified-channels-remove-buffer (current-buffer))
			(erc-modified-channels-display))
		nil))

(defalias 'fg-remote-em 'fg-remote-erc-mark)

(defun fg-remote-erc-cat-mark (pattern)
	"Combined `fg-remote-erc' and `fg-remote-erc-mark' commands.
Will get the active buffer contents, put a mark there, and return the contents."
	(prog1
		(fg-remote-erc pattern)
		(fg-remote-erc-mark pattern)))

(defalias 'fg-remote-ecm 'fg-remote-erc-cat-mark)


(defun fg-remote-log ()
	"Return contents of *Messages* buffer."
	(with-current-buffer "*Messages*"
		(buffer-substring-no-properties (point-min) (point-max))))


(defun fg-remote-eval (path)
	"Load and eval file at the specified local PATH.
If PATH is already opened in some buffer, error is signaled.
Result of the eval operation is not returned."
	(flet
		((kill-buffer-force ()
			(let (kill-buffer-query-functions)
				(-when-let
					(proc (get-buffer-process (current-buffer)))
					(delete-process proc))
				(set-buffer-modified-p nil)
				(kill-buffer))))
		(save-excursion
			(-when-let (buff (find-buffer-visiting path))
				(error "Specified path already opened in buffer: %s" (buffer-name buff)))
			(find-file path) ; not sure if there might still be some interactivity
			(condition-case err
				(prog1 (eval-buffer) (kill-buffer-force))
				(error (kill-buffer-force) (signal (car err) (cdr err)))))))


(defun fg-remote-emms (&optional action)
	"Show current emms track or perform specified ACTION.

Supported actions:
* play/pause (toggled, alias: p)
* track-next (alias: >, +)
* track-prev (alias: <, -)
* stop (alias: s)
* notify (desktop notification, alias: n)
* shuffle (for playlist)
* sort (for playlist)
* clear (clear playlist, alias: c)
* status (stopped, playing or paused)
* path (current track path)
* playlist (paths of all tracks, one per line, alias: m3u)

There's also separate emms-add (ea) command to add stuff to playlist."
	(if (not action)
		(let
			((track (emms-playlist-current-selected-track)))
			(format "[%s]%s%s"
				(fg-emms-player-status-string)
				(if (= 0 (length emms-playing-time-string))
					"" (format " %s" (s-trim emms-playing-time-string)))
				(if (not track) "" (format " :: %s" (or (emms-track-description track) "")))))
		(case (intern action)
			((play pause p) (emms-pause))
			((track-next > +) (emms-next))
			((track-prev < -) (emms-previous))
			((stop s) (emms-stop))
			((notify n) (fg-emms-notify))
			((shuffle) (prog1 nil (emms-shuffle)))
			((sort) (prog1 nil (emms-sort)))
			((clear c) (emms-playlist-mode-clear))
			((status) (fg-emms-player-status-string))
			((path) (emms-track-get (emms-playlist-current-selected-track) 'name))
			((playlist m3u)
				(with-temp-buffer
					(emms-source-playlist-unparse 'm3u
						(with-current-emms-playlist (current-buffer)) (current-buffer))
					(buffer-string)))
			(t (error "Unknown action: %s" action)))))


(defun fg-remote-emms-add (&rest paths)
	"Recursively add path(s) (or glob(s)) to emms playlist."
	(-each paths 'fg-emms-add-directory-tree-glob))

(defalias 'fg-remote-ea 'fg-remote-emms-add)
