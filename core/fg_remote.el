;; Stuff to call via emacsclient
;; TODO: cmd to save specified modified buffer
;; TODO: cmds to control emms

(require 'dns)
(setq-default
	server-use-tcp nil
	server-host ;; ipv6 stuff doesn't seem to be supported yet
		(dns-query-cached (concat system-name ".v4c"))
	server-port 6002)


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


(defun fg-remote-erc (&optional pattern)
	"WIthout PATTERN, displays last ERC activity in '<n> <chan>' (per line) format.
Otherwise same as `fg-remote-buffer-names', but only considers erc buffers,
and PATTERN can have special 'all', 'list' or 'l' values to set it to nil."
	(when (boundp 'erc-version-string)
		(if (not pattern)
			(--map
				(format "%03d %s"
					(cadr it) (buffer-name (car it)))
				erc-modified-channels-alist)
			(when (-contains? '("list" "l" "all") (format "%s" pattern)) (set 'pattern nil))
			(fg-remote-buffer-names pattern (erc-buffer-list)))))

(defun fg-remote-erc-mark (pattern)
	"Put /mark to a specified ERC chan and resets its activity track."
	(when (boundp 'erc-version-string)
		(with-current-buffer (fg-get-useful-buffer pattern (erc-buffer-list))
			(fg-erc-mark)
			(erc-modified-channels-remove-buffer (current-buffer))
			(erc-modified-channels-display))
		nil))


(defun fg-remote-log ()
	"Return contents of *Messages* buffer."
	(with-current-buffer "*Messages*"
		(buffer-substring-no-properties (point-min) (point-max))))
