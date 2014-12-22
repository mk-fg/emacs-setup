;; Stuff to call via emacsclient

(require 'dns)
(setq-default
	server-use-tcp nil
	server-host ;; ipv6 stuff doesn't seem to be supported yet
		(dns-query-cached (concat system-name ".v4c"))
	server-port 6002)


(defun fg-remote (data)
	"Writes stringified DATA to a /tmp/.ece.remote.XXXX temp-file and returns filename.
Path and filename prefix are fixed and independent of e.g. `temporary-file-directory'.
Final newline is ensured in DATA after converting it to string.
If DATA is nil, file won't be created.

Intended use is to call this wrapper on something that returns string
from emacsclient, match returned filename, read file contents, remove it."
	(when data
		(let*
			((temporary-file-directory "/tmp/")
				(tmp (make-temp-file ".ece.remote.")))
			(set 'data (format "%s" data))
			(unless (s-ends-with? "\n" data) (set 'data (concat data "\n")))
			(append-to-file data nil tmp)
			tmp)))


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


(defun fg-remote-list-buffers (&optional pattern)
	"Results of `fg-list-useful-buffers' as a newline-delimited string."
	(s-join ""
		(--map (concat (s-trim it) "\n")
			(fg-list-useful-buffers pattern))))

(defun fg-remote-get-buffer (pattern)
	"Text contents of a buffer with name matching PATTERN.
Matching is done via `fg-list-useful-buffers'.
Error is signaled if there is more than one match."
	(let ((names (fg-list-useful-buffers pattern)))
		(if (/= (length names) 1)
			(error (concat "Failed to uniquely match"
				" buffer by `%s', matches: %s") pattern (s-join ", " names))
			(with-current-buffer (car names)
				(buffer-substring-no-properties (point-min) (point-max))))))


(defun fg-remote-erc-activity ()
	"ERC activity in '<n> <chan>' (per line) format."
	(--map
		(format "%03d %s\n"
			(buffer-name (car it)) (cadr it))
		erc-modified-channels-alist))

(defun fg-remote-erc-mark (pattern)
	"Put /mark to a specified ERC chan."
	(let
		((names
			(--filter
				(with-current-buffer it (eq major-mode 'erc-mode))
				(fg-list-useful-buffers pattern))))
		(if (/= (length names) 1)
			(error (concat "Failed to uniquely match"
				" ERC buffer by `%s', matches: %s") pattern (s-join ", " names))
			(with-current-buffer (car names) (fg-erc-mark)))))
