;; Stuff to call via emacsclient

(defun fg-remote (data)
	"Writes stringified DATA to a /tmp/.ece.remote.XXXX temp-file and returns filename.
Path and filename prefix are fixed and independent of e.g. `temporary-file-directory'.
Intended use is to call this wrapper on something that returns string
from emacsclient, match returned filename, read file contents, remove it."
	(let*
		((temporary-file-directory "/tmp/")
			(tmp (make-temp-file ".ece.remote.")))
		(append-to-file (format "%s" data) nil tmp)
		tmp))


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
