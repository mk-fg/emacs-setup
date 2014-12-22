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
	"Return results of `fg-list-useful-buffers' as a newline-delimited string."
	(s-join "\n" (fg-list-useful-buffers pattern)))

(defun fg-remote-get-buffer (pattern)
	"Return text contents of a buffer with name matching PATTERN.
Matching is done via `fg-list-useful-buffers'.
Error is signaled if there is more than one match."
	(let ((names (fg-list-useful-buffers pattern)))
		(if (/= (length names) 1)
			(error (concat "Failed to uniquely match"
				" buffer by `%s', matches: %s") (s-join ", " names))
			(with-current-buffer (car names)
				(buffer-substring-no-properties (point-min) (point-max))))))
