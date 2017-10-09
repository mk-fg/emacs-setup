;;;; Python stuff


;; PyLookup

(defvar fg-pylookup-dir
	(concat fg-path "/extz/pylookup") "Pylookup directory")
(when (file-exists-p fg-pylookup-dir)
	(add-to-list 'load-path fg-pylookup-dir)

	(eval-after-load "pylookup" '(progn
		(setq-default
			pylookup-program (concat fg-pylookup-dir "/pylookup.py")
			pylookup-db-file (concat fg-path "/tmp/pylookup/pylookup.db")
			browse-url-browser-function 'w3m-browse-url)
		(defun pylookup-mode-quit-window ()
			(interactive)
			(bury-buffer pylookup-temp-buffer-name))))

	(autoload 'pylookup-lookup "pylookup"
		"Lookup SEARCH-TERM in the Python HTML indexes." t)
	(autoload 'pylookup-update "pylookup"
		"Run pylookup-update and create the database at `pylookup-db-file'." t))


;; Eval-as-python keys

(defvar fg-py-bin "python3"
	"Python binary to use for various eval/exec commands")

(defun fg-eval-py-wrap-expr (py-code)
	(setq py-code (s-trim py-code))
	(unless (s-contains? "\n" py-code)
		(setq py-code (concat "print(" (s-trim py-code) ", end='')")))
	py-code)

(defun fg-eval-py-message (start end)
	(let ((py-code (buffer-substring start end)))
		(with-temp-buffer
			(insert (fg-eval-py-wrap-expr py-code))
			(call-process-region
				(buffer-end -1) (buffer-end 1)
				fg-py-bin t t nil)
			(message "%s" (buffer-string)))))

(defun fg-eval-py-replace (start end)
	(let*
		((py-code (buffer-substring start end))
			(py-code-wrap (fg-eval-py-wrap-expr py-code)))
		(unless (string= (s-trim py-code) py-code-wrap)
			(delete-region start end)
			(save-excursion
				(set-marker (point-marker) start)
				(insert py-code-wrap)
				(setq end (point))))
		(call-process-region start end fg-py-bin t t nil)
		(when
			(and (s-ends-with? "\n" py-code) (/= (char-before) ?\n))
			(insert "\n"))))

(defun fg-eval-py (&optional whole-lines-only)
	(interactive)
	(save-excursion
		(fg-taint
			:call 'fg-eval-py-message
			:whole-lines-only whole-lines-only)))

(defun fg-eval-py-print (&optional whole-lines-only)
	(interactive)
	(fg-taint
		:call 'fg-eval-py-replace
		:whole-lines-only whole-lines-only))
