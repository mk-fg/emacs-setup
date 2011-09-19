;;;; Python stuff

;; PyLookup
(defvar fg-pylookup-dir
	(concat fg-path "/extz/pylookup") "Pylookup directory")
(when (file-exists-p fg-pylookup-dir)
	(add-to-list 'load-path fg-pylookup-dir)

	(eval-after-load "pylookup" '(progn
		(setq-default
			pylookup-program (concat fg-pylookup-dir "/pylookup.py")
			pylookup-db-file (concat fg-pylookup-dir "/pylookup.db")
			browse-url-browser-function 'w3m-browse-url)
		(defun pylookup-mode-quit-window ()
			(interactive)
			(bury-buffer pylookup-temp-buffer-name))))

	(autoload 'pylookup-lookup "pylookup"
		"Lookup SEARCH-TERM in the Python HTML indexes." t)
	(autoload 'pylookup-update "pylookup"
		"Run pylookup-update and create the database at `pylookup-db-file'." t))
