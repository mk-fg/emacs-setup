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

;; Ropemacs, Pymacs, Yasnippet
(when nil (eval-after-load "python" '(progn
	(require 'auto-complete nil t)
	(require 'yasnippet nil t)

	(progn
		;; Initialize Pymacs
		(autoload 'pymacs-apply "pymacs")
		(autoload 'pymacs-call "pymacs")
		(autoload 'pymacs-eval "pymacs" nil t)
		(autoload 'pymacs-exec "pymacs" nil t)
		(autoload 'pymacs-load "pymacs" nil t)
		;; Initialize Rope
		(pymacs-load "ropemacs" "rope-")
		(setq-default ropemacs-enable-autoimport t))

	;; Initialize Yasnippet
	;; TAB will only be used indirectly, so it shouldn't be mapped
	(setq-default yas/trigger-key (kbd "<C-M-S-s-XF86ClearGrab>"))
	(prog1
		(yas/initialize)
		(let ((snippets-dir (concat fg-path "/snippets")))
			(when (file-exists-p snippets-dir)
				(yas/load-directory snippets-dir))))


	;;; Auto-completion
	;;;  Integrates:
	;;;   1) Rope
	;;;   2) Yasnippet
	;;;   all with AutoComplete.el

	(defun prefix-list-elements (list prefix)
		(let (value)
			(nreverse
			 (dolist (element list value)
				(setq value (cons (format "%s%s" prefix element) value))))))
	(defvar ac-source-rope
		'((candidates
			 . (lambda ()
					 (prefix-list-elements (rope-completions) ac-target))))
		"Source for Rope")
	(defun ac-python-find ()
		"Python `ac-find-function'."
		(require 'thingatpt)
		(let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
			(if (null symbol)
					(if (string= "." (buffer-substring (- (point) 1) (point)))
							(point)
						nil)
				symbol)))
	(defun ac-python-candidate ()
		"Python `ac-candidates-function'"
		(let (candidates)
			(dolist (source ac-sources)
				(if (symbolp source)
						(setq source (symbol-value source)))
				(let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
							 (requires (cdr-safe (assq 'requires source)))
							 cand)
					(if (or (null requires)
									(>= (length ac-target) requires))
							(setq cand
										(delq nil
													(mapcar (lambda (candidate)
																		(propertize candidate 'source source))
																	(funcall (cdr (assq 'candidates source)))))))
					(if (and (> ac-limit 1)
									 (> (length cand) ac-limit))
							(setcdr (nthcdr (1- ac-limit) cand) nil))
					(setq candidates (append candidates cand))))
			(delete-dups candidates)))
	(add-hook 'python-mode-hook
						(lambda ()
									 (auto-complete-mode 1)
									 (set (make-local-variable 'ac-sources)
												(append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
									 (set (make-local-variable 'ac-find-function) 'ac-python-find)
									 (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
									 (set (make-local-variable 'ac-auto-start) nil)))

	;;Ryan's python specific tab completion
	(defun ryan-python-tab ()
		; Try the following:
		; 1) Do a yasnippet expansion
		; 2) Do a Rope code completion
		; 3) Do an indent
		(interactive)
		(if (eql (ac-start) 0)
				(indent-for-tab-command)))

	(defadvice ac-start (before advice-turn-on-auto-start activate)
		(set (make-local-variable 'ac-auto-start) t))
	(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
		(set (make-local-variable 'ac-auto-start) nil))

	(define-key python-mode-map (key "<M-f5>") 'ryan-python-tab))))
