
;;;; Old EPA mode (for gpg), superseded by GHG stuff below

;; use ";; -*- epa-file-encrypt-to: ("mk.fraggod@gmail.com") -*-" in file headers

(require 'epa-file)

(defvar epa-select-keys-default-name "mk.fraggod@gmail.com"
	"Name to fallback to when selecting keys.")

(defvar epa-select-keys-inhibit t
	"Do not use interactive prompt for recipient keys,
using `epa-file-encrypt-to' value instead.")

(setq-default epa-file-encrypt-to epa-select-keys-default-name)


;; These will replace epa-select-keys function, leaving the old definition as
;;  epa-select-keys-interactive, so it can still be used, if necessary
(fset 'epa-select-keys-interactive (symbol-function 'epa-select-keys))

(defun epa-select-keys (context prompt &optional names secret encrypt-to)
	"Return all key(s) referenced by name(s) in
`epa-file-encrypt-to' instead or a popup selection prompt
if `epa-select-keys-inhibit' is set to nil or ENCRYPT-TO is non-nil.

Only auto-picks keys with ultimate validity and email
regexp-match against NAMES to make sure it's the right key.

See `epa-select-keys-interactive' for the description of other parameters."
	(if (or encrypt-to epa-select-keys-inhibit)
		(or
			(block :loop
				(when encrypt-to
					(setq epa-file-encrypt-to encrypt-to))
				(message "EPA selecting key to: %s" epa-file-encrypt-to)
				(dolist
					(key (epg-list-keys context epa-file-encrypt-to secret))
					(dolist (uid (epg-key-user-id-list key))
						(let*
							;; Match names vs epg-user-id-string
							((uid-string (epg-user-id-string uid))
								(uid-names ; nil if no matches
									(or
										(not names)
										(fg-keep-when
											(lambda (name)
												(string-match
													(concat "<" (regexp-quote name) ">\\s-*$")
													uid-string))
											names))))
							;; Check epg-user-id-validity
							(when
								(and uid-names
									(eq (epg-user-id-validity uid) 'ultimate))
								(message "EPA selected gpg key: %s [%s]" uid-string
									(substring (epg-sub-key-id
										(car (epg-key-sub-key-list key))) -8)) ; car here is the primary key
								(return-from :loop (list key)))))))
			(error
				(format "Failed to match trusted gpg key against name(s): %s" names)))
		(epa-select-keys-interactive context prompt names secret)))

(defun epa-file-select-keys-default ()
	"Select global-default recipients for encryption.
Same as `epa-file-select-keys', but always picks key matching `epa-select-keys-default-name'."
	(interactive)
	(make-local-variable 'epa-file-encrypt-to)
	(setq epa-file-encrypt-to
		(mapcar
			(lambda (key) (epg-sub-key-id (car (epg-key-sub-key-list key))))
			(epa-select-keys (epg-make-context) nil nil nil epa-select-keys-default-name))))



;;;; TRAMP mode

(require 'auth-source)
(require 'tramp)

(setq-default
	tramp-default-method "ssh")

;; (add-to-list 'tramp-default-user-alist
;; 	'("ssh" ".*\\.\\(mplik\\.ru\\|e1\\)\\'" "mkfg"))
;; (add-to-list 'tramp-default-proxies-alist
;; 	'(".*.\\(mplik\\.ru\\|e1\\)\\'" "\\`root\\'" "/ssh:mkfg@%h:"))

;; (setq auth-source-debug t)
;; (password-reset)
;; (auth-source-user-or-password "password" "db-six.mplik.ru" "sudo" "root")



;;;; GHG transparent encryption
;; Based on jka-compr-install, which has all the same hooks

(defvar ghg-bin "ghg"
	"ghg binary path/name to use, passed to `call-process-region'.")
(defvar ghg-args nil
	"List of arguments to always pass to ghg process before any others.")

(defvar ghg-enc-inhibit nil
	"Non-nil disables ghg processing temporarily. Can be useful to bind to t in el code.")

(defun ghg-enc-install ()
	"Enable hooks to transparently work with ghg-encrypted files."

	;; XXX: other potential hooks/overrides:
	;;  file-coding-system-alist
	;;  auto-mode-alist - for minor mode vars
	;;  inhibit-local-variables-suffixes - needed only for containers like tar/zip
	;;  load-file-rep-suffixes

	(--map
		(put it 'ghg-io-op (intern (concat "ghg-io-op-" (symbol-name it))))
		'(write-region insert-file-contents))
		;; XXX: not sure about these:
		;;  file-local-copy - jka-compr uncompresses it
		;;  load - for open-and-run-lisp ops, can be used for auth.el.ghg later
		;;  byte-compiler-base-file-name - for .elc files, probably shouldn't be used
	(let*
		((handler-spec
				'("\\.ghg\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)?\\'" . ghg-io-handler))
			(handler-cons (assoc (car handler-spec) file-name-handler-alist)))
		(unless handler-cons (push handler-spec file-name-handler-alist)))

	nil)

(ghg-enc-install)


(defun ghg-io-handler (operation &rest args)
	(save-match-data
		(let ((ghg-op (get operation 'ghg-io-op)))
			(if (and ghg-op (not ghg-enc-inhibit))
				(apply ghg-op args)
				;; (message "ghg - passed io op: %s %s" operation args)
				(ghg-io-run-real-handler operation args)))))

(defun ghg-io-run-real-handler (operation args)
	(let
		((inhibit-file-name-handlers
				(cons 'ghg-io-handler
					(and (eq inhibit-file-name-operation operation) inhibit-file-name-handlers)))
			(inhibit-file-name-operation operation))
		(apply operation args)))


(defun ghg-io-op-write-region (start end file &optional append visit lockname mustbenew)
	(let*
		((filename (expand-file-name file)) ;; actual filename
		 (visit-file (if (stringp visit) (expand-file-name visit) filename))) ;; displayed filename

		(when
			(and mustbenew (file-exists-p filename)
				(or (eq mustbenew t)
					(not (y-or-n-p (format "ghg - file exists: %s  -- overwrite?" visit-file)))))
			(error "ghg - file exists, not overwriting: %s" visit-file))
		(when append (error "ghg - file-append not implemented: %s" visit-file))

		(let
			((args (-concat ghg-args '("-eo")))
				(temp-buff (get-buffer-create " *ghg-io-temp*"))
				(coding-system-for-write 'no-conversion)
				(coding-system-for-read 'no-conversion))
			(with-current-buffer temp-buff (widen) (erase-buffer))
			(unless
				(= 0 (apply 'call-process-region
					start end ghg-bin nil (list temp-buff nil) nil args))
				;; XXX: collect stderr on errors (can be written to temp-file)
				(error "ghg - call failed: %s %s" ghg-bin args))
			(with-current-buffer temp-buff
				(ghg-io-run-real-handler 'write-region
					(list (point-min) (point-max) filename append 'dont))
				(erase-buffer)))))

(defun ghg-io-op-insert-file-contents (file &optional visit beg end replace)
	(barf-if-buffer-read-only)
	(and (or beg end) visit (error "Attempt to visit less than an entire file"))

	(let*
		(c-start c-size err-file-not-found
			(filename (expand-file-name file)))

		(condition-case err-tmp
			(let
				((args (-concat ghg-args '("-do")))
					(temp-buff (get-buffer-create " *ghg-io-temp*"))
					(coding-system-for-write 'no-conversion)
					(coding-system-for-read 'no-conversion))

				(with-current-buffer temp-buff (widen) (erase-buffer))
				(unless
					(= 0 (apply 'call-process
						ghg-bin filename (list temp-buff nil) nil args))
					;; XXX: collect stderr on errors (can be written to temp-file)
					(error "ghg - call failed: %s %s" ghg-bin args))

				(let ;; to prevent "really edit?" file-locking queries
					((buffer-file-name (if visit nil buffer-file-name)))
					(when replace (goto-char (point-min)))
					(setq c-start (point))
					(insert-buffer-substring temp-buff beg end)
					(with-current-buffer temp-buff (erase-buffer))
					(setq c-size (- (point) c-start))
					(if replace (delete-region (point) (point-max)))
					(goto-char c-start)))
			(file-error
				;; (message "ghg - file-error: %s" err-tmp)
				;; (file-error "Opening process input file"
				;;  "no such file or directory""/path/to/file.ghg")
				(if (eq (nth 3 err-tmp) filename)
					(progn
						(unless visit
							(signal 'file-error (cons "Opening input file" (nthcdr 2 err-tmp))))
						;; Error is delayed otherwise, until buffer-file-name and such are set
						(setq err-file-not-found (nth 2 err-tmp)))
					(error "Unhandled file-error when running ghg: %s" err-tmp))))

		(unless err-file-not-found
			(decode-coding-inserted-region
				(point) (+ (point) c-size)
				filename visit beg end replace))

		(when visit
			(unlock-buffer)
			(setq buffer-file-name filename)
			(set-visited-file-modtime)
			(when err-file-not-found
				(signal 'file-error (cons "Opening input file" err-file-not-found))))

		(list filename c-size)))
