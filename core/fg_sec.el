;;;; GHG transparent encryption - https://github.com/mk-fg/ghg
;; Based on jka-compr-install, which has all the same hooks

(defvar ghg-bin "ghg"
	"ghg binary path/name to use, passed to `call-process-region'.")
(defvar ghg-args nil
	"List of arguments to always pass to ghg process before any others.")

(defvar ghg-enc-inhibit nil
	"Non-nil disables ghg processing temporarily. Can be useful to bind to t in el code.")

(defun ghg-enc-install ()
	"Enable hooks to transparently work with ghg-encrypted files."

	;; XXX: not sure about these io-ops:
	;;  file-local-copy - jka-compr uncompresses it
	;;  load - for open-and-run-lisp ops, can be used for auth.el.ghg later
	;;  byte-compiler-base-file-name - for .elc files, probably shouldn't be used
	(--map
		(put it 'ghg-io-op (intern (concat "ghg-io-op-" (symbol-name it))))
		'(write-region insert-file-contents))

	;; XXX: other potential overrides:
	;;  inhibit-local-variables-suffixes - needed only for containers like tar/zip
	;;  load-file-rep-suffixes
	(let ((ext-regexp "\\.ghg\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)?\\'"))
		(add-to-list 'auto-mode-alist (list ext-regexp nil 'ghg-file))
		(add-to-list 'auto-coding-alist (cons ext-regexp 'no-conversion))
		(add-to-list 'file-name-handler-alist (cons ext-regexp 'ghg-io-handler))
		(add-to-list 'file-coding-system-alist `(,ext-regexp no-conversion . no-conversion)))

	nil)

(ghg-enc-install) ;; XXX: can be moved into mode


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


(defvar ghg-io-coding-system-default 'utf-8-unix
	"Default coding system to use when e.g. opening files, if unspecified.
Simply leaving it up to emacs doesn't seem to work well,
esp. since utf-8 is used everywhere and guessing doesn't make much sense anymore.")

(defvar ghg-io-coding-system-force t
	"If non-nil, override buffer-specific coding system
settings for ghg buffers to `ghg-io-coding-system-default'.
Idea is to prevent arbitrary coding system 'drift' into nonsense,
which emacs seem to do with its prefer-* stuff.")

(defun ghg-io-coding-system-fix ()
	(when ghg-io-coding-system-force
		(setq
			buffer-file-coding-system ghg-io-coding-system-default
			save-buffer-coding-system ghg-io-coding-system-default
			last-coding-system-used ghg-io-coding-system-default)))


(defun ghg-io-op-write-region (start end file &optional append visit lockname mustbenew)
	(let*
		((coding-system-used last-coding-system-used)
			(filename (expand-file-name file)) ;; actual filename
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
			(with-current-buffer temp-buff (erase-buffer))
			(unless
				(= 0 (apply 'call-process-region
					start end ghg-bin nil (list temp-buff nil) nil args))
				;; XXX: collect stderr on errors (can be written to temp-file)
				(error "ghg - call failed: %s %s" ghg-bin args))
			(with-current-buffer temp-buff
				(ghg-io-run-real-handler 'write-region
					(list (point-min) (point-max) filename append 'dont))
				(erase-buffer)))

		(cond
			((eq visit t)
				(setq buffer-file-name filename)
				(set-visited-file-modtime))
			((stringp visit)
				(setq buffer-file-name visit)
				(let ((buffer-file-name filename))
				(set-visited-file-modtime))))
		(setq last-coding-system-used coding-system-used))

	(ghg-io-coding-system-fix)
	nil)

(defun ghg-io-op-insert-file-contents (file &optional visit beg end replace)
	(barf-if-buffer-read-only)
	(and (or beg end) visit (error "Attempt to visit less than an entire file"))

	(let*
		(c-start c-size err-file-not-found
			(coding-base coding-system-for-read)
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
			(let
				((coding-system-for-read
					(or coding-base ghg-io-coding-system-default)))
				(decode-coding-inserted-region
					(point) (+ (point) c-size)
					filename visit beg end replace)))

		(when visit
			(unlock-buffer)
			(setq buffer-file-name filename)
			(set-visited-file-modtime)
			(when err-file-not-found
				(signal 'file-error (cons "Opening input file" err-file-not-found))))

		(ghg-io-coding-system-fix)
		(list filename c-size)))


;;;; FHD - fido2-hmac-desalinate short-string encryption helpers
;; https://github.com/mk-fg/fgtk#fido2-hmac-desalinate-c
;;
;; fhd-crypt func here is used for a simple password-manager replacement.
;; When called from a hotkey in a buffer, it either decrypts and copies
;;  pointed-to secret, or replaces it with encrypted/decrypted version, if M-` is set.
;; Run (setq fhd-bin "echo" fhd-args '("-n" "some-secret")) and try it out.

(defvar fhd-bin "fhd"
	"fido2-hmac-desalinate binary to use, passed to `make-process' in `fhd-crypt'.")
(defvar fhd-args nil
	"List of static command-line args to pass to `fhd-bin' process.")
(defvar fhd-args-dev "/dev/fido2"
	"Device path to pass to `fhd-bin' as arg, if exists, and `fhd-args' is nil.")
(defvar fhd-proc nil
	"Currently running `fhd-bin' process for some pending operation.")
;; (setq fhd-bin "fhd" fhd-args nil) (setq fhd-bin "echo" fhd-args '("-n" "some-output"))

(defun fhd-crypt ()
	"Encrypt or decrypt thing at point or a region-selected one (but trimmed of spaces).
Starts async `fhd-proc', with result signaled in minibuffer and copied into clipboard.
Universal argument can be set to replace the thing at point or selected region,
instead of using `fhd-share-secret'.
Rejects short at-point strings to avoid handling parts by mistake, use region for those."
	(interactive)
	;; Get PW secret or fhd-token to process
	(let
		((pw-chars "^[:space:]\n")
			(pw (when (use-region-p)
				(buffer-substring-no-properties (region-beginning) (region-end))))
			(replace (and (listp current-prefix-arg) (car current-prefix-arg) (current-buffer)))
			salt data enc)
		(if pw
			(setq replace
				(when (and replace (use-region-p))
					(list replace (region-beginning) (region-end))))
			(save-excursion
				(skip-chars-backward pw-chars)
				(setq pw (point))
				(skip-chars-forward pw-chars)
				(when replace (setq replace (list replace pw (point))))
				(setq pw (buffer-substring-no-properties pw (point)))))
		;; Parse/encode token to SALT and DATA, setting ENC direction-flag
		(if (and (not (use-region-p)) (< (length pw) 8))
			(message "FHD-ERR: secret cannot be that short [ %s ]" pw)
			(if (string-match "^fhd\\.\\([^.]+\\)\\.\\(.*\\)$" pw)
				(setq salt (match-string 1 pw) data (match-string 2 pw))
				(setq salt (fg-random-string 4) data
					(base64-encode-string (fg-string-strip-whitespace pw) t) enc t))
			(if (process-live-p fhd-proc)
				(message "FHD-ERR: another process already running")
				;; Start fhd process
				(let
					((stdout (get-buffer-create " fhd-stdout"))
						(stderr (get-buffer-create " fhd-stderr"))
						(fhd-args (or fhd-args
							(and (file-exists-p fhd-args-dev) (list fhd-args-dev)))))
					(with-current-buffer stdout (erase-buffer))
					(with-current-buffer stderr (erase-buffer))
					(setq fhd-proc (make-process
						:name "fhd" :command (cons fhd-bin fhd-args)
						:buffer stdout :stderr stderr :noquery t :connection-type 'pipe
						:coding '(no-conversion . no-conversion) :sentinel #'fhd-proc-sentinel))
					(process-put fhd-proc 'fhd-salt salt)
					(process-put fhd-proc 'fhd-enc enc)
					(process-put fhd-proc 'fhd-replace replace)
					(process-put fhd-proc 'fhd-stderr stderr)
					(process-send-string fhd-proc (format "%s %s" salt data))
					(process-send-eof fhd-proc))
				(message "FHD: %scryption process started" (if enc "en" "de"))))))

(defun fhd-proc-sentinel (proc ev)
	"Prints success/error info, either running `fhd-share-secret' on result,
or replacing original (buffer a b) place if REPLACE is used."
	(unless (process-live-p fhd-proc)
		(setq fhd-proc nil)
		(let
			((code (process-exit-status proc))
				(out (with-current-buffer
					(process-buffer proc) (prog1 (buffer-string) (kill-buffer))))
				(err (fg-string-strip-whitespace (with-current-buffer
					(process-get proc 'fhd-stderr) (prog1 (buffer-string) (kill-buffer)))))
				(salt (process-get proc 'fhd-salt))
				(enc (process-get proc 'fhd-enc))
				(replace (process-get proc 'fhd-replace)))
			(if (= code 0)
				(let
					((result (if (not enc) out (format
						"fhd.%s.%s" salt (base64-encode-string out t)))))
					(if replace
						(cl-multiple-value-bind (buff a b) replace
							(with-current-buffer buff (save-excursion
								(delete-region a b) (goto-char a) (insert result))))
						(fhd-share-secret result enc)))
				(message (format
					"FHD-ERR [exit=%d]: %s" code (fg-string-or err "<no-stderr>")))))))

(defun fhd-share-secret (s &optional enc)
	"Share/copy secret S with other apps on the system, issuing any notifications.
ENC should be non-nil if secret is a result of encryption, as opposed to decryption."
	;; Simple fg-copy-string is not ideal here, as it puts secrets into emacs kill-ring
	;; Emacs doesn't paste from its own gui-select-text, hence using "exclip" here
	;; exclip also allows to time-out the secrets easily, for some additional safety
	;; (exclip binary is from mk-fg/fgtk repo here - https://github.com/mk-fg/fgtk#exclip)
	;; gui-select-text adds prefix char to fool emacs into thinking that it's a diff selection
	(let (select-enable-clipboard (select-enable-primary t)) (gui-select-text (concat "#" s)))
	(call-process "exclip" nil 0 nil "-xpb" "120") ; -p removes first byte added above
	(let ((output (if enc "ciphertext" "secret")))
		(fg-notify (format "fhd: %s copied to clipboard" output))
		(message "FHD: %s copied to clipboard" output)))
