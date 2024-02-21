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


;;;; FHD - fido2-hmac-desalinate and such short-string encryption helpers
;; https://github.com/mk-fg/fgtk#hdr-_hsm_fido2_piv_etc_smartcard_stuff
;;
;; fhd-crypt func here is used for a simple password-manager replacement.
;; When called from a hotkey in a buffer, it either decrypts and copies
;;  pointed-to secret, or replaces it with encrypted/decrypted version, if M-` is set.
;; (setq fhd-bin "cat" fhd-args '("-n" "some-secret")) can be used for fixed output.

(defvar fhd-bin "fhd"
	"fido2-hmac-desalinate binary to use, passed to `make-process' in `fhd-crypt'.")
(defvar fhd-bak "secret-token-backup"
	"secret-token-backup script, to use as a wrapper around `fhd-bin' when wrapping
new secrets, to make sure those can survive `fhd-dev' being broken or missing.")
(defvar fhd-args nil
	"List of static command-line args to pass to `fhd-bin' process.")
(defvar fhd-proc nil
	"Currently running `fhd-bin' process for some pending operation.")
(defvar fhd-dir nil
	"Working directory for running `fhd-bin' and `fhd-bak' commands under.
When nil, dir of the `buffer-file-name' where `fhd-crypt' was run will be used, if any.")
;; (setq fhd-bin "fhd" fhd-args nil) (setq fhd-bin "echo" fhd-args '("-n" "some-output"))

(defvar fhd-dev "/dev/fido2-fhd"
	"Device path to pass to `fhd-bin' as arg, if exists, and `fhd-args' is nil.
Can be used for both wrapping and unwrapping of all \"fhd.<salt>.<ct>\" secrets.")
(defvar fhd-dev-bak "/dev/fido2-fha"
	"Device path to check to use `fhd-bak' script for read-only secret retrieval.
Only checked and used when `fhd-dev' is not available for decryption.
If neither `fhd-dev' nor this node exist, calls to `fhd-crypt' will always fail.")

(defun fhd-crypt (&optional dec-proc)
	"Encrypt or decrypt thing at point or a region-selected one (but trimmed of spaces).
Starts async `fhd-proc', with result signaled in minibuffer and copied into clipboard.
Non-nil DEC-PROC will be passed to `fhd-share-secret' on decryption operation.
Universal argument can be set to replace the thing at point or selected region,
instead of using `fhd-share-secret'.
Rejects short at-point strings to avoid handling parts by mistake, use region for those."
	(interactive)
	(let
		((pw-chars "^[:space:]\n")
			(pw (when (use-region-p)
				(buffer-substring-no-properties (region-beginning) (region-end))))
			(replace (and (listp current-prefix-arg) (car current-prefix-arg) (current-buffer)))
			(fhd-dir (or fhd-dir (let ((p (buffer-file-name))) (and p (file-name-directory p)))))
			pw-pos salt data dec fhd-cmd stdout stderr)
		(if pw ;; Get PW secret or fhd-token to process
			(setq replace (when (and replace (use-region-p))
				(list replace (setq pw-pos (region-beginning)) (region-end))))
			(save-excursion
				(skip-chars-backward pw-chars)
				(setq pw (setq pw-pos (point)))
				(skip-chars-forward pw-chars)
				(when replace (setq replace (list replace pw (point))))
				(setq pw (buffer-substring-no-properties pw (point)))))
		(when ;; Run sanity-checks and proceed if fhd-cmd is set from fhd-proc-cmd
			(setq fhd-cmd (if (and (not (use-region-p)) (< (length pw) 8))
				(not (message "FHD-ERR: secret cannot be that short [ %s ]" pw))
				;; Parse/encode token to SALT and DATA, setting DEC direction-flag
				(if (string-match "^fhd\\.\\([^.]+\\)\\.\\(.*\\)$" pw)
					(setq salt (match-string 1 pw) data (match-string 2 pw) dec t)
					(setq salt (fg-random-string 4) data
						(base64-encode-string (fg-string-strip-whitespace pw) t)))
				(if (not (process-live-p fhd-proc))
					(if fhd-dir (fhd-proc-cmd pw-pos dec)
						(not (message "FHD-ERR: failed to determine buffer-file-name dir")))
					(not (message "FHD-ERR: another process already running")))))
			(setq
				stdout (get-buffer-create " fhd-stdout")
				stderr (get-buffer-create " fhd-stderr"))
			(with-current-buffer stdout (erase-buffer))
			(with-current-buffer stderr (erase-buffer))
			(setq fhd-proc (let ((default-directory fhd-dir)) (make-process
				:name "fhd" :command fhd-cmd
				:buffer stdout :stderr stderr :noquery t :connection-type 'pipe
				:coding '(no-conversion . no-conversion) :sentinel #'fhd-proc-sentinel)))
			(process-put fhd-proc 'fhd-salt salt)
			(process-put fhd-proc 'fhd-dec (when dec (or dec-proc t)))
			(process-put fhd-proc 'fhd-replace replace)
			(process-put fhd-proc 'fhd-stderr stderr)
			(process-send-string fhd-proc (format "%s %s" salt data))
			(process-send-eof fhd-proc)
			(message "FHD: %scryption process started" (if dec "de" "en")))))

(defun fhd-comment-from-path (pw-pos)
	"Return a string with prefix before PW-POS
and preceding indented headers above leading up to it,
with any fhd-strings scrubbed from it, and newlines/tabs escaped."
	(let (comment indent)
		(save-excursion
			(goto-char pw-pos)
			(let ((prefix (buffer-substring-no-properties (line-beginning-position) pw-pos)))
				(setq indent (fg-string-match "^[ \t]+" prefix))
				(push prefix comment)) ; initial prefix before pw
			(while (and (> (length indent) 0) (= (forward-line -1) 0)) ; up over indented lines
				(let*
					((line (buffer-substring-no-properties (point) (line-end-position)))
						(line-indent (and (string-match "\\w" line) (fg-string-match "^[ \t]*" line))))
					(when ; push any non-empty line with smaller indent
						(and line-indent (< (length line-indent) (length indent)))
						(setq indent line-indent) (push line comment)))))
		(fg-string-replace-pairs (mapconcat 'identity comment "\n")
			'(("\\(\\`[[:space:]\n]+\\|[[:space:]\n]+\\'\\)" "") ("\n" "\\\\n") ("\t" "  ")
				("\\(^\\|\\s-\\)fhd\\.[a-zA-Z0-9+/=]+\\.[a-zA-Z0-9+/=]+" "\\1fhd.xxx.yyy")) t)))

(defun fhd-proc-cmd (pw-pos dec)
	"Returns command-line of `fhd-bin' + `fhd-args' to run with its stdin/stdout semantics.
Checks operation type from DEC and `fhd-dev' or `fhd-dev-bak' paths,
to run `fhd-bak' as a wrapper or substitute command when appropriate.
Stdin gets \"salt b64-token\" input, raw opposite for token read from stdout.
PW-POS is used with `fhd-comment-from-path' for context to stored `fhd-bak' secret.
Can print error message and return nil to prevent running any process."
	(if fhd-args (cons fhd-bin fhd-args) ; simple run with fixed arguments
		(if (and fhd-dev (file-exists-p fhd-dev)) ; always use fhd-dev, if present
			(if (or dec (not fhd-bak)) (list fhd-bin fhd-dev) ; enc uses fhd-bak, if available
				(list fhd-bak "wrap" "-c" (fhd-comment-from-path pw-pos) "--" fhd-bin fhd-dev))
			(if (and dec fhd-bak fhd-dev-bak (file-exists-p fhd-dev-bak))
				(list fhd-bak "wrap" "-r") ; dec-retrieval using fhd-bak + fhd-dev-bak
				(not (message "FHD-ERR: no tool/dev for %scryption" (if dec "de" "en")))))))

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
				(dec (process-get proc 'fhd-dec))
				(replace (process-get proc 'fhd-replace)))
			(if (= code 0)
				(let
					((result (if dec out (format
						"fhd.%s.%s" salt (base64-encode-string out t)))))
					(if replace
						(cl-multiple-value-bind (buff a b) replace
							(with-current-buffer buff (save-excursion
								(delete-region a b) (goto-char a) (insert result))))
						(fhd-share-secret result dec)))
				(message (format
					"FHD-ERR [exit=%d]: %s" code (fg-string-or err "<no-stderr>")))))))

(defun fhd-share-secret (s &optional dec)
	"Share/copy secret S with other apps on the system, issuing any notifications.
DEC should be non-nil if secret is a result of decryption, as opposed to ciphertext.
It can be 'totp to process decrypted base32 into one-time-code for TOTP."
	;; Simple fg-copy-string is not ideal here, as it puts secrets into emacs kill-ring
	;; Emacs doesn't paste from its own gui-select-text, hence using "exclip" here
	;; exclip also allows to time-out the secrets easily, for some additional safety
	;; (exclip binary is from mk-fg/fgtk repo here - https://github.com/mk-fg/fgtk#exclip)
	;; gui-select-text adds prefix char to fool emacs into thinking that it's a diff selection
	(let ((out-type (if dec "secret" "ciphertext"))) (or
		(when (eq dec 'totp) (with-temp-buffer
			(insert s)
			(let*
				((code (call-process-region (point-min) (point-max) "oathtool" t t nil "-b" "--totp" "-"))
					(out (fg-string-strip-whitespace (buffer-string))))
				(when (and (= code 0) (not (string-match "^[0-9]+$" out))) ; otp + junk output
					(setq code -1 out "<non-numeric output with success-exit>"))
				(if (/= code 0)
					(message "FHD-ERR [oathtool-exit=%d]: %s" code out)
					(setq s out out-type "totp-code") nil))))
		(progn
			(let (select-enable-clipboard (select-enable-primary t)) (gui-select-text (concat "#" s)))
			(call-process "exclip" nil 0 nil "-xpb" "120") ; -p removes first byte added above
			(fg-notify (format "fhd: %s copied to clipboard" out-type))
			(message "FHD: %s copied to clipboard" out-type)))))
