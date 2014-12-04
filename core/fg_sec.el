;; use ";; -*- epa-file-encrypt-to: ("mk.fraggod@gmail.com") -*-" in file headers

(require 'epa-file)
(setq-default epa-file-encrypt-to "mk.fraggod@gmail.com")

(defvar epa-select-keys-inhibit t
	"Do not use interactive prompt for recipient keys,
using `epa-file-encrypt-to' value instead.")


;; These will replace epa-select-keys function, leaving the old definition as
;;  epa-select-keys-interactive, so it can still be used, if necessary
(fset 'epa-select-keys-interactive (symbol-function 'epa-select-keys))

(defun epa-select-keys (context prompt &optional names secret static)
	"Return all key(s) referenced by name(s) in
`epa-file-encrypt-to' instead or a popup selection prompt
if `epa-select-keys-inhibit' is set to nil or STATIC is non-nil.

Only auto-picks keys with ultimate validity and email
regexp-match against NAMES to make sure it's the right key.

See `epa-select-keys-interactive' for the description of other parameters."
	(if (or static epa-select-keys-inhibit)
		(or
			(block :loop
				(dolist
					(key (epg-list-keys context epa-file-encrypt-to secret))
					(dolist (uid (epg-key-user-id-list key))
						(let*
							;; Match names vs epg-user-id-string
							((uid-string (epg-user-id-string uid))
								(uid-names ; nil if no matches
									(fg-keep-when
										(lambda (name)
											(string-match
												(concat "<" (regexp-quote name) ">\\s-*$")
												uid-string))
										names)))
							;; Check epg-user-id-validity
							(when
								(and uid-names
									(eq (epg-user-id-validity uid) 'ultimate))
								(message "Encrypting with gpg key: %s [%s]" uid-string
									(substring (epg-sub-key-id
										(car (epg-key-sub-key-list key))) -8)) ; car here is the primary key
								(return-from :loop (list key)))))))
			(error
				(format "Failed to match trusted gpg key against name(s): %s" names)))
		(epa-select-keys-interactive context prompt names secret)))

;; Patched version from upstream to work with gpg-2.1.0
;;  https://lists.gnu.org/archive/html/emacs-diffs/2014-11/msg00088.html
(when
	(or (> emacs-major-version 24)
		(and (= emacs-major-version 24) (> emacs-minor-version 4)))
(defun epg--list-keys-1 (context name mode)
	(let
		((args
				(append
					(if epg-gpg-home-directory
						(list "--homedir" epg-gpg-home-directory))
					'("--with-colons" "--no-greeting" "--batch"
						"--with-fingerprint" "--with-fingerprint")
					(unless
						(eq (epg-context-protocol context) 'CMS)
						'("--fixed-list-mode"))))
			(list-keys-option
				(if (memq mode '(t secret))
					"--list-secret-keys"
					(if (memq mode '(nil public)) "--list-keys" "--list-sigs")))
			(coding-system-for-read 'binary)
			keys string field index)
		(if name
			(progn
				(unless (listp name) (setq name (list name)))
				(while name
					(setq
						args (append args (list list-keys-option (car name)))
						name (cdr name))))
			(setq args (append args (list list-keys-option))))
			(with-temp-buffer
				(apply #'call-process
					(if (eq (epg-context-protocol context) 'CMS)
						epg-gpgsm-program epg-gpg-program)
					nil (list t nil) nil args)
				(goto-char (point-min))
				(while (re-search-forward "^[a-z][a-z][a-z]:.*" nil t)
					(setq
						keys (cons (make-vector 15 nil) keys)
						string (match-string 0)
						index 0
						field 0)
					(while
						(and (< field (length (car keys)))
							(eq index (string-match "\\([^:]+\\)?:" string index)))
						(setq index (match-end 0))
						(aset (car keys) field (match-string 1 string))
						(setq field (1+ field))))
				(nreverse keys))))



;;;; TRAMP mode

(require 'auth-source)
(require 'tramp)

(setq-default
	tramp-default-method "ssh")

(add-to-list 'tramp-default-user-alist
	'("ssh" ".*\\.\\(mplik\\.ru\\|e1\\)\\'" "mkfg"))
(add-to-list 'tramp-default-proxies-alist
	'(".*.\\(mplik\\.ru\\|e1\\)\\'" "\\`root\\'" "/ssh:mkfg@%h:"))

;; (setq auth-source-debug t)
;; (password-reset)
;; (auth-source-user-or-password "password" "db-six.mplik.ru" "sudo" "root")
