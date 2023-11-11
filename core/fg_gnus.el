;;;; This is a work in progress
;;;; I've decided to try out Mew first, then fall back to gnus, if it won't be good enough...
;;;; Ok, back to gnus. Mew seem to be quite twisted and news-oriented stuff should be better

;;;; Few rumors to note on emacs mailers:
;;  * Seem to insist on it's own paths/cache system, which can't be good.
;;    Wanderlust in contrast seem to keep everything in standard maildirs.
;;    -- That's a total myth, there are backends for every single format in existance
;;  * Mew didn't have good enough imap support is the past (like 5 years ago).
;;  * WL imap seem to lack something, hence this: http://repo.or.cz/w/more-wl.git
;;  * "VM really needed you to have all your email on the mail client machine.",
;;    "VM was effectively treating IMAP as if it was a dumb server." (a couple of years ago)
;;  * "wanderlust depends on some strange mime packages."
;;  * gnus seem to be heavily news-oriented.
;;    -- confirmed, question is whether it's a bad thing


;; Needed for stuff like gnus-version-number
(require 'gnus-start)

(setq ;; setq-default doesn't seem to work for gnus-subscribe stuff
	user-mail-address "mk.fraggod@gmail.com"
	user-full-name "Mike Kazantsev"

	;; TODO: Is this used at all? Should it be dropped in favor of nnimap?
	;; mail-source-primary-source '(imap
	;; 	:server "fraggod.net"
	;; 	:user "fraggod"
	;; 	:password "mypassword"
	;; 	:stream 'starttls
	;; 	:fetchflag "\\Seen")
	;; mail-sources (list mail-source-primary-source)

	;; TODO: for several servers, see "*info* (gnus) IMAP"
	gnus-nntp-server nil

	gnus-select-method
		`(nnimap "fg_core"
			(nnimap-address "mail.fraggod.net")
			(nnimap-authenticator login)
			(nnimap-stream starttls)
			(nnimap-id ("name" "Gnus"
				"version" ,gnus-version-number
				"os" ,system-configuration
				"os-version" ,operating-system-release
				"vendor" "GNU"
				"support-url" ,user-mail-address))
			(remove-prefix "INBOX.")
			(nnimap-logout-timeout 10))

	gnus-auto-subscribed-groups
		(concat "^nnimap\\|" gnus-auto-subscribed-groups)
	gnus-subscribe-newsgroup-method 'gnus-subscribe-zombies
	gnus-subscribe-options-newsgroup-method 'gnus-subscribe-zombies
	nnimap-list-pattern "*"
	nnimap-expunge-on-close 'always
	;; nnimap-news-groups t ;; TODO: fallback to it?

	;; TODO: are these needed for imap paths translation?
	;; nnimap-split-rule
	;; 	'(("INBOX.gnus-imap"   "From:.*gnus-imap")
	;; 		("INBOX.junk"        "Subject:.*buy"))
	nnimap-split-download-body nil

	send-mail-function 'smtpmail-send-it
	message-send-mail-function 'smtpmail-send-it
	smtpmail-default-smtp-server "fraggod.net"
	smtpmail-smtp-server smtpmail-default-smtp-server

	smtpmail-auth-credentials
		'(("fraggod.net" 25 "" "mike_kazantsev" ,fg-auth-gnus-core))
	smtpmail-starttls-credentials '(("fraggod.net" 25 nil nil))

	mail-source-idle-time-delay 5
	mail-source-report-new-mail 10

	gnus-home-directory (concat fg-path-spool "gnus")
	gnus-directory (concat fg-path-spool "gnus")
	gnus-default-directory nil
	gnus-startup-file (concat gnus-directory "/newsrc")
	gnus-init-file (concat gnus-directory "/rc")
	gnus-kill-files-directory gnus-directory
	gnus-article-save-directory gnus-directory
	gnus-cache-directory (concat gnus-directory "/cache")
	mail-source-directory (concat gnus-directory "/mail")
	mail-source-crash-box (concat mail-source-directory "/spool")
	nnmail-message-id-cache-file (concat mail-source-directory "/msgid-cache")

	gnus-backup-startup-file t
	gnus-use-dribble-file t
	mail-source-default-file-modes #o0600
	mail-source-delete-incoming 30
	mail-source-incoming-file-prefix "ingress"
	nnmail-expiry-wait 30

	mm-discouraged-alternatives '("text/html" "text/richtext" "image/.*")

	nnimap-debug t

	gnus-novice-user nil
	gnus-use-full-window nil
	gnus-asynchronous t
	gnus-use-cache t
	gnus-use-article-prefetch 30
	gnus-use-header-prefetch nil)


;; Fetch new stuff at intervals
;; TODO: gnus-demon-add-scanmail looks similar, fallback to it if this
;;  won't work
;; TODO: doesn't work at all if gnus is closed on invocation,
;;  guess I better just trash it and write my own simple looping timer func
(when (require 'gnus-demon nil t)
	(gnus-demon-add-handler 'gnus-group-get-new-news 10 nil)
	(gnus-demon-init))

;; TODO: different posting styles, or just setup one
;; (setq gnus-posting-styles
;;       '(((header "to" "REDACTED")
;; 	 (from (concat user-full-name " REDACTED"))
;;          (setq message-cite-reply-above nil))
;; 	((header "to" "REDACTED")
;; 	 (from (concat user-full-name " REDACTED"))
;;          (setq message-cite-reply-above nil))
;;         ((header "to" "REDACTED")
;;          (from (concat user-full-name " REDACTED"))
;;          (setq message-cite-reply-above 't))
;; 	((header "to" "REDACTED")
;; 	 (from (concat user-full-name " REDACTED"))
;;          (setq message-cite-reply-above nil))))


;; TODO: setup biff
;; (defvar foundnewmbox "")
;; (defun fmbiff ()
;;   (interactive)
;;   (save-excursion
;;     (set-buffer "*Group*")
;;     (beginning-of-buffer)
;;     (defvar foundanymbox nil)
;;     (cond ((re-search-forward "INBOX.in" nil t)
;;            (setq foundanymbox t))
;;           (t (setq foundanymbox nil)))
;;     (set-buffer "*Group*")
;;     (beginning-of-buffer)
;;     (cond ((re-search-forward "0: INBOX.in" nil t)
;;            (setq foundnewmbox ""))
;;           (t (if foundanymbox (setq foundnewmbox "[M]")
;;                (setq foundnewmbox ""))))))

;; (unless (member 'foundnewmbox global-mode-string)
;;    (setq global-mode-string (append global-mode-string
;;                                     (list 'foundnewmbox))))

;; (add-hook 'gnus-after-getting-new-news-hook 'fmbiff)
;; (add-hook 'gnus-group-mode-hook 'fmbiff)


;; Separate netrc file replacement
;; TODO: extend to support different logins on the same server
(defadvice netrc-parse (around fg-gnus-auth-netrc)
	(setq ad-return-value
		`((("machine" . "mail.fraggod.net")
			("login" . "fraggod")
			("password" . ,fg-auth-gnus-core)))))

(defadvice nnimap-open-connection
	(before fg-gnus-auth-conn-pre activate)
	(ad-activate 'netrc-parse))
(defadvice nnimap-open-connection
	(after fg-gnus-auth-conn-post activate)
	(ad-deactivate 'netrc-parse))

;; This makes gnutls handshake much less verbose,
;;;  so it won't bump message area with multiline certificate info
(defadvice starttls-negotiate-gnutls
	(after fg-silent-starttls-negotiate-gnutls activate)
	(when ad-return-value
		(setq ad-return-value "TLS handshake success")))

;; TODO: setup formatting for group-mode and summary-mode window lines
;;  so I won't have to use fixed-pitch there. This basically means elimination of
;;  fixed-witdth fields.


;; Create paths
(dolist
	(path (list gnus-home-directory gnus-directory
		gnus-default-directory mail-source-directory gnus-cache-directory))
	(unless (or (not path) (file-directory-p path))
		(make-directory path t)
		(set-file-modes path #o700)))
