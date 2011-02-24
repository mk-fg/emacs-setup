;; TODO: scavenge http://zhangw.com/emacs/dotmewel.html

;;;; These are from mew manual
;; Optional setup (Read Mail menu for Emacs 21):
(when (boundp 'read-mail-command)
	(setq read-mail-command 'mew))
;; Optional setup (e.g. C-xm for sending a message):
(autoload 'mew-user-agent-compose "mew" nil t)
(when (boundp 'mail-user-agent)
	(setq mail-user-agent 'mew-user-agent))
(when (fboundp 'define-mail-user-agent)
	(define-mail-user-agent
		'mew-user-agent
		'mew-user-agent-compose
		'mew-draft-send-message
		'mew-draft-kill
		'mew-send-hook))

(setq-default
	mew-name "Mike Kazantsev"
	mew-user "mike_kazantsev"
	mew-mail-domain "fraggod.net"

	mew-proto "%"
	mew-mailbox-type 'imap

	;; mew-imap-ssl t
	mew-imap-delete nil
	mew-imap-header-only t
	mew-imap-auth-list '("LOGIN")
	mew-imap-server "fraggod.net"
	mew-imap-inbox-folder "%INBOX"
	mew-imap-queue-folder "%Queue"
	mew-imap-trash-folder "%Trash"
	mew-imap-friend-folder "%from"

	;; mew-smtp-ssl t
	mew-smtp-server "mail.fraggod.net"
	mew-smtp-auth-list '("LOGIN")

	mew-rc-file "~/.mewrc"
	mew-mail-path (concat fg-path "/tmp/mew")
	mew-conf-path (concat fg-path "/tmp/mew")
	mew-use-async-write t

	mew-use-biff t
	mew-use-biff-bell t
	mew-use-cached-passwd t

	;; mew-demo nil

	mew-use-unread-mark t)
