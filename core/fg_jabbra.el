(require 'password-cache) ; for jabbra reconnect features

(require 'jabber)
(setq-default
	jabber-account-list fg-auth-jabber-account-list
	jabber-activity-count-in-title t
	jabber-auto-reconnect t
	jabber-autoaway-status "AFK. Later..."
	jabber-autoaway-timeout 15
	jabber-avatar-cache-directory (concat fg-path "/tmp/jabbra.avatars")
	jabber-backlog-days nil
	jabber-backlog-number 40
	jabber-browse-buffer-format "*-j-browse:-%n-*"
	jabber-chat-buffer-format "*-j-chat-%n-*"
	jabber-chat-delayed-time-format "%m-%d %H:%M:%S"
	jabber-chat-fill-long-lines nil
	jabber-chat-time-format "%H:%M:%S"
	jabber-default-show ""
	jabber-display-menu nil
	jabber-global-history-filename (concat fg-path "/tmp/jabbra.log")
	jabber-groupchat-buffer-format "*-j-groupchat-%n-*"
	jabber-history-dir (concat fg-path "/tmp/jabbra")
	jabber-history-enable-rotation nil
	jabber-history-enabled t
	jabber-muc-completion-delimiter ", "
	jabber-muc-private-buffer-format "*-j-muc-priv-%g-%n-*"
	jabber-print-rare-time nil
	jabber-roster-buffer "*-j-roster-*"
	jabber-roster-line-format " %c %-35n %u %-8s  %S"
	jabber-roster-show-bindings nil
	jabber-roster-show-empty-group t
	jabber-roster-show-title nil
	jabber-use-global-history nil)

(defun fg-jabber-notify (from buffer text proposed-alert)
	(fg-notify from text :pixmap "jabbra" :strip t))
(add-hook 'jabber-alert-message-hooks 'fg-jabber-notify)

(defun fg-jabber-activity-reset ()
	(setq jabber-activity-jids nil)
	(jabber-activity-mode-line-update))


;; Create paths
(dolist
	(path (list jabber-avatar-cache-directory jabber-history-dir))
	(unless (or (not path) (file-directory-p path))
		(make-directory path t)
		(set-file-modes path #o700)))
