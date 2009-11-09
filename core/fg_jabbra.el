(require 'password-cache) ; for jabbra reconnect features
(setq-default
	starttls-use-gnutls t
	starttls-extra-arguments '("--insecure") ; for gnutls-cli: skip certificate validation, for gtalk w/ CN:gmail.com
	password-cache-expiry nil)

(require 'jabber)
(setq-default
	jabber-account-list
		'(("mike_kazantsev@fraggod.net/Home" (:connection-type . starttls))
			("mk.fraggod@gmail.com/Home" (:connection-type . starttls)))
	jabber-auto-reconnect t
	jabber-autoaway-status "AFK. Later..."
	jabber-autoaway-timeout 15
	jabber-avatar-cache-directory "~/.emacs.d/tmp/jabbra.avatars"
	jabber-backlog-days nil
	jabber-backlog-number 40
	jabber-browse-buffer-format "*-j-browse:-%n-*"
	jabber-chat-buffer-format "*-j-chat-%n-*"
	jabber-chat-delayed-time-format "%m-%d %H:%M:%S"
	jabber-chat-fill-long-lines nil
	jabber-chat-time-format "%H:%M:%S"
	jabber-default-show ""
	jabber-display-menu nil
	jabber-global-history-filename "~/.emacs.d/tmp/jabbra.log"
	jabber-groupchat-buffer-format "*-j-groupchat-%n-*"
	jabber-history-dir "~/.emacs.d/tmp/jabbra"
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
