(require 'newsticker)

(autoload 'w3m-region "w3m"
	"Render region in current buffer and replace with result." t)
(autoload 'w3m-toggle-inline-image "w3m"
	"Toggle the visibility of an image under point." t)

(setq-default
	newsticker-url-list
		'(("Music" "http://fraggod.net/feeds/music/" nil 7200)
			("SCM MetaTracker"
				"http://fraggod.net/feeds/scm_meta_trak/syndication/atom/" nil 3600)
			("SCM Tags" "http://fraggod.net/feeds/scm_tags/" nil 3600))
	newsticker-url-list-defaults '()

	newsticker-automatically-mark-items-as-old nil
	newsticker-automatically-mark-visited-items-as-old t
	newsticker-obsolete-item-max-age (* 30 (* 24 3600))

	newsticker-ticker-interval 0.3
	newsticker-scroll-smoothly t
	newsticker-hide-immortal-items-in-echo-area t
	newsticker-hide-obsolete-items-in-echo-area t

	newsticker-html-renderer 'w3m-region
	newsticker-date-format "(%H:%M, %A %d.%m)"

	newsticker-treeview-own-frame nil
	newsticker-treeview-listwindow-height 30
	newsticker-treeview-treewindow-width 40

	newsticker-dir (concat fg-path "/tmp/newsticker"))

	;;;; These are obsoleted by newsticker-dir and should not be used
	;; newsticker-cache-filename (concat newsticker-dir "/cache")
	;; newsticker-groups-filename (concat newsticker-dir "/groups")

(make-directory newsticker-dir t)


(defun fg-feeds ()
	(interactive)
	(if (newsticker-running-p)
		(if (window-live-p (newsticker--treeview-list-window))
			(newsticker-treeview-quit) (newsticker-show-news))
		(newsticker-start)
		(newsticker-start-ticker)))


;; Doesn't work: every item seem to be considered "new",
;;  yielding a shitload of these notifications on every single update
;; (defun fg-newsticker-notify (feed item)
;;   (fg-notify (format "Feeds: %s" feed) (newsticker--title item) :pixmap "feeds"))
;; (add-hook 'newsticker-new-item-functions 'fg-newsticker-notify)


;; No idea why, but it looks like newsticker--treeview-windows contains
;;  some dead window in place of proper treeview-list, temporary workaround
(defadvice newsticker--treeview-list-window
	(around fg-newsticker--treeview-list-window activate)
	ad-do-it
	(unless (window-live-p ad-return-value)
		(dolist (win (get-buffer-window-list (newsticker--treeview-list-buffer)))
			(when (window-live-p win) (setq ad-return-value win)))))
;; newsticker--selection-overlay is also "in no buffer"-broken because of that,
;;  so selection position is always unset in treeview-list, and set-window-point is never called
;; This hack just always sets it to the beginning of the buffer
(defadvice newsticker--treeview-list-feed-items
	(after fg-newsticker--treeview-list-feed-items activate)
	(set-window-point (newsticker--treeview-list-window) 0))


;; TODO: bind/find a key to toggle this
;; newsticker-hide-old-items-in-newsticker-buffer t
;; TODO: bind/find a key
;;  ...and maybe another one for plainview?
;; newsticker-show-news
