(require 'newsticker)

(setq-default
	newsticker-url-list
		'(("OSS Broadcasts" "http://fraggod.net/feeds/oss_broadcasts/feed/atom/" nil 1800)
			("Blogs Mesh" "http://fraggod.net/feeds/blogs_mesh/feed/atom/" nil 3600)
			("Music" "http://fraggod.net/feeds/music/" nil 7200)
			("SCM MetaTracker" "http://fraggod.net/feeds/scm_meta_trak/" nil 3600)
			("SCM Tags" "http://fraggod.net/feeds/scm_tags/" nil 3600))
	newsticker-url-list-defaults '()

	newsticker-automatically-mark-items-as-old nil
	newsticker-automatically-mark-visited-items-as-old t
	newsticker-obsolete-item-max-age (* 30 (* 24 3600))

	newsticker-html-render 'w3m-region
	newsticker-date-format "(%H:%M, %A %d.%m)"

	newsticker-dir (concat fg-path "/tmp/newsticker")
	newsticker-cache-filename (concat newsticker-dir "/cache"))

(make-directory newsticker-dir t)

	;; TODO: filters here?
	;; newsticker-auto-mark-filter-list
	;; 	'((slashdot ('old 'title "^Forget me!$")
	;; 		('immortal 'title "Read me")
	;; 		('immortal 'all "important")))

	;; TODO: notifications here
	;; newsticker-new-item-functions
	;; See `newsticker-download-images', and
	;; `newsticker-download-enclosures' for sample functions.

	;; TODO: bind a key to toggle this
	;; newsticker-hide-old-items-in-newsticker-buffer t

	;; TODO: bind to a key
	;; newsticker-show-news
