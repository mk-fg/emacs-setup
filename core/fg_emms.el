;; Basic setup
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-playlist-mode)
(require 'emms-playing-time)

(setq-default
	emms-directory (concat fg-path "/tmp/emms")
	emms-source-file-default-directory "/mnt/db/mediaCore/sound_music/"

	emms-source-playlist-default-format 'm3u
	emms-playlist-mode-center-when-go t

	emms-show-format "NP: %s")

(emms-playing-time 1)
(add-hook 'fg-emacs-exit-hook 'emms-stop)


;;;; Player

;;; Use VLC
;; (require 'emms-player-vlc)
;; (setq-default
;; 	emms-player-list
;; 		'(emms-player-vlc-playlist emms-player-vlc)
;; 	emms-player-vlc-parameters '("--intf=oldrc")
;; 	emms-player-vlc-playlist-parameters '("--intf=oldrc"))

(require 'emms-player-mpv) ;; in extz
(add-to-list 'emms-player-list 'emms-player-mpv)
(setq-default emms-player-list '(emms-player-mpv))

(defun fg-emms-player-status-string ()
	(if emms-player-playing-p
		(if emms-player-paused-p "paused" "playing") "stopped"))


;;;; Playlist controls

(defun fg-emms-playlist-mode-kill ()
	"Smart-kill: kills track at point if no region is active,
otherwise acts on the region, using basic emms functions."
	(interactive)
	(if (use-region-p)
		(emms-playlist-mode-kill)
		(emms-playlist-mode-kill-entire-track)))

(defun fg-emms-playlist-mode-copy ()
	"Yank selected or pointed-to track(s) into kill-buffer.
Uses basic `fg-copy' func internally, not emms-playlist stuff."
	(interactive)
	(fg-copy t))

(defun fg-emms-playlist-mode-del ()
	"Like `fg-emms-playlist-mode-kill', but w/o ring-buffer."
	(interactive)
	(emms-with-inhibit-read-only-t
		(fg-taint :call 'delete-region :whole-lines-only t)))

(defun fg-emms-playlist-mode-clone (arg)
	(interactive "p")
	(emms-with-inhibit-read-only-t (fg-clone arg)))


(defun fg-emms-call-something-on-glob (func pattern)
	"Expand glob pattern (unless exact-match path exists),
calling FUNC for every path result."
	(dolist
		(path
			(if (file-exists-p pattern)
				(list pattern)
				(file-expand-wildcards pattern)))
		(funcall func path)))

(defun fg-emms-add-directory-tree-glob (pattern)
	"Add all directories matching provided glob pattern."
	(interactive (list
		(read-directory-name "Play directory tree (glob): "
			emms-source-file-default-directory
			emms-source-file-default-directory)))
	(fg-emms-call-something-on-glob 'emms-add-directory-tree pattern))

(defun fg-emms-add-file-glob (pattern)
	"Add all files matching provided glob pattern."
	(interactive (list
		(read-file-name "Play file (glob): "
			emms-source-file-default-directory
			emms-source-file-default-directory)))
	(fg-emms-call-something-on-glob 'emms-add-file pattern))


;;;; Track info / description

(defun* fg-emms-file-track-wash-name (title &key strip-ext)
	"Process underscore-encoded spaces in name,
split numeric prefix (if delimited in a standard fashion), strip file extension."
	(let
		((trackno
			(dolist (sep '("_-_" "_" nil))
				(if sep
					(let ((title-parts (split-string title sep t)))
						(when (string-match-p "^[0-9]+$" (car title-parts))
							(setq title (mapconcat 'identity (cdr title-parts) sep))
							(return (setq trackno (car title-parts)))))
					nil))))
		(values
			(fg-string-replace-pairs
				(if strip-ext (replace-regexp-in-string "\\.[0-9a-zA-Z]+$" "" title) title)
				'(("_-_" " - ") ("-_+" ": ") ("_+" " ")))
			 trackno)))

(defun* fg-emms-track-info-fs (track)
	"Set TRACK info from fg_core standard pathname.
Examples:
	/some/path/Artist/2000_Album_X/07_-_Some_Track_Name.mp3
	/some/path/Artist/2001_Album-_Aftercolon/01_Some_Track_Name.ogg
	/some/path/Artist/2002_Single_File_Album_or_Standalone_Track.flac
	/some/path/Artist/2015_-_Album_with_year/cd-03/Track_Name.mp3
	/some/path/Artist/Some_Track_Name.ogg
	/some/path/Artist/01_Some_Track_Name.ogg"
	(when track
		(let ((name (emms-track-name track)))
			(setq name
				(split-string
					(if (not (string-prefix-p emms-source-file-default-directory name))
						name
						(substring name
							(length emms-source-file-default-directory)))
					"/" t))
			(unless (< (length name) 2)
				(multiple-value-bind
					(info-title info-tracknumber info-album info-year info-artist)
					(fg-emms-file-track-wash-name (car (last name)) :strip-ext t)
					(when info-title
						(setq name (--remove (string-match ; drop "cd 01" or similar components
							"^[^[:alnum:]]*\\([Cc][Dd]\\)?[^[:alnum:]]*[0-9]\\{1,2\\}[^[:alnum:]]*$" it) name))
						(if (<= (length name) 2)
							(setq info-artist (car (last name 2)))
							(multiple-value-setq
								(info-album info-year)
								(fg-emms-file-track-wash-name (car (last name 2))))
							(when
								(and (not info-year)
									(string-match
										(concat
											"^\\s-*\\(\\(19\\|20\\)[0-9]\\{2\\}\\)"
											"[^[:alnum:]].*?\\([[:alnum:]].*\\)$")
										info-album)
								(setq
									info-year (match-string 1 info-album)
									info-album (match-string 3 info-album))))
							(if info-year
								(setq info-artist (car (last name 3)))
								(setq info-artist info-album info-album nil))
							(when
								(and (not info-tracknumber)
									(string-match
										"^\\s-*\\([0-9]\\{2\\}\\)[^[:alnum:]].*?\\([[:alnum:]].*\\)$"
										info-title))
								(setq
									info-tracknumber (match-string 1 info-title)
									info-title (match-string 2 info-title))))
						(setq info-artist (replace-regexp-in-string "_+" " " info-artist))
						;; Actually set the values
						(dolist
							(sym '(info-artist info-album info-title info-tracknumber) track)
							(emms-track-set track sym (eval sym)))))))))

(defun fg-emms-info-track-description (track &optional no-fallback)
	"Return a description of TRACK."
	(let
		((desc (mapconcat
			(apply-partially 'emms-track-get track)
			'(info-artist info-album info-title) " :: ")))
		(if (string= desc " ::  :: ")
			(if no-fallback
				(emms-track-name track)
				;; TODO: check why emms doesn't fetch info immediately, disable it
				(fg-emms-track-info-fs track)
				(fg-emms-info-track-description track t))
			desc)))

(setq-default
	emms-track-description-function 'fg-emms-info-track-description
	emms-track-initialize-functions '(emms-info-initialize-track)
	emms-info-auto-update nil
	emms-info-functions '(fg-emms-track-info-fs))

;; Here's proper info getter. Pity it's so crippled...
;; TODO: fix this, not all data gets assigned, check while loop
;; TODO: perfomance impact here is huge, prehaps "call-process" is not async at all?
;; TODO: synchronous crap, I should either make it async or drop it altogether
;; (when (require 'emms-info-libtag nil t)
;; 	(setq-default
;; 		;; TODO: VERY slow over NFS, gotta do something about it first
;; 		emms-info-functions '(emms-info-libtag)))



;;;; Cache / history / playlists
;; Note: fg-emms-info-track-description should be defined before these
;;  are evaluated, same probably goes for the rest of emms-*-function

(defvar fg-emms-history-autosave-timer nil
	"Repetitive timer calling `fg-emms-history-autosave'.")

(when (require 'emms-cache nil t) (emms-cache-enable))
(when (require 'emms-history nil t)
	(emms-history-load)

	(defvar fg-emms-history-autosave-hash
		(and emms-playlist-buffer (sha1 emms-playlist-buffer))
		"Hash of saved contents of emacs playlists, used to check whether save is needed.")

	(defun fg-emms-history-autosave ()
		(let ((hash (and emms-playlist-buffer (sha1 emms-playlist-buffer))))
			(unless (string-equal hash fg-emms-history-autosave-hash)
				(emms-history-save)
				(setq fg-emms-history-autosave-hash hash))))

	(setq fg-emms-history-autosave-timer
		(run-at-time t 1200 'fg-emms-history-autosave))) ;; 20 min


(defvar fg-emms-playlist-directory "~/media/playlists"
	"Default path to save/load emms playlists.")

(defadvice emms-playlist-save (around fg-emms-playlist-save (format file) activate)
	"Store playlists in `fg-emms-playlist-directory' by default."
	(interactive (list (emms-source-playlist-read-format)
		(read-file-name "Store as: "
			fg-emms-playlist-directory
			fg-emms-playlist-directory)))
	ad-do-it)

(defadvice emms-add-playlist (around fg-emms-add-playlist (file) activate)
	"Load playlists from `fg-emms-playlist-directory' by default."
	(interactive (list
		(read-file-name "Playlist file: "
			fg-emms-playlist-directory
			fg-emms-playlist-directory)))
	ad-do-it)



;;;; Track change notification
(defun fg-emms-notify ()
	(interactive)
	(let ((track (emms-playlist-current-selected-track)))
		(when track
			(fg-notify
				"emms: now playing"
				(concat
					(emms-track-description track)
					(if (= 0 (length emms-playing-time-string))
						"" (format " (%s)" emms-playing-time-string)))
				:pixmap "emms"
				:urgency 'critical))))

(add-hook 'emms-player-started-hook 'fg-emms-notify)
(add-hook 'emms-player-started-hook 'emms-show)



;;;; EMMS buffers

;; TODO: emms-playlist-mode seem to be far from perfect.
;;  One possible way to enhance it is to scavenge ideas from other emacs+emms configurations.

;; emms-playlist-mode-switch-buffer lacks:
;;  a) emms-play-directory-tree call
;;  b) emms-playlist-mode-go
(defun emms ()
	"Switch to/from the current `emms-playlist' buffer or
invoke `emms-play-directory-tree'."
	(interactive)
	(if emms-playlist-buffer-p
		(emms-playlist-mode-bury-buffer)
		(let
			((playlist-restore-pos
				(if
					(or (null emms-playlist-buffer)
						(not (buffer-live-p emms-playlist-buffer)))
					(prog1 nil (call-interactively 'emms-play-directory-tree))
					(with-current-emms-playlist
						(-when-let (m emms-playlist-selected-marker) (marker-position m))))))
			(emms-playlist-mode-go)
			(when playlist-restore-pos
				(with-current-emms-playlist
					(emms-playlist-select playlist-restore-pos)
					(goto-char playlist-restore-pos))))))


;; Create paths
(make-directory emms-directory t)
