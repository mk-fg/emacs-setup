;; Basic setup
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-playlist-mode)

(setq-default
	emms-directory (concat fg-path "/tmp/emms")
	emms-source-file-default-directory "/mnt/db/mediaCore/sound_music"

	emms-source-playlist-default-format 'pls
	emms-playlist-mode-center-when-go t

	emms-show-format "NP: %s")



;;;; Player

;; (require 'emms-player-vlc)
;; (setq-default
;; 	emms-player-list
;; 		'(emms-player-vlc-playlist emms-player-vlc)
;; 	emms-player-vlc-parameters '("--intf=oldrc")
;; 	emms-player-vlc-playlist-parameters '("--intf=oldrc"))

(require 'emms-player-mplayer)
(setq-default
	emms-player-list
		'(emms-player-mplayer-playlist emms-player-mplayer)
	emms-player-mplayer-parameters
		(append emms-player-mplayer-parameters
			'("-noconfig" "user" "-vo" "null")))



;;;; Scrobbling

;; Used to have emms-lastfm-scrobbler here,
;;  but it was sync, so ditched it in favor of a dbus helper,
;;  statically specifying session key when emms-lastfm-scrobbler
;;  stopped working at all.

(when
	(and
		(require 'url-vars nil t) ;; otherwise it's "url-request-method is let-bound"
		(require 'emms-lastfm-client nil t)
		(require 'emms-lastfm-scrobbler nil t)
		(require 'emms-playing-time nil t))
	(setq-default
		emms-lastfm-client-username "FraGGod"
		emms-lastfm-client-api-key fg-auth-emms-lastfm-client-api-key
		emms-lastfm-client-api-secret-key fg-auth-emms-lastfm-client-api-secret-key
		emms-lastfm-client-api-session-key fg-auth-emms-lastfm-client-api-session-key)
	(emms-playing-time 1)

	;; Sabotage emms-lastfm network calls, because they block way too often
	(setq emms-lastfm-client-api-base-url nil)
	(defun emms-lastfm-client-construct-method-call (method arguments)
		(let ((debug-on-error t))
			(error "Call to banned emms-lastfm-client-construct-method-call, check it")))

	(add-hook 'emms-player-started-hook
		'fg-emms-lastfm-scrobbler-start-hook t)
	(add-hook 'emms-player-finished-hook
		'fg-emms-lastfm-scrobbler-stop-hook))


(defvar fg-emms-scrobble-tracks t
	"Controls whether tracks will be scrobbled to last.fm")
(defvar fg-emms-scrobble-via-dbus t
	"Use async dbus calls to scrobble tracks.
DBus component: https://github.com/mk-fg/dbus-lastfm-scrobbler")

(defun fg-emms-get-scrobblable-track ()
	(when fg-emms-scrobble-tracks
		(let ((current-track (emms-playlist-current-selected-track)))
			(and current-track
				(if (emms-track-get current-track 'info-title)
					current-track
					(message
						"Unable to scrobble track - no metadata: %s"
						(emms-track-get current-track 'name))
					nil)))))

(defun fg-emms-lastfm-scrobbler-dbus-call
	(artist album track &optional duration ts timeout)
	(let*
		((dbus-call-base
			(apply-partially 'dbus-call-method
				:session
				"net.fraggod.DBusLastFM"
				"/net/fraggod/DBusLastFM"
				"net.fraggod.DBusLastFM"))
			(dbus-call
				(lambda ()
					(apply dbus-call-base
						(if ts "Scrobble" "ReportNowPlaying")
						:timeout (or timeout 2000) ;; it should be async for a reason!
						:string artist
						:string (or album "")
						:string track
						:uint32 (or duration 0)
						(when ts (list :double ts))))))
		(condition-case err
			(funcall dbus-call)
			(dbus-error
				(when (and err (string= (cadr err) "NO-AUTH"))
					(let
						((auth
							(list
								emms-lastfm-client-api-key
								emms-lastfm-client-api-secret-key
								emms-lastfm-client-api-session-key)))
						(unless
							(dolist (k auth val)
								(if k (setq val t) (setq val nil) (return-from nil)))
							(error (funcall 'format "Missing auth keys: %s" auth)))
						(apply dbus-call-base "Auth" auth))
					(funcall dbus-call))))))

;; Default hook requires info-playing-time to be known, and it's hard to get w/o hangs
;; Also it requires *enabling* emms-playing-time, which is kinda undocumented
;; And it spits errors on tracks missing info - very annoying (replaced by non-flashy msgs)
(defun fg-emms-lastfm-scrobbler-stop-hook ()
	"Submit the track to last.fm if it has been played for 60s."
	(let ((current-track (fg-emms-get-scrobblable-track)))
		(if
			(not (and
				current-track
				(emms-lastfm-scrobbler-allowed-track-type current-track)
				(> emms-playing-time 60)))
			(message "Not scrobbling track - invalid type/length/metadata")
			;; info-playing-time is mandatory for last.fm submissions
			(unless (emms-track-get current-track 'info-playing-time)
				(emms-track-set current-track 'info-playing-time emms-playing-time))
			(if fg-emms-scrobble-via-dbus
				(apply
					'fg-emms-lastfm-scrobbler-dbus-call
					(append
						(mapcar
							(lambda (bit) (emms-track-get (fg-emms-get-scrobblable-track) bit))
							'(info-artist info-album info-title info-playing-time))
						(list (string-to-number
							emms-lastfm-scrobbler-track-play-start-timestamp))))
				(emms-lastfm-scrobbler-make-async-submission-call current-track nil)))))

;; Spits annoying errors on tracks missing info as well
(defun fg-emms-lastfm-scrobbler-start-hook ()
	"Update the now playing info displayed on the user's last.fm page.  This
		doesn't affect the user's profile, so it can be done even for tracks that
		should not be submitted."
	;; wait 5 seconds for the stop hook to submit the last track
	(sit-for 5)
	(let ((current-track (fg-emms-get-scrobblable-track)))
		(when current-track
			(setq
				emms-lastfm-scrobbler-track-play-start-timestamp
				(emms-lastfm-scrobbler-timestamp))
			(when (emms-lastfm-scrobbler-allowed-track-type current-track)
				(if fg-emms-scrobble-via-dbus
					(apply
						'fg-emms-lastfm-scrobbler-dbus-call
						(mapcar
							(lambda (bit) (emms-track-get current-track bit))
							'(info-artist info-album info-title info-playing-time)))
					(emms-lastfm-scrobbler-make-async-nowplaying-call current-track))))))



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
split numeric prefix, strip file extension."
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
		/some/path/Artist/Some_Track_Name.ogg
		/some/path/Artist/01_Some_Track_Name.ogg (no year in album part - no album info)"
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
					(if (<= (length name) 2)
						(setq info-artist (car (last name 2)))
						(multiple-value-setq
							(info-album info-year)
							(fg-emms-file-track-wash-name (car (last name 2))))
						(if info-year
							(setq info-artist (car (last name 3)))
							(setq info-artist info-album info-album nil)))
					(setq info-artist (replace-regexp-in-string "_+" " " info-artist))
					;; Actually set the values
					(dolist
						(sym '(info-artist info-album info-title info-tracknumber) track)
						(emms-track-set track sym (eval sym))))))))

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


(defvar emms-playlist-directory "~/media/playlists"
	"Default path to save/load emms playlists.")

(defadvice emms-playlist-save (around fg-emms-playlist-save (format file) activate)
	"Store playlists in `emms-playlist-directory' by default."
	(interactive (list (emms-source-playlist-read-format)
		(read-file-name "Store as: " emms-playlist-directory emms-playlist-directory nil)))
	ad-do-it)



;;;; Track change notification
(defun fg-emms-notify ()
	(interactive)
	(fg-notify
		"emms: now playing"
		(concat
			(emms-track-description (emms-playlist-current-selected-track))
			(if (= 0 (length emms-playing-time-string))
				"" (format " (%s)" emms-playing-time-string)))
		:pixmap "emms"
		:urgency 'critical))

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
		(when
			(or (null emms-playlist-buffer)
				(not (buffer-live-p emms-playlist-buffer)))
			(call-interactively 'emms-play-directory-tree))
		(emms-playlist-mode-go)))



;; Create paths
(make-directory emms-directory t)
