;; Basic setup
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-player-mplayer)
(require 'emms-playlist-mode)

(setq-default
	emms-player-list
		'(emms-player-mplayer-playlist emms-player-mplayer)
	;; emms-player-mplayer-parameters
	;; 	(append emms-player-mplayer-parameters '("-noconfig" "user"))

	emms-directory (concat fg-path "/tmp/emms")
	emms-source-file-default-directory "/mnt/db/mediaCore/sound_music"

	emms-source-playlist-default-format 'pls
	emms-playlist-mode-center-when-go t

	emms-show-format "NP: %s")



;;;; Scrobbling

;; Auth is kinda complicated there:
;; 1. API key/secret_key from http://www.last.fm/api/authentication
;; 2. emms-lastfm-client-user-authorization, allow in browser
;; 3. emms-lastfm-client-get-session,
;;  will store tmp/emms/emms-lastfm-client-sessionkey
;;
;; "emms-lastfm-scrobbler-nowplaying-data: Track title and artist must be known."
;;  might mean that track metadata wasn't extracted properly, it should be.

(when
	(and
		(require 'emms-lastfm-client nil t)
		(require 'emms-playing-time nil t))
	(setq-default
		emms-lastfm-client-username "FraGGod"
		emms-lastfm-client-api-key fg-auth-emms-lastfm-client-api-key
		emms-lastfm-client-api-secret-key fg-auth-emms-lastfm-client-api-secret-key)
	(emms-playing-time 1)
	(emms-lastfm-scrobbler-enable))

;; Default hook requires info-playing-time to be known, and I don't store it in a filename
;; Also it requires *enabling* emms-playing-time, which is kinda undocumented
(defun emms-lastfm-scrobbler-stop-hook ()
	"Submit the track to last.fm if it has been played for 60s."
	(let ((current-track (emms-playlist-current-selected-track)))
		(when
			(and
				(emms-lastfm-scrobbler-allowed-track-type current-track)
				(> emms-playing-time 60))
			;; info-playing-time is mandatory for last.fm submissions
			(unless (emms-track-get current-track 'info-playing-time)
				(emms-track-set current-track 'info-playing-time emms-playing-time))
			(emms-lastfm-scrobbler-make-async-submission-call current-track nil))))



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



;;;; Track info / description
(defun* fg-emms-file-track-wash-name (title &key strip-ext)
	"Process underscore-encoded spaces in name,
split numeric prefix, strip file extension."
	(let
		((trackno
			(dolist (sep '("_-_" "_"))
				(let ((title-parts (split-string title sep t)))
					(when (string-match-p "^[0-9]+$" (car title-parts))
						(setq title (mapconcat 'identity (cdr title-parts) sep))
						(return (setq trackno (car title-parts))))))))
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
		/some/path/Artist/2002_Single_File_Album_or_Standalone_Track.flac"
	(let ((name (emms-track-name track)))
		(if
			(not (string-prefix-p emms-source-file-default-directory name))
			(car (last (split-string name "/")))
			(setq name (split-string (substring name
				(length emms-source-file-default-directory)) "/" t))
			(multiple-value-bind
				(info-title info-tracknumber info-album info-artist)
				(fg-emms-file-track-wash-name (car (last name)) :strip-ext t)
				(if (> (length name) 2)
					(setq
						info-album
							(car (fg-emms-file-track-wash-name (car (last name 2))))
						info-artist (car (last name 3)))
					(setq info-artist (car (last name 2))))
				(setq info-artist (replace-regexp-in-string "_+" " " info-artist))
				;; Actually set the values
				(dolist
					(sym '(info-artist info-album info-title info-tracknumber) track)
					(emms-track-set track sym (eval sym)))))))

(defun fg-emms-info-track-description (track &optional no-fallback)
	"Return a description of TRACK."
	(let
		((desc (mapconcat
			(apply-partially 'emms-track-get track)
			'(info-artist info-album info-title) " :: ")))
		(if (and (not no-fallback) (string= desc " ::  :: "))
			(progn ;; TODO: check why emms doesn't fetch info immediately, disable it
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
		(emms-track-description (emms-playlist-current-selected-track))
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
