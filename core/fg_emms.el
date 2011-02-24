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

	emms-track-description-function 'fg-emms-info-track-description
	emms-show-format "NP: %s")



;;;; Scrobbling
;; TODO: make scrobbler work, this module doesn't seem to have any obvious hooks for it
;; (when (require 'emms-lastfm-client nil t)
;; 	(setq-default
;; 		emms-lastfm-client-username "FraGGod"
;; 		emms-lastfm-client-api-key fg-auth-emms-lastfm-client-api-key
;; 		emms-lastfm-client-api-secret-key fg-auth-emms-lastfm-client-api-secret-key))



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



;;;; Track info
(defun* fg-emms-file-track-wash-name (title &key strip-ext)
	(dolist (sep '("_-_" "_"))
		(let ((title-parts (split-string title sep t)))
			(when (string-match-p "^[0-9]+$" (car title-parts))
				(setq title (mapconcat 'identity (cdr title-parts) sep))
				(return))))
	(replace-regexp-in-string "_+" " "
		(if strip-ext (replace-regexp-in-string "\\.[0-9a-zA-Z]+$" "" title) title)))

(defun* fg-emms-file-track-description (track)
	(let ((track (emms-track-name track)))
		(if
			(not (string-prefix-p emms-source-file-default-directory track))
			(car (last (split-string track "/")))
			(setq track (split-string (substring track
				(length emms-source-file-default-directory)) "/" t))
			(let
				(album artist
					(title (fg-emms-file-track-wash-name (car (last track)) :strip-ext t)))
				(if (> (length track) 2)
					(setq
						album
							(fg-emms-file-track-wash-name (car (last track 2)))
						artist (car (last track 3)))
					(setq artist (car (last track 2))))
				(setq artist (replace-regexp-in-string "_+" " " artist))
				(mapconcat 'identity (list artist album title) " :: ")))))

(defun fg-emms-info-track-description (track)
	"Return a description of TRACK."
	(let
		((artist (emms-track-get track 'info-artist))
			(title (emms-track-get track 'info-title)))
		(cond
			((and artist title) (concat artist " :: " title))
			(title title)
			(t (fg-emms-file-track-description track)))))

;; Here's true info. Pity it's so crippled...
;; TODO: fix this, not all data gets assigned, check while loop
;; TODO: perfomance impact here is huge, prehaps "call-process" is not async at all?
;; TODO: synchronous crap, I should either make it async or drop it altogether
;; (when (require 'emms-info-libtag nil t)
;; 	(setq-default
;; 		;; TODO: VERY slow over NFS, gotta do something about it first
;; 		;; emms-track-initialize-functions '(emms-info-initialize-track)
;; 		emms-track-initialize-functions nil
;; 		emms-info-auto-update nil
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
