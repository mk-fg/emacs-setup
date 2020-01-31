;; Basic setup
(require 'emms)
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-playlist-mode)
(require 'emms-playlist-limit)
(require 'emms-playing-time)

(setq-default
	emms-directory (concat fg-path "/tmp/emms")
	emms-source-file-default-directory "/mnt/db/mediaCore/sound_music/"

	emms-source-playlist-default-format 'm3u
	emms-playlist-mode-center-when-go t
	emms-playlist-default-major-mode 'emms-playlist-mode

	emms-show-format "NP: %s")
(make-directory emms-directory t)

(emms-playing-time 1)
(add-hook 'fg-emacs-exit-hook 'emms-stop)



;;;; Player

(require 'emms-player-mpv) ;; in extz
(setq-default
	emms-player-list '(emms-player-mpv)
	emms-player-mpv-environment
		'("PULSE_PROP_media.role=music")
	emms-player-mpv-parameters
		'("--quiet" "--really-quiet" "--no-audio-display" "--force-window=no" "--vo=null"))
(custom-set-variables '(emms-player-mpv-update-metadata t t))

;; (setq emms-player-mpv-debug t)
;; (setq emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-audio-display"))
;; (setq emms-player-mpv-ipc-method 'file)
;; (emms-player-mpv-start (emms-playlist-current-selected-track))
;; (emms-player-stop)
;; (emms-player-mpv-proc-stop)
;; (emms-player-mpv-ipc-stop)
;; (emms-play-url "https://somafm.com/sf1033130.pls")

(defun fg-emms-player-status-string ()
	(if emms-player-playing-p
		(if emms-player-paused-p "paused" "playing") "stopped"))



;;;; XXX - temporary hack 2020-01-24
;; "Error from substring-of-symlink in  emms-source-file-directory-tree-internal" on emms-help
;; Almost certainly incorrect fix, proper one would probably use (file-truename ...)

(defun emms-source-file-directory-tree-internal (dir regex)
	"Return a list of all files under DIR that match REGEX.
This function uses only emacs functions, so it might be a bit slow."
	(let ((files '())
		(dirs (list dir)))
		(while dirs
			(cond
				((file-directory-p (car dirs))
					(if
						(or (string-match "/\\.\\.?$" (car dirs))
							(let ((symlink (file-symlink-p (car dirs))))
								(and symlink
									(string-equal dir
										(substring symlink 0
											(min (string-width dir) (string-width symlink)))))))
						(setq dirs (cdr dirs))
						(setq dirs
							(condition-case nil
								(append (cdr dirs) (directory-files (car dirs) t nil t))
								(error (cdr dirs))))))
				((string-match regex (car dirs))
					(setq files (cons (car dirs) files)
					dirs (cdr dirs)))
				(t
					(setq dirs (cdr dirs)))))
		files))



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

(defun fg-emms-playlist-new ()
	"Create new emms playlist buffer, switch to it and make it active."
	(interactive)
	(let ((buff (emms-playlist-new)))
		(emms-playlist-set-playlist-buffer buff)
		(emms-playlist-mode-go)))

(defun fg-emms-playlist-cycle ()
	"Switch to next emms playlist buffer and make it the active one.
Rotates `emms-playlist-buffers' to cycle them all in order
and removes any empty buffers there along the way.
See also `emms-playlist-mode-next' for a more canonical way to do similar thing."
	(interactive)
	(let ((buff-list (emms-playlist-buffer-list)) buff)
		;; Skip/remove any empty buffers
		(while (and buff-list (not buff))
			(setq buff (cadr buff-list))
			(if (not buff) (setq buff (car buff-list))
				(when (= (buffer-size buff) 0)
					(kill-buffer buff)
					(setq buff nil
						buff-list (cons (car buff-list) (cddr buff-list))))))
		;; Note: altering emms-playlist-buffers directly is unsafe
		(when buff
			(emms-playlist-set-playlist-buffer buff)
			(emms-playlist-mode-go)
			(setq emms-playlist-buffers
				(append (cdr buff-list) (list (car buff-list)))))))



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
							(let ((val (eval sym))) (when val (emms-track-set track sym val))))))))))

(defvar fg-emms-info-max-len 160
	"Max length of the whole info-string from `emms-track-description-function'.
Fields get truncated to roughly equal length until it fits.")

(defvar fg-emms-info-max-field-len 100
	"Max length of emms-info field in `emms-track-description-function'.
Anything longer will be truncated to that length via `s-truncate'.")

(defun fg-emms-info-track-description (track &optional no-fallback)
	"Return a description of TRACK.
NO-FALLBACK disables fallback to filename/path
and such simple stuff when no other metadata is available."
	(let*
		(desc
			(fields (mapcar
				(lambda (sym) (or (emms-track-get track sym) ""))
				'(info-artist info-album info-title)))
			(field-len-min 15) (field-len-step 3) (field-sep " :: ")
			(field-len
				(min fg-emms-info-max-field-len
					(apply 'max field-len-min (mapcar 'length fields)))))
		(if (> (apply 'max (mapcar 'length fields)) 0)
			(while
				(or (not desc)
					(and
						(>= field-len field-len-min)
						(> (length desc) fg-emms-info-max-len)))
				(when desc (setq field-len (- field-len field-len-step)))
				(setq desc (mapconcat
					(apply-partially 's-truncate field-len) fields field-sep)))
			(if no-fallback
				(emms-track-name track)
				(fg-emms-track-info-fs track)
				(fg-emms-info-track-description track t)))
		desc))

(setq-default
	emms-track-description-function 'fg-emms-info-track-description
	emms-track-initialize-functions '(emms-info-initialize-track)
	emms-info-auto-update nil
	emms-info-asynchronously nil
	emms-info-functions '(fg-emms-track-info-fs))

(defun fg-emms-reset-playlist-info ()
	"Remove all associated info for all file-based tracks in current playlist buffer,
removing everything but 'type and 'name from there,
and run `emms-info-initialize-track' on each one of these afterwards."
	(with-current-emms-playlist (save-excursion
		(goto-char (point-min))
		(emms-walk-tracks
			(let ((track (emms-playlist-track-at (point))))
				(when (eq (emms-track-get track 'type) 'file)
					(let ((tail (cddr track)))
						(when (eq (caar tail) 'name)
							(setcdr tail nil)
							(emms-info-initialize-track track)))))))))

;; (fg-emms-reset-playlist-info)



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
;; emms-playlist-mode-switch-buffer lacks:
;;  a) emms-play-directory-tree call
;;  b) emms-playlist-mode-go

(defun emms ()
	"Switch to/from current `emms-playlist-buffer',
any live emms playlist bufffer, or invoke `emms-play-directory-tree'."
	(interactive)
	(if emms-playlist-buffer-p
		(emms-playlist-mode-bury-buffer)
		(unless (buffer-live-p emms-playlist-buffer)
			(let ((buff-list (emms-playlist-buffer-list)))
				(when buff-list (emms-playlist-set-playlist-buffer (car buff-list)))))
		(let
			((playlist-restore-pos
				(if
					(not (buffer-live-p emms-playlist-buffer))
					(prog1 nil (call-interactively 'emms-play-directory-tree))
					(with-current-emms-playlist
						(-when-let (m emms-playlist-selected-marker) (marker-position m))))))
			(emms-playlist-mode-go)
			(when playlist-restore-pos
				(with-current-emms-playlist
					(emms-playlist-select playlist-restore-pos)
					(goto-char playlist-restore-pos))))))



;;;; mpv --lavfi-complex visualization lua script control
;; Messages de-setup/mpv/fg.lavfi-audio-vis.lua to enable/disable vis,
;;  along with setting --vo and --force-window for it to display in.

(defvar fg-emms-mpv-vis-state nil
	"Target state for mpv visualization.")

(defun fg-emms-mpv-vis-lavfi-enable (delays)
	(emms-player-mpv-cmd
		'(get_property vo-configured)
		`(lambda (mpv-data mpv-error)
			(if (eq mpv-data t)
				(emms-player-mpv-cmd
					'(script-message fg.lavfi-audio-vis.on))
				(run-at-time ,(car delays) nil
					'fg-emms-mpv-vis-lavfi-enable ',(cdr delays))))))

(defun fg-emms-mpv-vis-state-sync (state-current lavfi-delays)
	(when (eq fg-emms-mpv-vis-state 'toggle)
		(setq fg-emms-mpv-vis-state (not state-current)))
	(unless
		(eq (null state-current) (null fg-emms-mpv-vis-state))
		(if fg-emms-mpv-vis-state
			(progn
				(emms-player-mpv-cmd '(set_property force-window yes))
				(emms-player-mpv-cmd '(set_property vo gl))
				(run-at-time (car lavfi-delays) nil
					#'fg-emms-mpv-vis-lavfi-enable (cdr lavfi-delays)))
			(emms-player-mpv-cmd
				'(script-message fg.lavfi-audio-vis.off))
			(emms-player-mpv-cmd '(set_property force-window no))
			(emms-player-mpv-cmd '(set_property vo null)))))

(cl-defun fg-emms-mpv-vis-toggle (&optional (state nil state?))
	"Check --vo and toggle --vo/--force-window + message
fg.lavfi-audio-vis.lua to enable/disable audio visualization.
STATE can be used to specify explicit on/off state for window/vis.
Should work with video/album-art tracks too, but didn't test lua for it much."
	(interactive)
	(setq fg-emms-mpv-vis-state (if state? state 'toggle))
	(emms-player-mpv-cmd-prog '(get_property vo)
		(let
			((state-current
				(condition-case err
					(let ((vo (aref mpv-data 0)))
						(not (or
							(string= (alist-get 'name vo) "null")
							(not (alist-get 'enabled vo)))))
					('error nil)))
				(lavfi-delays '(0.1 0.1 0.1 0.2 0.2 0.3 0.5 1 3 5)))
			(fg-emms-mpv-vis-state-sync state-current lavfi-delays))))

(defun fg-emms-mpv-vis-toggle-af-wall ()
	"Message fg.lavfi-audio-vis.lua to enable/disable 'wall' audio filter."
	(interactive)
	(emms-player-mpv-cmd '(script-message fg.lavfi-audio-vis.af.wall)))
