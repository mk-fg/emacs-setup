;;
;; EMMS bindings for mpv player, using long-running mpv instance and JSON IPC interface.
;;
;; Only restarts mpv instance if it crashed (shouldn't happen),
;;  allows querying stuff like file duration directly from mpv,
;;  as well as handling any other events from it.
;;


(require 'emms)
(require 'json)


(defcustom emms-player-mpv
	(emms-player
		'emms-player-mpv-start
		'emms-player-mpv-stop
		'emms-player-mpv-playable-p)
	"*Parameters for mpv player."
	:type '(cons symbol alist)
	:group 'emms-player-mpv)

(defcustom emms-mpv-ipc-socket
	(expand-file-name (locate-user-emacs-file "emms-mpv-input-ipc.sock"))
	"Unix IPC socket to use with mpv --input-ipc-server option."
	:type 'file
	:group 'emms-player-mpv)


(defvar emms-mpv-proc nil
	"Long-running mpv --idle process controlled over --input-ipc-server unix socket.")

(defvar emms-mpv-proc-cmd
	'("mpv" "--quiet" "--really-quiet" "--vo=null" "--idle"
		"--input-ipc-server=${emms-mpv-ipc-socket}")
	"mpv start command.
First arg will be used as both argv[0] and binary name,
each arg formatted via `s-lex-format' before each start.
Should probably include --idle and --input-ipc-server options.")

(defvar emms-mpv-proc-env-ext '("PULSE_PROP_media.role=music")
	"List of extra environment vars
(in addition to `process-environment') to pass to started mpv process.")

(defvar emms-mpv-proc-kill-delay 5
	"Delay until SIGKILL gets sent to `emms-mpv-proc', in case it refuses to exit cleanly.")


(defvar emms-mpv-ipc-proc nil
	"Unix socket process that communicates with running `emms-mpv-proc' instance.")

(defvar emms-mpv-ipc-buffer " *emms-mpv-ipc*"
	"Buffer to associate with `emms-mpv-ipc-proc' socket-process.")

(defvar emms-mpv-ipc-connect-timer nil
	"Timer for connection attempts to mpv ipc unix socket.")
(defvar emms-mpv-ipc-connect-delays
	'(0.1 0.1 0.1 0.1 0.1 0.1 0.2 0.2 0.3 0.3 0.5 1.0)
	"List of delays before initiating socket connection for new mpv process.")

(defvar emms-mpv-ipc-connect-command nil
	"json command for `emms-mpv-ipc-sentinel' to run as soon as it connects to mpv.
I.e. last command that either initiated connection or was used while connecting to mpv.
Set by `emms-player-mpv-start', cleared once it gets queued by `emms-mpv-ipc-sentinel'.")

(defvar emms-mpv-ipc-req-id 1
	"Auto-incremented request_id value sent in JSON requests.
Wraps-around upon reaching emms-mpv-ipc-req-id-max automatically (unlikely to ever happen).")
(defvar emms-mpv-ipc-req-id-max (expt 2 30)
	"Max value for emms-mpv-ipc-req-id to wrap around after.
Should be fine with both mpv and emacs, and probably never reached anyway.")

(defvar emms-mpv-ipc-req-table nil
	"Auto-initialized hash table of outstanding API req_ids to their handler funcs.")

(defvar emms-mpv-ipc-debug nil
	"Enable to print sent/received json lines and events to *Messages*.")

(defvar emms-mpv-ipc-ids
	'(:duration 1)
	"Plist of ID numbers (int) assigned to keyword symbols, which get auto-replaced in requests.
Existing IDs should never be changed at runtime without reconnect.")


(defvar emms-mpv-ev-stopped t
	"Flag that is set when `emms-player-stopped' event is not supposed to be emitted.
This is to avoid confusing emms logic when mpv emits stop-start events on track changes.")


;; mpv process/connection handlers

(defun emms-mpv-ipc-sentinel (proc ev)
	(when emms-mpv-ipc-debug
		(message "emms-mpv-ipc-sentinel: [%s] %s" proc (s-trim ev)))
	(when (s-starts-with? "open" ev)
		(emms-mpv-ipc-req-send
			'("observe_property" :duration "duration") nil proc)
		(when emms-mpv-ipc-connect-command
			(let ((cmd emms-mpv-ipc-connect-command))
				(setq emms-mpv-ipc-connect-command nil)
				(emms-mpv-ipc-req-send cmd nil proc)))))

(defun emms-mpv-ipc-filter (proc s)
	(when (buffer-live-p (process-buffer proc))
		(with-current-buffer (process-buffer proc)
			(let ((moving (= (point) (process-mark proc))))
				(save-excursion
					(goto-char (process-mark proc))
					(insert s)
					(set-marker (process-mark proc) (point)))
				(if moving (goto-char (process-mark proc))))
			;; process all complete lines of json, if any
			(let ((p0 (point-min)))
				(while
					(progn
						(goto-char p0) (end-of-line)
						(equal (following-char) #xa))
					(let*
						((p1 (point))
							(json (buffer-substring p0 p1)))
						(delete-region p0 (+ p1 1))
						(emms-mpv-ipc-line json)))))))

(defun emms-mpv-ipc-connect (delays)
	"Make IPC connection attempt, rescheduling if there's no socket by (car DELAYS).
(cdr DELAYS) gets passed to next connection attempt,
so it can be rescheduled further until function runs out of DELAYS values."
	(when emms-mpv-ipc-debug
		(message "emms-mpv-ipc-connect-delay: %s" (car delays)))
	(setq emms-mpv-ipc-proc
		(make-network-process ; returns nil if there's no socket yet
			:name "emms-mpv-ipc"
			:family 'local
			:service emms-mpv-ipc-socket
			:nowait t
			:coding '(utf-8 . utf-8)
			:buffer (get-buffer-create emms-mpv-ipc-buffer)
			:noquery t
			:filter 'emms-mpv-ipc-filter
			:sentinel 'emms-mpv-ipc-sentinel))
	(when (and (not emms-mpv-ipc-proc) delays)
		(run-at-time (car delays) nil 'emms-mpv-ipc-connect (cdr delays))))

(defun emms-mpv-ipc ()
	"Returns live+open ipc socket process or nil, (re-)starting mpv/connection if necessary.
Will always return nil when starting async process/connection,
and any follow-up command should be stored to `emms-mpv-ipc-connect-command' in this case."

	(unless (process-live-p emms-mpv-proc)
		(when emms-mpv-ipc-debug
			(message "emms-mpv-ipc: starting new mpv process"))
		(setq emms-mpv-proc
			(let
				((cmd
					(--map
						(s-format it (lambda (k) (symbol-value (intern k))))
						emms-mpv-proc-cmd))
					(process-environment
						(append process-environment emms-mpv-proc-env-ext)))
				(apply 'start-process "emms-mpv" nil (nth 0 cmd) cmd)))
			(set-process-query-on-exit-flag emms-mpv-proc nil))

	(unless (process-live-p emms-mpv-ipc-proc)
		(when emms-mpv-ipc-connect-timer (cancel-timer emms-mpv-ipc-connect-timer))
		(when emms-mpv-ipc-proc (delete-process emms-mpv-ipc-proc))
		(with-current-buffer (get-buffer-create emms-mpv-ipc-buffer) (erase-buffer))
		(setq
			emms-mpv-ipc-proc nil
			emms-mpv-ipc-connect-timer nil
			emms-mpv-ipc-req-id 1
			emms-mpv-ipc-req-table nil
			emms-mpv-ipc-connect-timer
				(run-at-time
					(car emms-mpv-ipc-connect-delays)
					nil 'emms-mpv-ipc-connect (cdr emms-mpv-ipc-connect-delays))))

	(and
		emms-mpv-ipc-proc
		(eq (process-status emms-mpv-ipc-proc) 'open)
		emms-mpv-ipc-proc))

;; Not used anywhere - player process/connection hang around forever
(defun emms-mpv-ipc-stop ()
	"Stop running `emms-mpv-proc' instance via SIGINT,
which should also close `emms-mpv-ipc-proc' socket.
`delete-process' (SIGKILL) timer is started if `emms-mpv-proc-kill-delay' is non-nil."
	(when emms-mpv-proc
		(let ((proc emms-mpv-proc))
			(setq emms-mpv-proc nil)
			(when (process-live-p proc)
				(interrupt-process proc)
				(when emms-mpv-proc-kill-delay
					(run-at-time
						emms-mpv-proc-kill-delay nil
						(lambda (proc) (delete-process proc)) proc))))))


;; JSON IPC protocol handlers

(defun emms-mpv-ipc-req-send (cmd &optional handler proc)
	"Send API request and assign HANDLER to response for it, if any.
HANDLER func will be called with decoded response json as (handler data err),
where ERR will be either nil on \"success\" or whatever is in json.
If HANDLER is nil, default `emms-mpv-ipc-req-error-printer' will be used to at least log errors.
PROC can be specified to avoid `emms-mpv-ipc' call (e.g. from sentinel/filter funcs)."
	(--map-indexed
		(when
			(and (keywordp it) (not (s-starts-with? ":json-" (symbol-name it))))
			(setcar (nthcdr it-index cmd) (plist-get emms-mpv-ipc-ids it)))
		cmd)
	(let
		((req-id emms-mpv-ipc-req-id)
			(proc (or proc (emms-mpv-ipc)))
			(handler (or handler 'emms-mpv-ipc-req-error-printer)))
		(setq emms-mpv-ipc-req-id
			(if (< emms-mpv-ipc-req-id emms-mpv-ipc-req-id-max)
				(1+ emms-mpv-ipc-req-id) 1))
		(when handler
			(unless emms-mpv-ipc-req-table
				(setq emms-mpv-ipc-req-table (make-hash-table)))
			(puthash req-id handler emms-mpv-ipc-req-table))
		(let ((json (concat (json-encode (list :command cmd :request_id req-id)) "\n")))
			(when emms-mpv-ipc-debug
				(message "emms-mpv-ipc-json >> %s" (s-trim json)))
			(process-send-string proc json))))

(defun emms-mpv-ipc-req-resolve (req-id data err)
	"Run handler-func for specified req-id."
	(let
		((handler (gethash req-id emms-mpv-ipc-req-table))
			(err (if (string= err "success") nil err)))
		(remhash req-id emms-mpv-ipc-req-table)
		(when handler (funcall handler data err))))

(defun emms-mpv-ipc-req-error-printer (data err)
	(when err (message "emms-mpv-ipc-error: %s" (s-trim err))))

(defun emms-mpv-ipc-line (json)
	"Handler for all json lines from mpv process."
	(when emms-mpv-ipc-debug
		(message "emms-mpv-ipc-json << %s" (s-trim json)))
	(let*
		((json-data (json-read-from-string json))
			(req-id (alist-get 'request_id json-data))
			(ev (alist-get 'event json-data)))
		(when req-id ; response to command
			(emms-mpv-ipc-req-resolve req-id
				(alist-get 'data json-data) (alist-get 'error json-data)))
		(pcase ev ; mpv event
			('nil)
			("property-change"
				(pcase
					;; Does reverse-mapping for emms-mpv-ipc-ids plist
					(let ((id (alist-get 'id json-data)) res)
						(dolist (v emms-mpv-ipc-ids)
							(if (not res) (setq res v)
								(if (= v id) (cl-return res) (setq res nil)))))
					(:duration
						(let
							((total (alist-get 'data json-data))
								(track (emms-playlist-current-selected-track)))
							(when total
								(setq total (round total))
								(emms-track-set track 'info-playing-time total)
								(emms-track-set track 'info-playing-time-min (/ total 60))
								(emms-track-set track 'info-playing-time-sec (% total 60)))))))
			;; On track-change after emms-player-start, mpv emits end-file + start-file,
			;;   and first one of these must not call emms-player-stopped, as that'd switch track again.
			("start-file" (setq emms-mpv-ev-stopped nil))
			("playback-restart" (emms-player-started emms-player-mpv))
			("end-file" (unless emms-mpv-ev-stopped (emms-player-stopped))))))


;; High-level EMMS interface

(defun emms-player-mpv-cmd (cmd &optional proc)
	"Send mpv command connection to it is open, or otherwise schedule
connection and set `emms-mpv-ipc-start-track' for `emms-mpv-ipc-sentinel'.
PROC can be specified to avoid `emms-mpv-ipc' call."
	(unless proc (setq proc (emms-mpv-ipc)))
	(if proc
		(emms-mpv-ipc-req-send cmd nil proc)
		(setq emms-mpv-ipc-connect-command cmd)))


(defun emms-player-mpv-playable-p (track)
	(memq (emms-track-type track) '(file url streamlist playlist)))

(defun emms-player-mpv-start (track)
	(setq emms-mpv-ev-stopped t)
	(let
		((cmd (if (memq (emms-track-get track 'type) '(streamlist playlist)) 'loadlist 'loadfile)))
		(emms-player-mpv-cmd `(,cmd ,(emms-track-get track 'name) replace))))

(defun emms-player-mpv-stop ()
	(setq emms-mpv-ev-stopped t)
	(emms-player-mpv-cmd `(stop))
	(emms-player-stopped))


(defun emms-player-mpv-pause ()
	(emms-player-mpv-cmd `(set_property pause t)))

(defun emms-player-mpv-resume ()
	(emms-player-mpv-cmd `(set_property pause ,json-false)))

(defun emms-player-mpv-seek (sec)
	(emms-player-mpv-cmd `(seek ,sec relative)))

(defun emms-player-mpv-seek-to (sec)
	(emms-player-mpv-cmd `(seek ,sec absolute)))

(emms-player-set emms-player-mpv 'pause 'emms-player-mpv-pause)
(emms-player-set emms-player-mpv 'resume 'emms-player-mpv-resume)
(emms-player-set emms-player-mpv 'seek 'emms-player-mpv-seek)
(emms-player-set emms-player-mpv 'seek-to 'emms-player-mpv-seek-to)


(provide 'emms-player-mpv)
