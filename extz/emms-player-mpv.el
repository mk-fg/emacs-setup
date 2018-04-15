;;
;; EMMS bindings for mpv player, using long-running mpv instance and JSON IPC interface
;; or fallback to oneshot mpv processes with --input-file for older versions (<0.7.0).
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

(defcustom emms-player-mpv-command-name "mpv"
	"mpv binary to use. Can be absolute path or just binary name."
	:type 'file
	:group 'emms-player-mpv)

(defcustom emms-player-mpv-parameters
	'("--quiet" "--really-quiet" "--no-audio-display")
	"Extra command-line arguments for started mpv process(es).
Either a list of strings or function returning such list.
Extra arguments --idle and --input-file/--input-ipc-server
are added automatically, depending on mpv version.
Note that unless --no-config option is specified here,
mpv will also use options from its configuration files.
For mpv binary path, see `emms-player-mpv-command-name'."
	:type '(choice (repeat :tag "List of mpv arguments" string) function)
	:group 'emms-player-mpv)

(defcustom emms-player-mpv-environment ()
	"List of extra environment variables (\"VAR=value\" strings) to pass on to mpv process.
These are added on top of `process-environment' by default.
Adding nil as an element to this list will discard emacs
`process-environment' and only pass variables that are specified in the list."
	:type '(repeat (choice string (const :tag "Start from blank environment" nil)))
	:group 'emms-player-mpv)

(defcustom emms-player-mpv-ipc-method nil
	"Switch for which IPC method to use with mpv.
Possible symbols: detect, ipc-server, unix-socket, file.
Defaults to nil value, which will cause `emms-mpv-ipc-detect'
to pick one based on mpv --version output.
Using JSON-IPC variants (ipc-server and unix-socket) enables
support for various feedback and metadata options from mpv."
	:type '(choice
		(const :tag "Auto-detect from mpv --version" nil)
		(const :tag "Use --input-ipc-server JSON IPC (v0.17.0 2016-04-11)" ipc-server)
		(const :tag "Use --input-unix-socket JSON IPC (v0.7.0 2014-10-16)" unix-socket)
		(const :tag "Use --input-file FIFO (any mpv version)" file))
	:group 'emms-player-mpv)

(defcustom emms-player-mpv-ipc-socket
	(expand-file-name (locate-user-emacs-file "emms-mpv-ipc.sock"))
	"Unix IPC socket or FIFO to use with mpv --input-* options,
depending on `emms-player-mpv-ipc-method' value and/or mpv version."
	:type 'file
	:group 'emms-player-mpv)


(defvar emms-mpv-proc nil
	"Running mpv process, controlled over --input-ipc-server/--input-file sockets.")

(defvar emms-mpv-proc-kill-delay 5
	"Delay until SIGKILL gets sent to `emms-mpv-proc',
if it refuses to exit cleanly on `emms-mpv-proc-stop'.")


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
	"JSON command for `emms-mpv-ipc-sentinel' to run as soon as it connects to mpv.
I.e. last command that either initiated connection or was used while connecting to mpv.
Set by `emms-player-mpv-start' and such, cleared once it gets sent by `emms-mpv-ipc-sentinel'.")

(defvar emms-mpv-ipc-req-id 1
	"Auto-incremented request_id value sent in JSON requests.
Wraps-around upon reaching `emms-mpv-ipc-req-id'-max automatically (unlikely to ever happen).")
(defvar emms-mpv-ipc-req-id-max (expt 2 30)
	"Max value for emms-mpv-ipc-req-id to wrap around after.
Should be fine with both mpv and emacs, and probably never reached anyway.")

(defvar emms-mpv-ipc-req-table nil
	"Auto-initialized hash table of outstanding API req_ids to their handler funcs.")

(defvar emms-mpv-ipc-ids '(:duration 1)
	"plist of ID integers assigned to keyword symbols, which get auto-replaced in requests.
E.g. '(observe_property :duration duration) -> '(observe_property 1 duration).
Existing IDs should never be changed at runtime without mpv restart.")


;; XXX: add user-defined events/requests/handlers
(defvar emms-mpv-ev-stopped t
	"Flag that is set when `emms-player-stopped' event is not supposed to be emitted.
This is to avoid confusing emms logic when mpv emits stop-start events on track changes.")



;; ----- helpers

(defvar emms-mpv-ipc-debug nil
	"Enable to print sent/received json lines and events
to *Messages* buffer using `emms-mpv-debug-msg'.")

(defun emms-mpv-debug-trim (s)
	(if (stringp s)
		(replace-regexp-in-string "\\(^[ \t\n\r]+\\|[ \t\n\r]+$\\)" "" s t t) s))

(defun emms-mpv-debug-msg (tpl-or-msg &rest tpl-values)
	"Print debug message to *Messages* if `emms-mpv-ipc-debug' is non-nil.
Message is only formatted if TPL-VALUES is non-empty.
Strips whitespace from start/end of TPL-OR-MSG and strings in TPL-VALUES."
	(when emms-mpv-ipc-debug
		(setq
			tpl-or-msg (emms-mpv-debug-trim tpl-or-msg)
			tpl-values (seq-map 'emms-mpv-debug-trim tpl-values))
		(unless tpl-values
			(setq tpl-or-msg (replace-regexp-in-string "%" "%%" tpl-or-msg t t)))
		(apply 'message tpl-or-msg tpl-values)))


(defun emms-mpv-ipc-fifo-p ()
	"Returns non-nil if --input-file fifo should be used.\
Runs `emms-mpv-ipc-detect' to init `emms-player-mpv-ipc-method' if necessary."
	(unless emms-player-mpv-ipc-method
		(setq emms-player-mpv-ipc-method
			(emms-mpv-ipc-detect emms-player-mpv-command-name)))
	(eq emms-player-mpv-ipc-method 'file))

(defun emms-mpv-ipc-detect (cmd)
	"Run mpv --version and return symbol for best IPC method supported.
CMD should be either name of mpv binary to use or full path to it.
Return values correspond to `emms-player-mpv-ipc-method' options.
Error is signaled if mpv binary fails to run."
	(with-temp-buffer
		(let ((exit-code (call-process cmd nil '(t t) nil "--version")))
			(unless (zerop exit-code)
				(insert (format "----- process exited with code %d -----" exit-code))
				(error (format "Failed to run mpv binary [%s]:\n%s" cmd (buffer-string))))
			(goto-char (point-min))
			(pcase
				(if (re-search-forward "^mpv\\s-+\\(\\([0-9]+\\.?\\)+\\)" nil t 1)
					(mapconcat (lambda (n) (format "%03d" n))
						(seq-map 'string-to-number
							(split-string (match-string-no-properties 1) "\\." t)) ".")
					"000.000.000")
				((pred (string> "000.006.999")) 'file)
				((pred (string> "000.016.999")) 'unix-socket)
				(- 'ipc-server)))))


;; ----- mpv process

(defun emms-mpv-proc-init-fifo (path &optional mode)
	"Create named pipe (fifo) socket for mpv --input-file PATH, if not exists already.
Optional MODE should be 12-bit octal integer, e.g. #o600 (safe default).
Signals error if mkfifo exits with non-zero code."
	(let ((attrs (file-attributes path)))
		(when ; remove non-fifo at the same path
			(and attrs (not (string-prefix-p "p" (nth 8 attrs))))
			(delete-file path) (setq attrs nil))
		(unless attrs
			(unless
				(zerop (call-process "mkfifo" nil nil nil
					(format "--mode=%o" (or mode #o600)) path))
				(error (format "Failed to run mkfifo for mpv --input-file path: %s" path))))))

(defun emms-mpv-proc-sentinel (proc ev)
	(emms-mpv-debug-msg "emms-mpv-proc: [%s] %s" proc ev)
	(when (and (emms-mpv-ipc-fifo-p) (eq (process-status proc) 'run))
		(setq emms-mpv-ev-stopped nil))
	(when (memq (process-status proc) '(exit signal))
		(unless emms-mpv-ev-stopped (emms-player-stopped))))

(defun emms-mpv-proc-init (&rest media-args)
	"initialize new mpv process as `emms-mpv-proc'.
MEDIA-ARGS are used instead of --idle, if specified."
	(emms-mpv-proc-stop)
	(when (emms-mpv-ipc-fifo-p)
		(emms-mpv-proc-init-fifo emms-player-mpv-ipc-socket))
	(let*
		((argv emms-player-mpv-parameters)
			(argv (append
				(list emms-player-mpv-command-name)
				(if (functionp argv) (argv) argv)
				(list (format "--input-%s=%s"
					emms-player-mpv-ipc-method emms-player-mpv-ipc-socket))
				(or media-args '("--idle"))))
			(env emms-player-mpv-environment)
			(process-environment (append
				(unless (seq-some 'not env) process-environment) (seq-filter 'identity env))))
		(emms-mpv-debug-msg "emms-mpv-proc: start %s" argv)
		(setq emms-mpv-proc
			(make-process :name "emms-mpv"
				:buffer nil :command argv :noquery t :sentinel 'emms-mpv-proc-sentinel))))

(defun emms-mpv-proc-stop ()
	"Stop running `emms-mpv-proc' instance via SIGINT, if any.
`delete-process' (SIGKILL) timer is started if `emms-mpv-proc-kill-delay' is non-nil."
	(when emms-mpv-proc
		(let ((proc emms-mpv-proc))
			(setq
				emms-mpv-proc nil
				emms-mpv-ev-stopped t)
			(if (not (process-live-p proc)) (delete-process proc)
				(interrupt-process proc)
				(when emms-mpv-proc-kill-delay
					(run-at-time
						emms-mpv-proc-kill-delay nil
						(lambda (proc) (delete-process proc)) proc))))))


;; ----- IPC socket/fifo

(defun emms-mpv-ipc-sentinel (proc ev)
	(emms-mpv-debug-msg "emms-mpv-ipc: [%s] %s" proc ev)
	(when (memq (process-status proc) '(open run))
		(emms-mpv-ipc-req-send
			'(observe_property :duration duration) nil proc)
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
			;; Process/remove all complete lines of json, if any
			(let ((p0 (point-min)))
				(while
					(progn
						(goto-char p0) (end-of-line)
						(equal (following-char) #xa))
					(let*
						((p1 (point))
							(json (buffer-substring p0 p1)))
						(delete-region p0 (+ p1 1))
						(emms-mpv-ipc-recv json)))))))

(defun emms-mpv-ipc-connect (delays)
	"Make IPC connection attempt, rescheduling if there's no socket by (car DELAYS).
(cdr DELAYS) gets passed to next connection attempt,
so it can be rescheduled further until function runs out of DELAYS values.
Sets `emms-mpv-ipc-proc' value to resulting process on success."
	(emms-mpv-debug-msg "emms-mpv-ipc-connect-delay: %s" (car delays))
	(setq emms-mpv-ipc-proc
		(make-network-process ; returns nil if there's no socket yet
			:name "emms-mpv-ipc"
			:family 'local
			:service emms-player-mpv-ipc-socket
			:nowait t
			:coding '(utf-8 . utf-8)
			:buffer (get-buffer-create emms-mpv-ipc-buffer)
			:noquery t
			:filter 'emms-mpv-ipc-filter
			:sentinel 'emms-mpv-ipc-sentinel))
	(when (and (not emms-mpv-ipc-proc) delays)
		(run-at-time (car delays) nil 'emms-mpv-ipc-connect (cdr delays))))

(defun emms-mpv-ipc-connect-fifo ()
	"Set `emms-mpv-ipc-proc' to process wrapper for
writing to a named pipe (fifo) file/node or signal error."
	(setq emms-mpv-ipc-proc
		(start-process-shell-command "emms-mpv-input-file" nil
			(format "cat > '%s'"
				(replace-regexp-in-string "'" "'\"'\"'" emms-player-mpv-ipc-socket t t))))
	(set-process-query-on-exit-flag emms-mpv-ipc-proc nil)
	(unless emms-mpv-ipc-proc (error (format
		"Failed to start cat-pipe to fifo: %s" emms-player-mpv-ipc-socket)))
	(when emms-mpv-ipc-connect-command
		(let ((cmd emms-mpv-ipc-connect-command))
			(setq emms-mpv-ipc-connect-command nil)
			(emms-mpv-ipc-fifo-cmd cmd emms-mpv-ipc-proc))))

(defun emms-mpv-ipc-init ()
	"initialize new mpv ipc socket/file process and associated state."
	(when emms-mpv-ipc-proc
		(delete-process emms-mpv-ipc-proc)
		(setq emms-mpv-ipc-proc nil))
	(if (emms-mpv-ipc-fifo-p) (emms-mpv-ipc-connect-fifo)
		(when emms-mpv-ipc-connect-timer (cancel-timer emms-mpv-ipc-connect-timer))
		(with-current-buffer (get-buffer-create emms-mpv-ipc-buffer) (erase-buffer))
		(setq
			emms-mpv-ipc-req-id 1
			emms-mpv-ipc-req-table nil
			emms-mpv-ipc-connect-timer nil
			emms-mpv-ipc-connect-timer
				(run-at-time (car emms-mpv-ipc-connect-delays)
					nil 'emms-mpv-ipc-connect (cdr emms-mpv-ipc-connect-delays)))))

(defun emms-mpv-ipc ()
	"Returns live+open ipc socket process or nil, (re-)starting mpv/connection if necessary.
Can return nil when starting async process/connection, and any follow-up
command should be stored to `emms-mpv-ipc-connect-command' in this case."
	(unless (process-live-p emms-mpv-proc) (emms-mpv-proc-init))
	(unless (process-live-p emms-mpv-ipc-proc) (emms-mpv-ipc-init))
	(and
		emms-mpv-ipc-proc
		(memq (process-status emms-mpv-ipc-proc) '(open run))
		emms-mpv-ipc-proc))


;; ----- IPC protocol

(defun emms-mpv-ipc-id-subst (cmd)
	"Return CMD with keyword symbols replaces according to `emms-mpv-ipc-ids'.
:json-* symbols are ignored, unrecognized symbols will signal error."
	(let (res)
		(dolist (v cmd (reverse res))
			(when (and (keywordp v) (not (string-prefix-p ":json-" (symbol-name v))))
				(setq v (or (plist-get emms-mpv-ipc-ids v)
					(error (format "ipc-id symbol not found in emms-mpv-ipc-ids: %s" v)))))
			(setq res (cons v res)))))

(defun emms-mpv-ipc-id-resolve (ipc-id)
	"Return keyword symbol corresponding to specified IPC-ID from `emms-mpv-ipc-ids' or nil."
	(let (res)
		(dolist (v emms-mpv-ipc-ids)
			(if (not res) (setq res v)
				(if (= v ipc-id) (cl-return res) (setq res nil))))))

(defun emms-mpv-ipc-req-send (cmd &optional handler proc)
	"Send JSON IPC request and assign HANDLER to response for it, if any.
HANDLER func will be called with decoded response JSON as (handler data err),
where ERR will be either nil on \"success\" or whatever is in JSON.
If HANDLER is nil, default `emms-mpv-ipc-req-error-printer' will be used to at least log errors.
PROC can be specified to avoid `emms-mpv-ipc' call (e.g. from sentinel/filter funcs)."
	(let
		((req-id emms-mpv-ipc-req-id)
			(cmd (emms-mpv-ipc-id-subst cmd))
			(proc (or proc (emms-mpv-ipc)))
			(handler (or handler 'emms-mpv-ipc-req-error-printer)))
		(setq emms-mpv-ipc-req-id
			(if (< emms-mpv-ipc-req-id emms-mpv-ipc-req-id-max)
				(1+ emms-mpv-ipc-req-id) 1))
		(unless emms-mpv-ipc-req-table
			(setq emms-mpv-ipc-req-table (make-hash-table)))
		(let ((json (concat (json-encode (list :command cmd :request_id req-id)) "\n")))
			(emms-mpv-debug-msg "emms-mpv-ipc-json >> %s" json)
			(process-send-string proc json))
		(puthash req-id handler emms-mpv-ipc-req-table)))

(defun emms-mpv-ipc-req-resolve (req-id data err)
	"Run handler-func for specified req-id."
	(let
		((handler (gethash req-id emms-mpv-ipc-req-table))
			(err (if (string= err "success") nil err)))
		(remhash req-id emms-mpv-ipc-req-table)
		(when handler (funcall handler data err))))

(defun emms-mpv-ipc-req-error-printer (data err)
	(when err (message "emms-mpv-ipc-error: %s" err)))

(defun emms-mpv-ipc-recv (json)
	"Handler for all JSON lines from mpv process.
Only used with JSON IPC, never called with --input-file as there's no feedback there."
	(emms-mpv-debug-msg "emms-mpv-ipc-json << %s" json)
	(let*
		((json-data (json-read-from-string json))
			(req-id (alist-get 'request_id json-data))
			(ev (alist-get 'event json-data)))
		(when req-id ; response to command
			(emms-mpv-ipc-req-resolve req-id
				(alist-get 'data json-data) (alist-get 'error json-data)))
		(pcase ev ; mpv event, if non-nil
			('nil) ; fast path for non-event messages
			;; XXX: add user-defined event handling here
			("property-change"
				(pcase (emms-mpv-ipc-id-resolve (alist-get 'id json-data))
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

(defun emms-mpv-ipc-fifo-cmd (cmd proc)
	"Send --input-file command string for older mpv versions.
PROC can be specified to avoid `emms-mpv-ipc' call."
	(let
		((proc (or proc (emms-mpv-ipc)))
			(cmd-line (concat (mapconcat
				(lambda (s) (format "%s" s))
				(emms-mpv-ipc-id-subst cmd) " ") "\n")))
			(emms-mpv-debug-msg "emms-mpv-ipc-fifo >> %s" cmd-line)
			(process-send-string proc cmd-line)))


;; ----- High-level EMMS interface

(defun emms-player-mpv-cmd (cmd &optional proc)
	"Send mpv command to process/connection if both are running,
or otherwise schedule start/connect and set
`emms-mpv-ipc-start-track' for `emms-mpv-ipc-sentinel'.
PROC can be specified to avoid `emms-mpv-ipc' call."
	(unless proc (setq proc (emms-mpv-ipc)))
	(if proc
		(if (emms-mpv-ipc-fifo-p)
			(emms-mpv-ipc-fifo-cmd cmd proc)
			(emms-mpv-ipc-req-send cmd nil proc))
		(setq emms-mpv-ipc-connect-command cmd)))


(defun emms-player-mpv-playable-p (track)
	(memq (emms-track-type track) '(file url streamlist playlist)))

(defun emms-player-mpv-start (track)
	(setq emms-mpv-ev-stopped t)
	(let
		((track-name (emms-track-get track 'name))
			(track-is-playlist (memq (emms-track-get track 'type) '(streamlist playlist))))
		(if (emms-mpv-ipc-fifo-p)
			(progn
				(emms-mpv-proc-init (if track-is-playlist "--playlist" "--") track-name)
				(emms-player-started emms-player-mpv))
			(let
				((cmd (if track-is-playlist 'loadlist 'loadfile)))
				(emms-player-mpv-cmd `(,cmd ,track-name replace))))))

(defun emms-player-mpv-stop ()
	(setq emms-mpv-ev-stopped t)
	(emms-player-mpv-cmd `(stop))
	(emms-player-stopped))


(defun emms-player-mpv-pause ()
	(emms-player-mpv-cmd `(set pause yes)))

(defun emms-player-mpv-resume ()
	(emms-player-mpv-cmd `(set pause no)))

(defun emms-player-mpv-seek (sec)
	(emms-player-mpv-cmd `(seek ,sec relative)))

(defun emms-player-mpv-seek-to (sec)
	(emms-player-mpv-cmd `(seek ,sec absolute)))

(emms-player-set emms-player-mpv 'pause 'emms-player-mpv-pause)
(emms-player-set emms-player-mpv 'resume 'emms-player-mpv-resume)
(emms-player-set emms-player-mpv 'seek 'emms-player-mpv-seek)
(emms-player-set emms-player-mpv 'seek-to 'emms-player-mpv-seek-to)


(provide 'emms-player-mpv)
