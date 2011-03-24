(defvar fg-stack-time-line-format "---== %Y-%m-%d, %a"
	"Date format for auto-appended dates after sleep-lines.")

(defun fg-stack-check-previous-ts (time)
	"Check if previous timestamp is immediately
preceding to TIME and insert note if it isn't."
	(let ((err))
		(save-excursion
			(search-backward "---== " 0 t 2)
			(unless
				(looking-at
					(regexp-quote
						(format-time-string fg-stack-time-line-format
							(time-subtract time (seconds-to-time (* 24 3600))))))
				(setq err t)))
		(when err
			(insert (concat " -- " (propertize
				"note: time continuity seem to be broken"
				'font-lock-face 'bold)  " --")))))

(defun fg-newline-stack ()
	(interactive)
	(lexical-let
		((sleep-match
			(string-match "^\\([^[:space:]].*\\)?sleep"
				(buffer-substring (line-beginning-position) (point)))))
		(fg-newline)
		(when sleep-match
			(let*
				((time (current-time))
					(time
						(if (< (nth 2 (decode-time)) 18)
							time (time-add time (seconds-to-time (* 24 3600))))))
				(insert
					(format-time-string fg-stack-time-line-format time))
				(fg-stack-check-previous-ts time))
			(fg-newline))))


(defvar fg-stack-buffer nil
	"Buffer with fg-stack opened")
(defvar fg-stack-return-point nil
	"Buffer to return to if fg-stack is already active")

(defun fg-stack-buffer (&optional one-way)
	"Switch to fg-stack buffer and back"
	(interactive)
	(if
		(or one-way
			(not fg-stack-return-point)
			(not (eq fg-stack-buffer (current-buffer))))
		(progn
			(setq fg-stack-return-point (current-buffer))
			(find-file "~/media/secure/stack.gpg")
			(ignore-errors (fg-scite-stack t))
			(setq fg-stack-buffer (current-buffer)))
		(bury-buffer)
		(switch-to-buffer fg-stack-return-point)
		(setq fg-stack-return-point nil)))
