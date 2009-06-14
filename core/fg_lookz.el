;; Font init
(require 'font-lock)
(set-frame-font "Luxi Sans-8")
(global-font-lock-mode t) ; font can only be changed manually

;; Time is critical
(setq-default display-time-day-and-date t
	display-time-24hr-format t
	calendar-date-style 'european
	calendar-latitude [56 50 north]
	calendar-longitude [60 35 east]
	calendar-time-display-form ; "13:05 (YEKST)"
		'(24-hours ":" minutes
			(if time-zone " (") time-zone (if time-zone ")")))
(display-time)

;; Assorted minor tweaks
(setq inhibit-startup-screen t
	frame-title-format "emacs - %b" ; format the title-bar to include buffer name
	show-paren-style 'mixed ; mark the area in the direction of far parenthesis
	visible-bell t) ; flash on weird stuff

;; Cosmetic minor modes
(show-paren-mode) ; show matching parenthesis
(column-number-mode) ; column numbers at the bottom
(display-battery-mode) ; show the death clock

;; Rodent banishment (if any)
(when (and (display-mouse-p) (require 'avoid))
  (mouse-avoidance-mode 'animate))

;; Display page delimiter ^L as a horizontal line (plus "^L")
(or standard-display-table (setq standard-display-table (make-display-table)))
(aset standard-display-table ?\f (vconcat (make-vector 64 ?-) "^L"))


;; TODO: M4 keyz for per-buffer mono/sans font switching (see buffer-face-set)
;; TODO: Pull colors from this stuff:
; (color-theme-initialize)
; (color-theme-mods)


;; Mask for X
(defvar fg-color-fg-core)
(defvar fg-color-bg-core)

(defun fg-masq-x ()
	"Css-like binding."
	(custom-set-faces
		`(default
			((t
				(:foreground ,fg-color-fg-core
					:background ,fg-color-bg-core))))
		`(flyspell-duplicate ((t (:foreground "Gold3" :underline t :weight normal))))
		`(flyspell-incorrect ((t (:foreground "OrangeRed" :underline t :weight normal))))
		`(font-lock-comment-face ((t (:foreground "SteelBlue1"))))
		`(font-lock-function-name-face ((t (:foreground "gold"))))
		`(font-lock-keyword-face ((t (:foreground "springgreen"))))
		`(font-lock-type-face ((t (:foreground "PaleGreen"))))
		`(font-lock-variable-name-face ((t (:foreground "Coral"))))
		`(menu
			((((type x-toolkit))
			(:background "light slate gray"
				:foreground ,fg-color-fg-core
				:box (:line-width 2 :color "grey75" :style released-button)))))
		`(mode-line ((t (:foreground "black" :background "light slate gray")))))
	(tool-bar-mode -1)
	(scroll-bar-mode -1)
	(set-cursor-color fg-color-fg-core)
	(set-foreground-color fg-color-fg-core)
	(set-background-color fg-color-bg-core)
	(set-face-foreground 'default fg-color-fg-core)
	(set-face-background 'default fg-color-bg-core))

(defun fg-masq-x-dark ()
	(interactive)
	(let*
		((fg-color-fg-core "#6ad468")
		 (fg-color-bg-core "#101c10"))
		(fg-masq-x)))

(defun fg-masq-x-light ()
	(interactive)
	(let*
		((fg-color-fg-core "black")
		 (fg-color-bg-core "white"))
		(fg-masq-x)))

;; (defalias 'fg-masq-x-dark 'fg-masq-x-light)

;; Mask 4 no-X, uniform dark-only
(defun fg-masq-nox ()
	(interactive)
	(custom-set-faces
		'(default ((t (:foreground "wheat" :background "black"))))
		'(font-lock-comment-face ((t (:foreground "magenta"))))
		'(font-lock-function-name-face ((t (:foreground "red"))))
		'(font-lock-keyword-face ((t (:foreground "green"))))
		'(font-lock-type-face ((t (:foreground "blue"))))
		'(font-lock-string-face ((t (:foreground "cyan"))))
		'(font-lock-variable-name-face ((t (:foreground "blue"))))
		'(menu
			((((type x-toolkit))
				(:background "white"
					:foreground "black"
					:box (:line-width 2 :color "grey75" :style released-button)))))
		'(modeline ((t (:foreground "blue" :background "white")))))
	(set-cursor-color "blue")
	(set-foreground-color "white")
	(set-background-color "black")
	(set-face-foreground 'default "white")
	(set-face-background 'default "black"))


;; Set masq depending on time-of-the-day
(require 'solar)
(defvar fg-sunset-timer)
(defvar fg-sunrise-timer)

(defun fg-smart-lookz (&optional frame)
	"Automatically switch to dark background after sunset
and to light background after sunrise.
Note that `calendar-latitude' and `calendar-longitude'
should be set before calling the `solar-sunrise-sunset'."
	(interactive)
	(if (and calendar-latitude calendar-longitude calendar-time-zone)
		(let*
			((l (solar-sunrise-sunset (calendar-current-date)))
				(sunrise-string (apply 'solar-time-string (car l)))
				(sunset-string (apply 'solar-time-string (car (cdr l))))
				(current-time-string (format-time-string "%H:%M")))
			(if
				(or (string-lessp current-time-string sunrise-string)
					(string-lessp sunset-string current-time-string))
				(fg-masq-x-dark)
				(fg-masq-x-light))
			(when (and (boundp 'fg-sunset-timer) (timerp fg-sunset-timer))
				(cancel-timer fg-sunset-timer))
			(when (and (boundp 'fg-sunrise-timer) (timerp fg-sunrise-timer))
				(cancel-timer fg-sunrise-timer))
			(setq fg-sunset-timer (run-at-time sunset-string (* 60 60 24) 'fg-masq-x-dark))
			(setq fg-sunrise-timer (run-at-time sunrise-string (* 60 60 24) 'fg-masq-x-light)))))

;; Masquerade! :p
(if window-system
	(progn
		(fg-smart-lookz)
		(add-to-list 'after-make-frame-functions 'fg-smart-lookz))
	(fg-masq-nox)) ; time-of-the-day independent, since terms should be plain black
