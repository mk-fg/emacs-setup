;; Font init
(set-frame-font "Luxi Sans-8")

;; Color init
(require 'font-lock)
(global-font-lock-mode t)

;; Startup screen? WTF!?
(custom-set-variables '(inhibit-startup-screen t))

;; Time is critical
(setq display-time-day-and-date t
	display-time-24hr-format t)
(display-time)

;; Format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")

;; Column numbers at the bottom
(column-number-mode t)

;; Parenthesis' matching
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Flash!
(setq visible-bell t)

;; Show the death clock
(display-battery-mode)


;; TODO: M4 keyz for per-buffer mono/sans font switching (see buffer-face-set)
;; TODO: Pull colors from this stuff:
; (color-theme-initialize)
; (color-theme-mods)


;; Mask for de X

; TODO: add time-based masq switching (see http://www.jurta.org/en/emacs/dotemacs)
; (defun masq_x ()
	; (let
		; ((color "green"))
		; (setq default-frame-alist
			; (append default-frame-alist
				; '((foreground-color . color)
				; (background-color . "black")
				; (cursor-color . "blue"))))))

(defun masq_x ()
	(custom-set-faces
		'(default ((t (:foreground "#6ad468" :background "#101c10"))))
		'(flyspell-duplicate ((t (:foreground "Gold3" :underline t :weight normal))))
		'(flyspell-incorrect ((t (:foreground "OrangeRed" :underline t :weight normal))))
		'(font-lock-comment-face ((t (:foreground "SteelBlue1"))))
		'(font-lock-function-name-face ((t (:foreground "gold"))))
		'(font-lock-keyword-face ((t (:foreground "springgreen"))))
		'(font-lock-type-face ((t (:foreground "PaleGreen"))))
		'(font-lock-variable-name-face ((t (:foreground "Coral"))))
		'(menu ((((type x-toolkit)) (:background "light slate gray" :foreground "#6ad468" :box (:line-width 2 :color "grey75" :style released-button)))))
		'(mode-line ((t (:foreground "black" :background "light slate gray")))))
	(tool-bar-mode -1)
	(set-cursor-color "#6ad468")
	(set-foreground-color "#6ad468")
	(set-background-color "#101c10")
	(set-face-foreground 'default "#6ad468")
	(set-face-background 'default "#101c10"))

;; Mask 4 no-X
(defun masq_nox ()
	(custom-set-faces
		'(default ((t (:foreground "wheat" :background "black"))))
		'(font-lock-comment-face ((t (:foreground "magenta"))))
		'(font-lock-function-name-face ((t (:foreground "red"))))
		'(font-lock-keyword-face ((t (:foreground "green"))))
		'(font-lock-type-face ((t (:foreground "blue"))))
		'(font-lock-string-face ((t (:foreground "cyan"))))
		'(font-lock-variable-name-face ((t (:foreground "blue"))))
		'(menu ((((type x-toolkit)) (:background "white" :foreground "black" :box (:line-width 2 :color "grey75" :style released-button)))))
		'(modeline ((t (:foreground "blue" :background "white")))))
	(set-cursor-color "blue")
	(set-foreground-color "white")
	(set-background-color "black")
	(set-face-foreground 'default "white")
	(set-face-background 'default "black"))

;; Masquerade!
(if window-system (masq_x) (masq_nox))
