;; Font init
(set-default-font "Luxi Sans-8")


;; TODO: Pull colors from this stuff:
;;(color-theme-initialize)
;;(color-theme-lawrence)


;; Mask for de X
(defun masq_x ()
	(custom-set-faces
		'(default ((t (:foreground "wheat" :background "black"))))
		'(flyspell-duplicate ((t (:foreground "Gold3" :underline t :weight normal))))
		'(flyspell-incorrect ((t (:foreground "OrangeRed" :underline t :weight normal))))
		'(font-lock-comment-face ((t (:foreground "SteelBlue1"))))
		'(font-lock-function-name-face ((t (:foreground "gold"))))
		'(font-lock-keyword-face ((t (:foreground "springgreen"))))
		'(font-lock-type-face ((t (:foreground "PaleGreen"))))
		'(font-lock-variable-name-face ((t (:foreground "Coral"))))
		'(menu ((((type x-toolkit)) (:background "light slate gray" :foreground "wheat" :box (:line-width 2 :color "grey75" :style released-button)))))
		'(mode-line ((t (:foreground "black" :background "light slate gray"))))
		'(tool-bar ((((type x w32 mac) (class color)) (:background "midnight blue" :foreground "wheat" :box (:line-width 1 :style released-button))))))
		(tool-bar-mode -1)
		(set-cursor-color "deep sky blue")
		(set-foreground-color "wheat")
		(set-background-color "black")
		(set-face-foreground 'default "wheat")
		(set-face-background 'default "black"))

;; Mask 4 no-X
(defun masq_nox ()
	(custom-set-faces
		'(default ((t (:foreground "white" :background "black"))))
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


;; Fuck da startup screen
(custom-set-variables '(inhibit-startup-screen t))


;; TODO: M4 keyz for per-buffer mono/sans font switching (see buffer-face-set)
