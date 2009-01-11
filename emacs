(set-default-font "DejaVu Sans Condensed-8")
(set-fontset-font (frame-parameter nil 'font)
  'han '("cwTeXHeiBold" . "unicode-bmp"))


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


(color-theme-initialize)
(color-theme-infodoc)
(color-theme-late-night)


(setq make-backup-files nil)
(setq auto-save-mode nil)
