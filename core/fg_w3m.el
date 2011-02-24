;; For newsticker
;; (autoload 'w3m-region "w3m"
;; 	"Render region in current buffer and replace with result." t)
;; (autoload 'w3m-toggle-inline-image "w3m"
;; 	"Toggle the visibility of an image under point." t)

(require 'w3m)

(setq-default
	browse-url-browser-function 'w3m-browse-url

	w3m-profile-directory (concat fg-path "/tmp/w3m")
	w3m-init-file (concat w3m-profile-directory "/init")
	w3m-default-save-directory w3m-profile-directory
	w3m-arrived-file (concat w3m-profile-directory "/arrived")
	w3m-favicon-cache-file (concat w3m-profile-directory "/favicon-cache")
	w3m-session-file (concat w3m-profile-directory "/sessions")
	w3m-bookmark-file (concat w3m-profile-directory "/bookmarks.html")
	w3m-form-textarea-directory (concat w3m-profile-directory "/textareas")
	w3m-default-directory nil ;; aka "use w3m-profile-directory"

	w3m-use-favicon t
	w3m-favicon-use-cache-file t
	w3m-use-cookies t

	w3m-coding-system 'utf-8
	w3m-default-coding-system 'utf-8
	w3m-file-coding-system 'utf-8
	w3m-file-name-coding-system 'utf-8
	w3m-input-coding-system 'utf-8
	w3m-output-coding-system 'utf-8
	w3m-terminal-coding-system 'utf-8
	w3m-bookmark-file-coding-system 'utf-8
	w3m-coding-system-priority-list '(utf-8)

	w3m-confirm-leaving-secure-page nil)


	;; TODO: check out how it looks
	;; w3m-use-title-buffer-name t
	;; TODO: valid home here?
	;; w3m-home-page "about:"
	;; TODO: gnus-compose here (message-mode?)
	;; w3m-mailto-url-function '...

	;; TODO: setup these shortcuts
	;; (require 'w3m-search)
	;; w3m-uri-replace-alist '(...)
	;; w3m-search-engine-alist '(...)
	;; (eval-after-load "w3m-search"
	;;   '(progn
	;;      (add-to-list 'w3m-search-engine-alist
	;;                   '("ports"
	;;                     "http://www.freebsd.org/cgi/ports.cgi?query=%s"
	;;                     nil))
	;;      (add-to-list 'w3m-uri-replace-alist
	;;                   '("\\`g:" w3m-search-uri-replace "google"))
	;;      (add-to-list 'w3m-uri-replace-alist
	;;                   '("\\`fp:" w3m-search-uri-replace "ports"))))


;; Inhibit window-manipulations
(defun w3m-delete-frames-and-windows (&optional exception) nil)

;; Textarea linebreaks' consistency
(add-hook 'w3m-form-input-textarea-mode-hook
	(lambda()
		(save-excursion
			(while (re-search-forward "\r\n" nil t) (replace-match "\n" nil nil))
			(delete-other-windows))))


(make-directory w3m-profile-directory t)
(make-directory w3m-form-textarea-directory t)


;; TODO: >> fg_keyz.el
;; (global-set-key "\C-xm" 'browse-url-at-point)
;; (global-set-key (kbd "<f11>") 'w3m-browse-current-buffer)


;; Fetched from: http://www.b7j0c.org/stuff/dotw3m.html
;; TODO: in need of total revision


;; TODO: same for my link-list
;; ;; place your cursor over a link, save it in delicious
;; (defun delicious-post-url ()
;; 	(interactive)
;; 	(if (null (w3m-anchor))
;; 		(message "no anchor at point")
;; 		(let ((url (w3m-anchor)))
;; 			(if (w3m-url-valid url)
;; 			(progn (w3m-goto-url (concat "http://delicious.com/save?url=" url)))
;; 			(message "no URL at point!")))))


;; TODO: useful!
;; (defun w3m-browse-current-buffer ()
;; 	(interactive)
;; 	(let ((filename (concat (make-temp-file "w3m-") ".html")))
;; 		(unwind-protect
;; 			(progn
;; 				(write-region (point-min) (point-max) filename)
;; 				(w3m-find-file filename))
;; 			(delete-file filename))))


;; TODO: wtf4?
;; ;; link numbering
;; (require 'w3m-lnum)
;; (defun my-w3m-go-to-linknum ()
;; 	"Turn on link numbers and ask for one to go to."
;; 	(interactive)
;; 	(let ((active w3m-link-numbering-mode))
;; 		(when (not active) (w3m-link-numbering-mode))
;; 		(unwind-protect
;; 				(w3m-move-numbered-anchor (read-number "Anchor number: "))
;; 			(when (not active) (w3m-link-numbering-mode)))))
;; (define-key w3m-mode-map "f" 'my-w3m-go-to-linknum)
