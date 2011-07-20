(defun fg-hook-autopush ()
	(message (file-relative-name (buffer-file-name)
		"/home/fraggod/hatch/e1/public_transport/local_trains/"))
	(when
		(member
			(file-relative-name (buffer-file-name)
				"/home/fraggod/hatch/e1/public_transport/local_trains/")
			'("embed/style.scss" "embed/route_details.coffee" "route_web.php"))
		(start-process "fg-push" "fg-push" "/home/fraggod/bin/fgpush")))


(add-hook 'after-save-hook 'fg-hook-autopush)
(remove-hook 'after-save-hook 'fg-hook-autopush)
