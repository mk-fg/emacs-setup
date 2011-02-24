;; use ";; -*- epa-file-encrypt-to: ("mk.fraggod@gmail.com") -*-" in file headers

(require 'epa-file)
(setq-default epa-file-encrypt-to "mk.fraggod@gmail.com")

(defvar epa-select-keys-inhibit t
	"Do not use interactive prompt for recipient keys,
using `epa-file-encrypt-to' value instead.")


;; These will replace epa-select-keys function, leaving the old definition as
;; epa-select-keys-interactive, so it can still be used, if necessary
(fset 'epa-select-keys-interactive (symbol-function 'epa-select-keys))

(defun epa-select-keys (context prompt &optional names secret static)
	"Return all key(s) referenced by name(s) in
`epa-file-encrypt-to' instead or a popup selection prompt
if `epa-select-keys-inhibit' is set to nil or STATIC is non-nil.
See `epa-select-keys-interactive' for the description of other parameters."
	(if (or static epa-select-keys-inhibit)
		(epg-list-keys context epa-file-encrypt-to secret)
		(epa-select-keys-interactive context prompt names secret)))


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
			(setq fg-stack-buffer (current-buffer)))
		(bury-buffer)
		(switch-to-buffer fg-stack-return-point)
		(setq fg-stack-return-point nil)))
