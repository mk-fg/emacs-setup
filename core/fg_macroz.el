;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'setnu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Duplicate line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duplicate-line ()
	(interactive)
	(let
		((pos (point)))
		(progn
			(kill-whole-line)
			(yank) (yank)
			(goto-char pos))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control-Tab emulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nice-ctl-tab (arg)
  "Switches to another buffer (control-tab)-style.
This function must be bound to both the C-TAB key and the C-Sh-TAB key.
The exact behavior can be set to emacs-style, vc6-style, or vc8-style,
by calling the functions pc-control-tab-emulation-type-vc8,
pc-control-tab-emulation-type-vc8, and pc-control-tab-emulation-type-vc8.
Default behavior is vc8."
  (interactive "P")
  (let ((filtered-buffer-list (buffer-list))
        (tmp-buffer-list)
        (check-buffer))

    ;; Remove minibuffers and auxiliary buffers from buffer list
    (setq tmp-buffer-list filtered-buffer-list)
    (setq filtered-buffer-list nil)
    (while tmp-buffer-list
      (setq check-buffer (car tmp-buffer-list))
      (setq tmp-buffer-list (cdr tmp-buffer-list))
      (if (or (not (string-match "^[ \t]*\\*.*\\*$" (buffer-name check-buffer)))
              (equal "*scratch*" (buffer-name check-buffer)))
          (setq filtered-buffer-list (cons check-buffer filtered-buffer-list))))
    (setq filtered-buffer-list (nreverse filtered-buffer-list))

    ;; Message if no buffer is left, or only the current buffer is left
    ;; and the request is not to use another window.
    (if (null filtered-buffer-list)
        (message "No eligible buffer to switch to")
      (if (and (null (cdr filtered-buffer-list))
               (not arg)
               (eq (car filtered-buffer-list) (current-buffer)))
          (message "%s is the only eligible buffer"
                   (buffer-name (car filtered-buffer-list)))

        ; Good to go, dispatch to appropriate type of control-tab behavior
        (nice-ctl-tab-vc8 filtered-buffer-list arg)))))


(defun nice-ctl-tab-vc8 (filtered-buffer-list arg)
  "Switches to another buffer with control-tab, using vc8-style display of list of buffers.
When called by C-TAB, it offers to switch to the first buffer on the local
buffer list that is not a minibuffer or an auxiliary buffer such as *Help*.
When called by C-Sh-TAB, the last such buffer on the buffer list is used.
When the user presses C-TAB or C-Sh-TAB, all buffers on the buffer list are
offered cyclically, subject to the same selection criteria as the first one. "
  (let* ((temp-buffer-list)
        (selection-buffer-name "*PC-Mode Quick Buffer Selection*")
        (help-message "C-TAB/down-arrow=down, C-S-TAB/up-arrow=up, TAB/RET=select, other=abort.")
        (input)
        (num-windows-before (length (window-list)))
        (curr-buffer (current-buffer))
        (curr-buffer-name (buffer-name curr-buffer))
        (force-other-window)
        (target-buffer-name))

    (setq force-other-window (or arg
                                 (and (string-match "^[ \t]*\\*.*\\*$" curr-buffer-name)
                                      (not (equal curr-buffer-name "*scratch*")))))

    ;; Bring up the buffer selection buffer.
    ;;
    (unwind-protect
        (progn
          (if (eq 1 num-windows-before)
              (switch-to-buffer-other-window selection-buffer-name t)
            (switch-to-buffer selection-buffer-name t))
          (setq temp-buffer-list filtered-buffer-list)
          (while temp-buffer-list
            (insert (concat (buffer-name (car temp-buffer-list)) "\n"))
            (setq temp-buffer-list (cdr temp-buffer-list)))
          (set-buffer-modified-p nil)

          ;; Initial position of selection
          ;;
          (goto-char (point-min))
          ;; NOTE: Filtered buffer list may have length 1 if there is one
          ;; eligible buffer, but the current buffer is an uneligible one.
          (if (> (length filtered-buffer-list) 1)
              (if (equal (event-modifiers last-command-event) '(control))
                  (progn (end-of-line)
                         (if (equal curr-buffer-name (buffer-substring (point-min) (point)))
                             (forward-line 1))
                         (beginning-of-line))
                (goto-char (1- (point-max)))
                (beginning-of-line)))
          (set-mark (point))
          (end-of-line)

          ;; Cycle through displayed buffer names
          ;;
          (message help-message)
          (setq input (read-event))
          (while (and (eventp input)
                      (or (and (eq (event-basic-type input) 'down)
                               (eq (event-modifiers input) nil))
                          (and (eq (event-basic-type input) 'up)
                               (eq (event-modifiers input) nil))
                          (and (eq (event-basic-type input) 'tab)
                               (equal (event-modifiers input) '(control)))
                          (and (eq (event-basic-type input) 'tab)
                               (equal (event-modifiers input) '(control shift)))
                          (and (eq (event-basic-type input) 'iso-lefttab)
                               (equal (event-modifiers input) '(control)))
                          (and (eq (event-basic-type input) 'iso-lefttab)
                               (equal (event-modifiers input) '(control shift)))))
            (message help-message)
            (if (or (and (eq (event-basic-type input) 'down)
                         (eq (event-modifiers input) nil))
                    (and (eq (event-basic-type input) 'tab)
                         (equal (event-modifiers input) '(control)))
                    (and (eq (event-basic-type input) 'iso-lefttab)
                         (equal (event-modifiers input) '(control))))
                (progn
                  (forward-line 1)
                  (if (eobp) (goto-char (point-min))))
              (if (equal -1 (forward-line -1))
                  (progn (goto-char (1- (point-max)))
                         (beginning-of-line))))
            (set-mark (point))
            (end-of-line)
            (setq input (read-event))))

      ;; Record selected buffer name, kill temp buffer
      ;;
      (let ((eolpos (point)))
        (beginning-of-line)
        (setq target-buffer-name (buffer-substring (point) eolpos)))
      (kill-buffer nil)
      (if (eq 1 num-windows-before)
          (delete-window)))

    (switch-to-buffer curr-buffer) ; without this, buffer selection window would not always be restored properly

    ;; Switch buffers upon input RET or TAB
    ;;
    (if (and (eventp input)
             (or (eq (event-basic-type input) 'return)
                 (eq (event-basic-type input) 'tab))
             (not (event-modifiers input)))
        (if (or force-other-window
                (and
                 (get-buffer-window target-buffer-name)
                 (not (eq (get-buffer target-buffer-name) curr-buffer))))
              (switch-to-buffer-other-window target-buffer-name)
          (switch-to-buffer target-buffer-name))))
  (message ""))
