(setq-default
	ediff-diff-program "ediff-prog"
	ediff-split-window-function 'split-window-horizontally
	ediff-use-long-help-message t)

;; This one always seem to mark my current frame as "unsuitable",
;;  while it always is, and it's the only one I really need
(defadvice ediff-skip-unsuitable-frames (around fg-ediff-skip-unsuitable-frames activate) t)
