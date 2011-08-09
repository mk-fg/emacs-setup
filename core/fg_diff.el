(setq-default
	ediff-diff-options "-a"
	ediff-split-window-function 'split-window-horizontally
	ediff-use-long-help-message t)

;; This one always seem to mark my current frame as "unsuitable",
;;  while it always is, and it's the only one I really need
(defun ediff-skip-unsuitable-frames (&optional ok-unsplittable) t)
