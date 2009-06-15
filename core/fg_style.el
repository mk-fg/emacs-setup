;; Tabs appearance and formatting basics
(setq require-final-newline t)
(setq indent-tabs-mode t)
(setq default-tab-width 2)
(setq indent-region-function 'fg-indent-command) ; default one inserts spaces
(setq py-indent-offset 2)
(setq tab-width 2)
(setq fill-column 80)
(setq basic-indent 2)
(setq tab-always-indent t) ; overidden by smart-tab

;; Stop emacs from arbitrarily adding lines to the end of a file when the
;; cursor is moved past the end of it
(setq next-line-add-newlines nil)
