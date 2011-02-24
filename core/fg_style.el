;; Tabs appearance and formatting basics
(setq-default
	indent-tabs-mode t
	tab-width 2
	default-tab-width 2
	indent-region-function
		'fg-indent-command ; default one inserts spaces
	py-indent-offset 2
	python-indent 2
	python-guess-indent nil ; this disables spaces/tab flip in py buffers
	basic-indent 2
	require-final-newline t
	tab-always-indent t ; overidden by fg-tab
	c-syntactic-indentation nil ; stupid ten-tabs indentation
	fill-column 80)

;; Make absolutely sure python-mode uses tabs
(add-hook 'python-mode-hook
	#'(lambda () (setq tab-width 2 indent-tabs-mode t)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
