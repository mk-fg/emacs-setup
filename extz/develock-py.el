;;
;; develock-py.el
;;
;; Made by Daniel Farina
;; Login   <drfarina@acm.org>
;;
;; Started on  Sun Feb 14 09:21:21 2010 Daniel Farina
;; Last update Sun Feb 14 09:27:12 2010 Daniel Farina
;;

(require 'develock)

(defcustom develock-python-font-lock-keywords
	'(;; a long line
		(develock-find-long-lines
		(1 'develock-long-line-1 t)
		(2 'develock-long-line-2 t))
		;; long spaces
		(develock-find-tab-or-long-space
		(1 'develock-whitespace-2)
		(2 'develock-whitespace-3 nil t))
		;; trailing whitespace
		("[^\t\n ]\\([\t ]+\\)$"
		(1 'develock-whitespace-1 t))
		;; spaces before tabs
		("\\( +\\)\\(\t+\\)"
		(1 'develock-whitespace-1 t)
		(2 'develock-whitespace-2 t))
		;; tab space tab
		("\\(\t\\) \t"
		(1 'develock-whitespace-2 append))
		;; only tabs or spaces in the line
		("^[\t ]+$"
		(0 'develock-whitespace-2 append))
		;; reachable E-mail addresses
		("<?[-+.0-9A-Z_a-z]+@[-0-9A-Z_a-z]+\\(\\.[-0-9A-Z_a-z]+\\)+>?"
		(0 'develock-reachable-mail-address t))
		;; things to be paid attention
		("\\<\\(?:[Ff][Ii][Xx][Mm][Ee]\\|[Tt][Oo][Dd][Oo]\\)\\(?::\\|\\>\\)"
		(0 'develock-attention t)))
	"Extraordinary level highlighting for the Python mode."
	:type develock-keywords-custom-type
	:set 'develock-keywords-custom-set
	:group 'develock
	:group 'font-lock)

(defvar python-font-lock-keywords-x nil
 "Extraordinary level font-lock keywords for the Python mode.")

(setq develock-keywords-alist
	(cons '(python-mode
			python-font-lock-keywords-x
			develock-python-font-lock-keywords)
		develock-keywords-alist))

(plist-put develock-max-column-plist 'python-mode 99)
