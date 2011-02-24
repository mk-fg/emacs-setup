;;; obarray-fns.el --- obarray-manipulating routines

;; Copyright (C) 1995, 1998, 2006 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Created: 1998-08-18

;; $Id: obarray-fns.el,v 1.3 2006/10/25 09:01:23 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Updates of this program may be available via the URL
;; http://www.splode.com/~friedman/software/emacs-lisp/

;;; Code:

(defun obarrayp (ob)
  "Return t if OB is an obarray.
An obarray is a symbol table data structure used internally by emacs and as
a hashtable mechanism in many emacs lisp programs.  They are superficially
similar to vectors but their internal structure differs.  The set of
objects which pass `vectorp' in emacs are a proper superset of the set of
obarrays."
  (let ((candidate-sym))
    (and (vectorp ob)  ;; must be a vector
         ;; Immediate vector elements must be `0' (for empty buckets) or a
         ;; symbol interned in that obarray.  Anything else indicates a
         ;; corrupt obarray or a vector which is not an obarray at all.
         (let ((i 0)
               (passp t)
               obj)
           (while (< i (length ob))
             (setq obj (aref ob i))
             (setq i (1+ i))
             (cond ((symbolp obj)
                    (or candidate-sym
                        (setq candidate-sym obj)))
                   ((and (numberp obj)
                         (zerop obj)))
                   (t
                    (setq passp nil)
                    (setq i (length ob)))))
           passp)
         ;; If candidate-sym is nil, the obarray must be empty since any
         ;; non-empty obarray must have at least one symbol topmost in a
         ;; bucket.
         (or (null candidate-sym)
             ;; If interning fails, this obarray is defective in some way.
             (eq (intern-soft (symbol-name candidate-sym) ob)
                 candidate-sym)))))

(defun copy-obarray (ob)
  "Return a copy of an obarray.
Only the keys of the obarray are copied; the property list and function and
value cells of any symbols in the new obarray point to the same objects as
the old obarray."
  (let ((new-ob (make-vector (length ob) 0))
        (new-sym nil))
    (mapatoms (function
               (lambda (ob)
                 (setq new-sym (intern (symbol-name ob) new-ob))
                 (and (boundp ob)
                      (set new-sym (symbol-value ob)))
                 (and (fboundp ob)
                      (fset new-sym (symbol-function ob)))
                 (setplist new-sym (symbol-plist ob))))
              ob)
    new-ob))

(defun make-local-obarray (sym)
  "Make SYM, a symbol whose value should be an obarray, have a local value in the current buffer.
The new local value is a copy of the global value \(see copy-obarray\).
If SYM is already buffer-local, nothing is done."
  (interactive (list (intern
                      (completing-read
                       "Make Local Obarray Variable: "
                       obarray
                       (function (lambda (s)
                                   (and (boundp s)
                                        (obarrayp (symbol-value s)))))
                       t))))
  (cond ((if (fboundp 'local-variable-p)
             ;; this function introduced in emacs 19.29
             (local-variable-p sym)
           (assq sym (buffer-local-variables))))
        (t
         (set (make-local-variable sym)
              (copy-obarray (default-value sym))))))

(defun unintern-symbols-with-prefix (str &optional obarray-table)
  "Unintern all symbols starting with prefix STR from OBARRAY-TABLE.
OBARRAY-TABLE defaults to the variable `obarray'.

If `unintern' is not implemented \(emacs 19.28 and prior\), each symbol's
function and value cells are unbound and its property list is set to nil."
  (or obarray-table
      (setq obarray-table obarray))
  (let ((completions (all-completions str obarray-table))
        s)
    (while completions
      (setq s (intern-soft (car completions) obarray-table))
      (and s
           (if (fboundp 'unintern)
               ;; this function introduced in emacs 19.29
               (unintern s obarray-table)
             (setplist s nil)
             (makunbound s)
             (fmakunbound s)))
      (setq completions (cdr completions)))))


(defun find-symbols (pred &optional ob)
  "Return a list of symbols satisfying PRED.
PRED is a predicate of one argument: a symbol.
Optional arg OB is an obarray; default is the global obarray."
  (let ((result))
    (mapatoms (lambda (s)
                (and (funcall pred s)
                     (setq result (cons s result))))
              ob)
    result))

(defun symbols-with-property (prop &optional ob)
  "Return a list of symbols having property PROP."
  (find-symbols (lambda (s) (plist-member (symbol-plist s) prop)) ob))

(defun symbol-properties (&optional ob)
  "Return a list of all property names for all symbols."
  (let ((pl nil) p)
    (mapatoms (lambda (s)
                (setq p (symbol-plist s))
                (while p
                  (or (memq (car p) pl)
                      (setq pl (cons (car p) pl)))
                  (setq p (cdr (cdr p)))))
              ob)
    pl))

(provide 'obarray-fns)

;;; obarray-fns.el ends here
