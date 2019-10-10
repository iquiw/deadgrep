;;; deadgrep-transient.el --- Transient interface for deadgrep  -*- lexical-binding: t; -*-

;; URL: https://github.com/Wilfred/deadgrep
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.12.0") (transient "20191002"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Perform deadgrep searches with transient interface.

;; Install from MELPA, then `M-x deadgrep-transient' will do a search!

;;; Code:

(require 'deadgrep)
(require 'transient)

(define-infix-command deadgrep-transient:--after-context ()
  :description "After context"
  :class 'transient-option
  :key "-A"
  :argument "--after-context="
  :reader 'transient-read-number-N+)

(define-infix-command deadgrep-transient:--before-context ()
  :description "Before context"
  :class 'transient-option
  :key "-B"
  :argument "--before-context="
  :reader 'transient-read-number-N+)

(define-infix-command deadgrep-transient:--fixed-strings ()
  :description "String match"
  :class 'transient-switch
  :key "-F"
  :argument "--fixed-strings")

(define-infix-command deadgrep-transient:--*-case ()
  :description "Search case"
  :class 'transient-switches
  :key "-s"
  :argument-format "--%s"
  :argument-regexp "\\(--\\(smart-case\\|case-sensitive\\|ignore-case\\)\\)"
  :choices '("smart-case" "case-sensitive" "ignore-case"))

(define-infix-command deadgrep-transient:--type ()
  :description "File type"
  :class 'transient-option
  :key "-t"
  :argument "--type="
  :reader 'deadgrep-transient--read-file-type)

(define-infix-command deadgrep-transient:--glob ()
  :description "File glob"
  :class 'transient-option
  :key "-g"
  :argument "--glob="
  :reader 'deadgrep-transient--read-glob)

(define-infix-command deadgrep-transient:--word-regexp ()
  :description "Word match"
  :class 'transient-switch
  :key "-w"
  :argument "--word-regexp")

(define-transient-command deadgrep-transient ()
  "Start a ripgrep search"
  ["Arguments"
   (deadgrep-transient:--fixed-strings)
   (deadgrep-transient:--word-regexp)
   (deadgrep-transient:--*-case)
   (deadgrep-transient:--before-context)
   (deadgrep-transient:--after-context)
   (deadgrep-transient:--type)
   (deadgrep-transient:--glob)]
  ["Search"
   ("g" "Search in project root" deadgrep-transient-search)
   ("c" "Search in current directory" deadgrep-transient-search-in-current)])

(defun deadgrep-transient-search (search-term)
  "Search in the project root directory using transient arguments.
The command is intended to be used inetractively, and SEARCH-TERM is given from user's input."
  (interactive (list (deadgrep--read-search-term)))
  (let* ((deadgrep--arguments-function #'deadgrep-transient--arguments)
         (dir (funcall deadgrep-project-root-function))
         (buf (deadgrep--buffer
               search-term
               dir
               (or deadgrep--initial-filename
                   (buffer-file-name)))))

    (switch-to-buffer-other-window buf)

    (deadgrep-transient--set-options current-transient-suffixes)

    (setq imenu-create-index-function #'deadgrep--create-imenu-index)
    (setq next-error-function #'deadgrep-next-error)

    (deadgrep--write-heading)

    (deadgrep--start
     search-term
     deadgrep--search-type
     deadgrep--search-case)))

(defun deadgrep-transient--set-options (suffixes)
  "Set deadgrep options from transient SUFFIXES."
  (setq deadgrep--search-case 'sensitive)
  (setq deadgrep--search-type 'regexp)

  (let ((after-context 0) (before-context 0) word-regexp)
    (dolist (suffix suffixes)
      (pcase (oref suffix :command)
        ('deadgrep-transient:--after-context
         (-when-let (value (oref suffix value))
           (setq after-context (string-to-number value))))
        ('deadgrep-transient:--before-context
         (-when-let (value (oref suffix value))
           (setq before-context (string-to-number value))))

        ('deadgrep-transient:--fixed-strings
         (when (transient-infix-value suffix)
           (if word-regexp
               (setq deadgrep--search-type 'words)
             (setq deadgrep--search-type 'string))))

        ('deadgrep-transient:--word-regexp
         (when (transient-infix-value suffix)
           (setq word-regexp t)
           (when (eq deadgrep--search-type 'string)
             (setq deadgrep--search-type 'words))))

        ('deadgrep-transient:--*-case
         (-when-let (value (cdr (assoc (transient-infix-value suffix)
                                       '(("--case-sensitive" . sensitive)
                                         ("--ignore-case" . ignore)
                                         ("--smart-case" . smart)))))
           (setq deadgrep--search-case value)))

        ('deadgrep-transient:--glob
         (-when-let (value (oref suffix value))
           (setq deadgrep--file-type (cons 'glob value))))

        ('deadgrep-transient:--type
         (-when-let (value (oref suffix value))
           (setq deadgrep--file-type (cons 'type value))))))

    (when (or (/= before-context 0) (/= after-context 0))
      (setq deadgrep--context (cons before-context after-context)))))

(defun deadgrep-transient-search-in-current ()
  "Search the current directory using transient arguments."
  (interactive)
  (let ((deadgrep-project-root-function (lambda () default-directory)))
    (call-interactively #'deadgrep-transient-search)))

(defun deadgrep-transient--arguments (search-term &optional _search-type _case _context)
  "Format ripgrep command using SEARCH-TERM."
  (let (args
        (base-args '("--color=ansi" "--line-number" "--no-heading" "--with-filename")))
    (push "." args)
    (push search-term args)
    (push "--" args)
    (append
     base-args
     (transient-args 'deadgrep-transient)
     args)))

(defun deadgrep-transient--read-file-type (_prompt _initial-input _history)
  "Read file type for `deadgrep-transient:--file-type'."
  (deadgrep--read-file-type (buffer-file-name)))

(defun deadgrep-transient--read-glob (_prompt _initial-input _history)
  "Read glob pattern for `deadgrep-transient:--glob'."
  (deadgrep--read-glob (buffer-file-name)))

(define-key deadgrep-mode-map "t" #'deadgrep-transient)

(provide 'deadgrep-transient)
;;; deadgrep-transient.el ends here
