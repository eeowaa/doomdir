;;; eeowaa-debug.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

;;
;;; Watchpoints

(defvar eeowaa-watchpoint--symbols nil
  "A list of symbols with watchpoints.
This variable should not be modified directly. For that, use
`eeowaa-watchpoint-set' and `eeowaa-watchpoint-unset'.")

(defun eeowaa-watchpoint--break (&rest _)
  "Variable watch function to start the debugger.
Assign this function to the WATCH-FUNCTION argument of
`add-variable-watcher' to unconditionally start the debugger
when a SYMBOL is about to be modified."
  (debug))

(defun eeowaa-watchpoint-set (symbol)
  "Set a watchpoint for SYMBOL."
  (interactive
   `(,(intern
       (completing-read "Symbol: " obarray
                        ;; Ripped from `counsel--variable-p'
                        (lambda (s)
                          (or (and (boundp s)
                                   (not (keywordp s)))
                              (get s 'variable-documentation)))))))
  (add-variable-watcher symbol #'eeowaa-watchpoint--break)
  (cl-pushnew symbol eeowaa-watchpoint--symbols)
  (message (format "Added watchpoint for `%s'" symbol)))

(defun eeowaa-watchpoint-unset (symbol)
  "Unset a watchpoint for SYMBOL."
  (interactive
   (list
    (if (null eeowaa-watchpoint--symbols)
        (error "No watchpoints to remove")
      (let ((input (completing-read "Symbol: "
                                    eeowaa-watchpoint--symbols nil t)))
             (if (string= input "")
                 (error "No symbol given")
               (intern input))))))
  (remove-variable-watcher symbol #'eeowaa-watchpoint--break)
  (setq eeowaa-watchpoint--symbols (delete symbol eeowaa-watchpoint--symbols))
  (message (format "Removed watchpoint for `%s'" symbol)))

(defun eeowaa-watchpoint-list ()
  "Print a list of all watched variables."
  (interactive)
  (princ eeowaa-watchpoint--symbols))

;;
;;; Edebug

;; TODO: Create helper functions to work with edebug

(provide 'eeowaa-debug)
;;; eeowaa-debug.el ends here
