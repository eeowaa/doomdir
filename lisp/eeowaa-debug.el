;;; eeowaa-debug.el -*- lexical-binding: t; -*-

;;
;;; Watchpoints

(defvar eeowaa-debug--watchpoints nil
  "A list of variable watchpoints.
This variable should not be modified directly. For that, use
`eeowaa-debug-watchpoint-set' and `eeowaa-debug-watchpoint-remove'.")

(defun eeowaa-debug--watchpoint-break (symbol newval operation where)
  "Start the debugger when SYMBOL is about to be modified.
See the help text for `add-variable-watcher' for an explanation
function arguments."
  (debug))

(defun eeowaa-debug-watchpoint-set (symbol)
  "Set a watchpoint for SYMBOL."
  (interactive
   `(,(completing-read "Symbol: "
                       obarray
                       ;; Ripped from `counsel--variable-p'
                       (lambda (s)
                         (or (and (boundp s)
                                  (not (keywordp s)))
                             (get s 'variable-documentation))))))
  (add-variable-watcher symbol #'eeowaa-debug--watchpoint-break)
  (cl-pushnew symbol eeowaa-debug--watchpoints))

(defun eeowaa-debug-watchpoint-unset (symbol)
  "Unset a watchpoint for SYMBOL."
  (interactive
   (list
    (if (null eeowaa-debug--watchpoints)
        (error "No watchpoints to remove")
      (let ((input (completing-read "Symbol: "
                                    eeowaa-debug--watchpoints nil t)))
             (if (string= input "")
                 (error "No symbol given")
               (intern input))))))
  (remove-variable-watcher symbol #'eeowaa-debug--watchpoint-break)
  (setq eeowaa-debug--watchpoints (delete symbol eeowaa-debug--watchpoints))
  (message (format "Removed watchpoint for `%s'" symbol)))

(defun eeowaa-debug-watchpoint-list ()
  "Print a list of all watched variables."
  (interactive)
  (princ eeowaa-debug--watchpoints))

;;
;;; Edebug

;; TODO: Create helper functions to work with edebug

(provide 'eeowaa-debug)
;;; eeowaa-debug.el ends here
