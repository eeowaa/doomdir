;;; eeowaa-help.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'doom-lib)

(defun eeowaa-alternate-keys (key &optional insert)
  "Print message listing equivalent alternate key sequences for KEY.
KEY is a pair (SEQ . RAW-SEQ) of key sequences, where
RAW-SEQ is the untranslated form of the key sequence SEQ.
If INSERT (the prefix arg) is non-nil, insert the message in the buffer.
While reading KEY interactively, this command temporarily enables
menu items or tool-bar buttons that are disabled to allow getting help
on them."
  (interactive
   ;; Ignore mouse movement events because it's too easy to miss the
   ;; message while moving the mouse.
   (list (car (help--read-key-sequence 'no-mouse-movement)) current-prefix-arg))
  (where-is (cadr (help--analyze-key (car key) (cdr key))) insert))

(defun eeowaa-find-command (key)
  "Find the definition of the command bound to KEY."
  (interactive
   (letf! (defadvice eeowaa-find-command-prompt-a (args)
            :filter-args #'read-key-sequence
            (cons "Find function bound to the following key, mouse click, or menu item: "
                  (cdr args)))
     (list (car (help--read-key-sequence 'no-mouse-movement)))))
  (find-function (cadr (help--analyze-key (car key) (cdr key)))))

(provide 'eeowaa-help)
;;; eeowaa-help.el ends here
