;;; eeowaa-common.el --- Generic helper constructs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;;; User interaction

(defun eeowaa-parse-raw-prefix (arg)
  "Parse raw prefix argument ARG passed to the parent function.
Returns a list of (TYPE VALUE), where TYPE is a symbol and VALUE
is an integer. Possible forms include:

  (none 1)
      No prefix argument was supplied.

  (universal VALUE)
      `C-u' was typed one or more times, without a following numeric value.
      Evaluate `(log VALUE 4)' to determine how many times `C-u' was typed.

  (negative -1)
      `M--' or `C-u -' was typed, without following digits.

  (numeric VALUE)
      `C-u VALUE' or `M-VALUE' was typed, where VALUE is an integer.

Example usage:

  (defun my/test-function (arg)
    (interactive \"P\") ;; raw prefix argument
    (cl-destructuring-bind (type value) (eeowaa-parse-raw-prefix arg)
      (message \"%s: %d\" type value)))

See also: `(elisp) Prefix Command Arguments'."
  (cond
   ((null arg) (list 'none 1))
   ((listp arg) (list 'universal (car arg)))
   ((eq '- arg) (list 'negative -1))
   ((integerp arg) (list 'numeric arg))
   (t (error "Unrecognized prefix argument format: %s" arg))))

(defun eeowaa-read-positive-int (&optional prompt default)
  "Read a positive integer from the minibuffer.
PROMPT defaults to \"Positive integer: \""
  (let ((number (read-number (or prompt "Positive integer: ") default)))
    (if (and (integerp number) (> number 0))
        number
      (message "Please enter a positive integer.")
      (sit-for 1)
      (eeowaa-read-positive-int prompt))))


;;; Networking

(defvar dns-servers)
(defvar dns-timeout)

(defvar eeowaa-onlinep-dns-server "8.8.8.8") ;; Google DNS
(defvar eeowaa-onlinep-timeout 0.1)

(defun eeowaa-onlinep ()
  "Return non-nil if a public internet connection is available.
To make this determination, this function queries an external DNS
server for the hostname of its own IP address."
  (let ((dns-servers (list eeowaa-onlinep-dns-server))
        (dns-timeout eeowaa-onlinep-timeout))
    (dns-query eeowaa-onlinep-dns-server nil nil t)))


(provide 'eeowaa-common)
;;; eeowaa-common.el ends here
