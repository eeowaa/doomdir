;;; eeowaa-lib.el --- eeowaa's standard elisp library -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my collection of elisp helper constructs. Use them in the development
;; of other elisp libraries or in your personal Emacs config.

;;; Todo:

;; - Remove dependency on `doom-lib'
;; - Add more functions for working with faces

;;; Code:

(require 'doom-lib)


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


;;; Lists

(defmacro eeowaa-add-to-exclusion-list (list-var value)
  "Add VALUE to cdr of LIST-VAR with car of not."
  `(if (or (not (boundp ',list-var))
           (not (eq 'not (car ,list-var))))
       (setq ,list-var (list 'not ,value))
     (let ((list-cdr (cdr ,list-var)))
       (setcdr ,list-var (pushnew! list-cdr ,value))
       ,list-var)))

(defun eeowaa-alist-set (alist key value &optional compare-fn)
  "Ensure that ALIST contains (KEY . VALUE).
If ALIST already contains an element with a car of KEY, the cdr
of that element will be set to VALUE. Otherwise, a new element
will be inserted into the ALIST. If COMPARE-FN is provided, ALIST
element KEY values will be compared with that instead of `eq'."
  (error "Unimplemented"))


;;; Faces

(defmacro eeowaa-use-face (face other-face)
  "Force FACE to be the same as OTHER-FACE.
Examples:

  (eeowaa-use-face tab-bar-tab +workspace-tab-selected-face)
  (eeowaa-use-face tab-bar-tab-inactive +workspace-tab-face)"
  `(custom-set-faces!
     '(,face
       ,@(mapcan (lambda (key) (list key nil))
                 (doom-plist-keys (face-attr-construct face)))
       :inherit ,other-face)))


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


(provide 'eeowaa-lib)
;;; eeowaa-lib.el ends here
