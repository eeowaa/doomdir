;;; buffer-group.el --- Create and operate on groups of buffers -*- lexical-binding: t; -*-

;;; Code:

;;; Buffer group definitions

(defvar buffer-group-alist nil
  "An alist of buffer groups corresponding to property plists.
Each plist supports the following properties:

:names  A list of buffer name regexps.
:modes  A list of major mode symbols.

Use `buffer-group-define' to create a new buffer group.
Use `buffer-group-plist-put' and `buffer-group-property-pushnew'
to modify properties of an existing buffer group.")

(defun buffer-group-identifiers ()
  "Return identifiers for all buffer groups."
  (mapcar #'car buffer-group-alist))

;; TODO Add `interactive' spec to allow users to create a new buffer group with
;; no properties. The buffer group should not already exist.

;;;###autoload
(defmacro buffer-group-define (buffer-group &optional properties)
  "Define or redefine a buffer group."
  (declare (indent defun))
  (let ((member-fn (buffer-group-display-condition-fn buffer-group)))
    `(progn
       (defun ,member-fn (buffer-name _action)
         (buffer-group-member-p ',buffer-group buffer-name))
       (setf (alist-get ',buffer-group buffer-group-alist) ,properties)
       ',buffer-group)))


;;; Buffer group properties

(defun buffer-group-plist (buffer-group)
  "Return `buffer-group-alist' entry for BUFFER-GROUP."
  (alist-get buffer-group buffer-group-alist))

(defun buffer-group-plist-get (buffer-group property)
  "Get the value of PROPERTY for BUFFER-GROUP."
  (plist-get (buffer-group-plist buffer-group) property))

(defun buffer-group-plist-put (buffer-group property value)
  "Set the value of PROPERTY for BUFFER-GROUP."
  (let ((plist (alist-get buffer-group buffer-group-alist)))
    (setf plist (plist-put plist property value))))

(defun buffer-group-property-pushnew (buffer-group property &rest values)
  "Add VALUES to PROPERTY of BUFFER-GROUP."
  (let* ((plist (alist-get buffer-group buffer-group-alist))
         (value-list (plist-get plist property)))
    (dolist (value values)
      (cl-pushnew value value-list))
    (setf plist (plist-put plist property value-list))))

(defalias 'buffer-group-properties #'buffer-group-plist)
(defalias 'buffer-group-property-get #'buffer-group-plist-get)
(defalias 'buffer-group-property-put #'buffer-group-plist-put)


;;; Buffer group membership

(defun buffer-group-name-match-p (buffer-group &optional buffer-name)
  "Return non-nil if BUFFER-NAME matches a regexp in BUFFER-GROUP.
BUFFER-NAME defaults to the current buffer's name."
  (unless buffer-name
    (setq buffer-name (buffer-name)))
  (cl-some (lambda (re) (string-match-p re buffer-name))
           (buffer-group-plist-get buffer-group :names)))

(defun buffer-group-mode-match-p (buffer-group &optional buffer-name)
  "Return non-nil if BUFFER-NAME describes a buffer with major mode in BUFFER-GROUP.
BUFFER-NAME defaults to the current buffer's name."
  (with-current-buffer (or buffer-name (buffer-name))
    (memq major-mode (buffer-group-plist-get buffer-group :modes))))

(defun buffer-group-member-p (buffer-group &optional buffer-name)
  "Return non-nil if BUFFER-NAME is a member of BUFFER-GROUP.
BUFFER-NAME defaults to the current buffer's name."
  (or (buffer-group-name-match-p buffer-group buffer-name)
      (buffer-group-mode-match-p buffer-group buffer-name)))

(defun buffer-group-of (buffer-or-name)
  "Return the buffer group identifier of BUFFER-OR-NAME."
  (let ((buffer-name (if (bufferp buffer-or-name)
                         (buffer-name buffer-or-name)
                       buffer-or-name)))
    (catch 'match
      (dolist (buffer-group (buffer-group-identifiers))
        (when (buffer-group-member-p buffer-group buffer-name)
          (throw 'match buffer-group))))))


;;; Buffer group display rules

(defun buffer-group-display-condition-fn (buffer-group)
  "Return symbol for `display-buffer-alist' condition of BUFFER-GROUP."
  (intern (format "buffer-group-member-p--%s" buffer-group)))

(defun buffer-group-display-rule (buffer-group)
  "Return `display-buffer-alist' entry for BUFFER-GROUP."
  (alist-get (buffer-group-display-condition-fn buffer-group)
             display-buffer-alist))

(defun buffer-group-display-action (buffer-group)
  "Return action of `display-buffer-alist' entry for BUFFER-GROUP."
  (cdr-safe (buffer-group-display-rule buffer-group)))

(defun buffer-group-display-action-functions (buffer-group)
  "Return action functions of `display-buffer-alist' entry for BUFFER-GROUP.
The result is always a list, even if the entry in `display-buffer-alist'
contains a single function instead of a list of functions."
  (let ((functions (car-safe (buffer-group-display-action buffer-group))))
    (if (listp functions)
        functions
      (list functions))))

(defun buffer-group-display-action-alist (buffer-group)
  "Return action alist of `display-buffer-alist' entry for BUFFER-GROUP."
  (cdr-safe (buffer-group-display-action buffer-group)))


;;; Side window configuration

(defvar buffer-group-side-window-functions
  '(display-buffer-in-side-window)
  "Action functions for buffer-group side windows.
See `display-buffer-in-side-window' for more information.")

(defvar buffer-group-side-window-defaults
  '((side . bottom)
    (slot . 0))
  "Default action alist for buffer-group side windows.
See `display-buffer-in-side-window' for more information.")

(defvar buffer-group-side-window-preserve-size t
  "Preserve the \"size\" of a side window by default.
For top or bottom side windows, preserve window height.
For left or right side windows, preserve window width.")

(defun buffer-group-side-window--defaults (&optional alist)
  (let ((defaults buffer-group-side-window-defaults))
    (when buffer-group-side-window-preserve-size
      (let ((side (or (alist-get 'side alist)
                      (alist-get 'side buffer-group-side-window-defaults))))
        (setf (alist-get 'preserve-size defaults)
              (if (member side '(bottom top))
                  '(nil . t)
                '(t . nil)))))
    defaults))

;;;###autoload
(defun buffer-group-side-window-setup (buffer-group &optional alist)
  "Configure BUFFER-GROUP to display in a side window.
ALIST is merged with `buffer-group-side-window-defaults'."
  (let ((defaults (buffer-group-side-window--defaults alist))
        (condition (buffer-group-display-condition-fn buffer-group)))
    (mapc (lambda (action)
            (unless (alist-get (car action) alist)
              (push action alist)))
          defaults)
    (setf (alist-get condition display-buffer-alist)
          (cons buffer-group-side-window-functions alist)))
  display-buffer-alist)


;;; Reuse window configuration

(defvar buffer-group-reuse-window-functions
  '(buffer-group-display-in-group-window)
  "Action functions for buffer-group reused windows.")

(defun buffer-group-display-in-group-window (buffer alist)
  "Display BUFFER in a window of its buffer group.
ALIST is an association list of action symbols and values. See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists. The following symbol, when used in ALIST, has a
special meaning:

  `buffer-group' specifies a list of buffer groups to consider
    when determining a window to select. If nil, use the buffer
    group of BUFFER as determined by `buffer-group-of'.

This is an action function for buffer display, see Info node
`(elisp) Buffer Display Action Functions'. It should be called
only by `display-buffer' or a function directly or indirectly
called by the latter."
  (let* ((buffer-groups (or (alist-get 'buffer-group alist)
                            (list (buffer-group-of buffer))))
         (window (get-window-with-predicate
                  (lambda (win)
                    (let ((bufname (buffer-name (window-buffer win))))
                      (cl-some (lambda (bufgroup)
                                 (buffer-group-member-p bufgroup bufname))
                               buffer-groups))))))
    (when window
      (window--display-buffer buffer window 'reuse))))

;;;###autoload
(defun buffer-group-reuse-window-setup (buffer-group)
  "Configure BUFFER-GROUP to display in reused windows."
  (let ((condition (buffer-group-display-condition-fn buffer-group))
        (alist nil))
    (setf (alist-get condition display-buffer-alist)
          (cons buffer-group-reuse-window-functions alist)))
  display-buffer-alist)


;;; Default buffer groups

(buffer-group-side-window-setup
 (buffer-group-define diagnostics
   `(:names ("^\\*Warnings"
             "^\\*\\(?:CPU\\|Memory\\)-Profiler-Report "
             "^\\*lsp-log\\*"
             "^\\*.*ls\\(?:::stderr\\)?\\*"
             "^\\*envrc\\*")
     :modes (messages-buffer-mode debugger-mode))))

(buffer-group-side-window-setup
 (buffer-group-define search
   `(:modes (occur-mode grep-mode
             ,@(when (modulep! :completion vertico)
                 '(embark-collect-mode))))))

(buffer-group-side-window-setup
 (buffer-group-define compilation
   `(:names ("^\\*\\(?:[Cc]ompil\\(?:ation\\|e-Log\\)\\)"
             "^\\*Async\\(?: Shell Command\\|-native-compile-log\\)\\*"
             "^\\*doom eval\\*"
             "^ \\*Install vterm\\*")
     :modes (compilation-mode
             native-comp-limple-mode))))

(buffer-group-side-window-setup
 (buffer-group-define internals
   `(:names ("^\\*\\(?:Process List\\|timer-list\\|Threads\\)\\*"
             "^\\*\\(?:Ibuffer\\|Buffer List\\)\\*"
             "^\\*Bookmark List\\*"
             "^\\*Packages\\*")
     :modes (process-menu-mode timer-list-mode thread-list-mode
             Buffer-menu-mode ibuffer-mode
             bookmark-bmenu-mode
             package-menu-mode)))
 '((side . top)))

(provide 'buffer-group)

;;; buffer-group.el ends here
