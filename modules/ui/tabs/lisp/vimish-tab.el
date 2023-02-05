;;; vimish-tab.el --- Vim-like window-local tabs using tab-line.el -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'tab-line)

(defgroup vimish-tab nil
  "Wrapper around `tab-line' using Vim idioms."
  :group 'convenience
  :version "27.1")


;;; Unused helper functions

;; REVIEW All of these functions are currently unused and can probably be
;; removed. I will keep them around for the time being until I have implemented
;; all desired functionality in this library.

(defun vimish-tab--set-nth (n list value)
  "Set the Nth element of LIST to VALUE and return the result."
  (append (seq-take list n) (list value) (nthcdr (1+ n) list)))

(defun vimish-tab--modify-alist-in-list (list alist &rest args)
  "Return a modified copy of LIST with an updated ALIST member.
ARGS should be a list of KEY and VALUE pairs."
  (let* ((tail (member alist list))
         (head (butlast list (length tail)))
         key value)
    (unless tail (error "ALIST is not a member of LIST"))
    (unless (cl-evenp (length args)) (error "Odd number of ARGS"))
    (while args
      (setq key (pop args)
            value (pop args))
      (setf (alist-get key alist) value))
    (append head (list alist) (cdr tail))))

(defun vimish-tab--modify-tab (tab &optional tabs &rest args)
  "Modify TAB by settings each KEY to VALUE in ARGS."
  (set-window-parameter nil 'vimish-tab-list
   (vimish-tab--modify-alist-in-list
    (or tabs (window-parameter nil 'vimish-tab-list))
    tab args)))


;;; Helper functions

(defun vimish-tab--set (&rest settings)
  "Given [SYM VAL] pairs, set each SYM to its VAL."
  (declare (indent defun))
  (while settings
    (let ((variable (pop settings))
          (value (pop settings)))
      (funcall (or (get variable 'custom-set) #'set-default-toplevel-value)
               variable value))))


;;; Window parameters

(cl-pushnew '(vimish-tab-index . writable) window-persistent-parameters)
(defun vimish-tab-index (&optional noerror)
  "Return the selected window's current tab index.
When NOERROR is provided, do not signal an error."
  (let ((index (window-parameter nil 'vimish-tab-index)))
    (cond
     ((wholenump index) index)
     (noerror nil)
     (t (error "Window parameter `vimish-tab-index' is not a non-negative integer")))))

(cl-pushnew '(vimish-tab-list . writable) window-persistent-parameters)
(defun vimish-tab-list (&optional noerror)
  "Return the selected window's tabs.
When NOERROR is provided, do not signal an error."
  (let ((tabs (window-parameter nil 'vimish-tab-list)))
    (cond
     ((listp tabs) tabs)
     (noerror nil)
     (t (error "Window parameter `vimish-tab-list' is not a list")))))

;; TODO See about adding other window parameters set by `tab-line':
;; `tab-line-groups', `tab-line-group', and `tab-line-hscroll'
(cl-pushnew '(tab-line-cache . writable) window-persistent-parameters)


;;; Tab alists

(defun vimish-tab--make-tab (buffer index &optional select)
  (unless (bufferp buffer)
    (error "BUFFER argument is not a buffer object"))
  (list (cons 'index index)
        (cons 'selected select)
        (cons 'name (buffer-name buffer))
        (cons 'buffer buffer)
        (cons 'select #'vimish-tab-select)))

;; REVIEW Consider refactoring parts of this library to set tab parameters
;; directly using `set-vimish-tab-parameter' instead of constructing new tab
;; lists and passing them to `set-window-parameter'.

(defun vimish-tab-parameters (&optional tab)
  "Return the parameters of TAB and their values.
TAB defaults to the selected tab."
  (or tab (vimish-tab-current)))

(defmacro vimish-tab-parameter (tab parameter)
  "Return TAB's value for PARAMETER.
TAB can be any tab and defaults to the selected one."
  `(alist-get ,parameter
              ,(or tab `(nth (window-parameter nil 'vimish-tab-index)
                             (window-parameter nil 'vimish-tab-list)))))

(defmacro set-vimish-tab-parameter (tab parameter value)
  "Set TAB's value of PARAMETER to VALUE.
TAB can be any tab and defaults to the selected one.
Return VALUE."
  `(setf (vimish-tab-parameter ,tab ,parameter) ,value))


;;; Tab buffers

(defcustom vimish-tab-new-buffer-function #'vimish-tab-default-buffer
  "Function to return a buffer in a newly-created tab."
  :type 'function
  :group 'vimish-tab)

(defconst vimish-tab-fallback-buffer "*scratch*"
  "Name of buffer to open in new tabs when all else fails.")

(defcustom vimish-tab-default-buffer vimish-tab-fallback-buffer
  "Name of default buffer opened by `vimish-tab-default-buffer'."
  :type '(choice
          (string :tag "Name of buffer to get or create")
          (function :tag "Function returning buffer object"))
  :group 'vimish-tab)

(defun vimish-tab-default-buffer ()
  (or (and (stringp vimish-tab-default-buffer)
           (get-buffer-create vimish-tab-default-buffer))
      (and (functionp vimish-tab-default-buffer)
           (funcall vimish-tab-default-buffer))
      (get-buffer-create vimish-tab-fallback-buffer)))

(defun vimish-tab-select-buffer ()
  (condition-case nil
      (get-buffer (read-buffer-to-switch "Open in new tab: "))
    (quit (vimish-tab-default-buffer))))

(defun vimish-tab--persp-complete-buffer ()
  (apply-partially #'completion-table-with-predicate
                   #'internal-complete-buffer
                   (lambda (name)
                     (member (if (consp name) (car name) name)
                             (mapcar #'buffer-name (persp-buffer-list))))
                   nil))

(defun vimish-tab-persp-buffer ()
  (condition-case nil
      (let ((completion-table (vimish-tab--persp-complete-buffer)))
        (minibuffer-with-setup-hook
            (lambda ()
              (setq minibuffer-completion-table completion-table)
              (if (and (boundp 'icomplete-with-completion-tables)
                       (listp icomplete-with-completion-tables))
                  (setq-local icomplete-with-completion-tables
                              (cons completion-table
                                    icomplete-with-completion-tables))))
          (get-buffer (read-buffer "Open in new tab: "
                                   (other-buffer (current-buffer))
                                   (confirm-nonexistent-file-or-buffer)))))
    (quit (vimish-tab-default-buffer))))

;; NOTE Without this, `find-file-noselect' will display the buffer in the
;; selected tab in addition to displaying the buffer in the new tab.
(defun vimish-tab-file-buffer (file)
  (save-window-excursion
    (find-file-noselect file)))

(defun vimish-tab-projectile-file-buffer ()
  (condition-case nil
      (let* ((project-root (projectile-acquire-root))
             (file (projectile-completing-read "Find file: "
                                               (projectile-project-files project-root))))
        (if (not file)
            (vimish-tab-default-buffer)
          ;; TODO Temporarily instrument `projectile-find-file-hook'
          (vimish-tab-file-buffer (expand-file-name file project-root))))
    (quit (vimish-tab-default-buffer))))


;;; Hooks

;; TODO Make this function more lightweight and do most of the heavy lifting in
;; in various package functions only when needed (not every time a window
;; changes its buffer).
(defun vimish-tab--update ()
  "Update the selected tab with current buffer info.
Creates new window parameters if they are missing and fixes corruption."
  (when vimish-tab-mode
    (if-let
        ((index (vimish-tab-index t))
         (tabs (vimish-tab-list t))
         (selected-tab (nth index tabs))
         (buffer (window-buffer)))
        (progn
          (setf (alist-get 'buffer selected-tab) buffer)
          (setf (alist-get 'name selected-tab) (buffer-name buffer))
          (setf (alist-get 'selected selected-tab) t) ;; HACK Fix corner cases
          (set-window-parameter nil 'vimish-tab-list tabs))
      (set-window-parameter nil 'vimish-tab-index 0)
      (set-window-parameter nil 'vimish-tab-list
       (list (vimish-tab--make-tab (current-buffer) 0 t))))
    (vimish-tab-redisplay)))

(put 'vimish-tab-update-window 'permanent-local-hook t)
(defun vimish-tab-update-window (window)
  (when (windowp window)
    (with-selected-window window
      (vimish-tab--update))))

;; TODO Advise `rename-buffer' and `kill-buffer' to update the tab lists of all
;; windows (including those not displayed).

;; REVIEW Ensure that the tab list gets updated appropriately
;; `window-buffer-change-functions'
;;     Functions called during redisplay when window buffers have changed.
;; `window-selection-change-functions'
;;     Functions called during redisplay when the selected window has changed.
;; `buffer-list-update-hook'
;;     Hook run when the buffer list changes.


;;; Listing tabs

;; Used by `tab-line-format', `tab-line-format-template', and `tab-line-hscroll'.
(vimish-tab--set 'tab-line-tabs-function #'vimish-tab-list)

(defun vimish-tab-current (&optional noerror)
  "Return the current tab of the selected window.
When NOERROR is provided, do not signal an error."
  (when-let* ((index (vimish-tab-index noerror))
              (tabs (vimish-tab-list noerror)))
    (nth index tabs)))

(defun vimish-tab--property-hack (prop string)
  (when (and (eq prop 'tab) (null string))
    (vimish-tab-current)))

(advice-add 'tab-line--get-tab-property :before-until #'vimish-tab--property-hack)

(defconst vimish-tab--force-mode-line-update
  (symbol-function 'force-mode-line-update)
  "Constant definition of `force-mode-line-update'.
Needed by `vimish-tab-force-mode-line-update' when called from
`vimish-tab--mode-line-update-hack' to avoid an infinite loop.")

(defvar vimish-tab--force-updating nil)

;;;###autoload
(defun vimish-tab-force-updating-p ()
  "Return non-nil if `vimish-tab-force-tab-line-update' is executing."
  vimish-tab--force-updating)

;; NOTE If this isn't working in some cases, we might want to try implementing a
;; hack found in Doom's `+vterm/here' to force a redraw:
;; (save-window-excursion (pop-to-buffer "*scratch*"))
(defun vimish-tab-force-tab-line-update (&rest _)
  "Force-update the tab line of the current buffer.
Use in place of `force-mode-line-update' to update the tab line
when switching between tabs containing the same buffer.

This works by temporarily toggling the buffer-modified flag. For
buffers not visiting a file, the variable `buffer-file-name' is
temporarily set to a dummy file path."
  (let ((mode-line-format (format-mode-line mode-line-format))
        (no-file (not buffer-file-name)))
    (when no-file (setq buffer-file-name "/tmp/.vimish-tab"))
    (let ((flag (buffer-modified-p)))
      (set-buffer-modified-p (not flag))
      (funcall vimish-tab--force-mode-line-update)
      (redisplay)
      (set-buffer-modified-p flag))
    (when no-file (setq buffer-file-name nil))))

(defun vimish-tab--mode-line-update-hack (fn &rest args)
  "Temporarily redefine `force-mode-line-update' for `vimish-tab'.
Use as around advice for functions calling `force-mode-line-update'."
  (cl-letf (((symbol-function 'force-mode-line-update)
             #'vimish-tab-force-tab-line-update))
    (apply fn args)))

(defun vimish-tab-name-format (tab tabs)
  (let* ((selected-p (alist-get 'selected tab))
         (name (alist-get 'name tab))
         (face (if selected-p
                   (if (eq (selected-window) (old-selected-window))
                       'tab-line-tab-current
                     'tab-line-tab)
                 'tab-line-tab-inactive)))
    (dolist (fn tab-line-tab-face-functions)
      (setf face (funcall fn tab tabs face nil selected-p)))
    (apply 'propertize
           (concat " "
                   (propertize name
                               'keymap tab-line-tab-map
                               'follow-link 'ignore)
                   (or (and tab-line-close-button-show
                            (not (eq tab-line-close-button-show
                                     (if selected-p 'non-selected 'selected)))
                            tab-line-close-button)
                       "")
                   " ")
           `(tab ,tab
             ,@(if selected-p '(selected t))
             face ,face
             mouse-face tab-line-highlight))))

(vimish-tab--set 'tab-line-tab-name-format-function #'vimish-tab-name-format)

(defcustom vimish-tab-show t
  "Analogous to `tab-bar-show', except for the tab-line."
  :type '(choice (const :tag "Always" t)
                 (const :tag "When more than one tab" 1)
                 (const :tag "Never" nil))
  :group 'vimish-tab)

(defun vimish-tab-show-p ()
  "Whether or not to display tabs for the current window."
  (or (eq vimish-tab-show t)
      (and (integerp vimish-tab-show)
           (> (length (vimish-tab-list)) vimish-tab-show))))

;; NOTE The `tab-line-format' window parameter overrides the buffer-local
;; variable `tab-line-format'. Because a buffer can be displayed in multiple
;; windows with differing values of `vimish-tab-show-p', the window parameter
;; should be used to hide the tab bar instead of the buffer-local variable.
;; Currently, this window parameter is updated by `vimish-tab--update' and
;; `vimish-tab-close-other-tabs'.
(cl-pushnew '(tab-line-format . writable) window-persistent-parameters)

(defun vimish-tab-redisplay ()
  "Selectively redisplay the tab line."
  (set-window-parameter nil 'tab-line-format
                            (unless (vimish-tab-show-p) 'none)))


;;; Selecting tabs

(defun vimish-tab-select-nth (n &optional tabs)
  "Select the Nth tab in TABS."
  (unless tabs
    (setq tabs (vimish-tab-list)))
  (unless (< -1 n (length tabs))
    (error "Out of bounds tab selection index"))
  (let ((old-buffer (alist-get 'buffer (vimish-tab-current t)))
        buffer result)
    (dolist (tab tabs)
      (if (eq (alist-get 'index tab) n)
          (let ((buf (alist-get 'buffer tab)))
            (setq buffer
                  (if (bufferp buf) buf
                    (warn "Selected tab does not specify a live buffer")
                    ;; FIXME Creating a new buffer when `global-vimish-tab-mode'
                    ;; is enabled will call `vimish-tab-mode' on the new buffer.
                    ;; This probably results in conflicts.
                    (funcall vimish-tab-new-buffer-function)))
            (setf (alist-get 'selected tab) t))
        (setf (alist-get 'selected tab) nil))
      (push tab result))
    (set-window-parameter nil 'vimish-tab-list (reverse result))
    (set-window-parameter nil 'vimish-tab-index n)
    (if (eq buffer old-buffer)
        (vimish-tab-force-tab-line-update)
      (let ((display-buffer-overriding-action '(display-buffer-same-window)))
        (pop-to-buffer buffer)))))
;;      (vimish-tab-mode +1))))) ;; REVIEW Why is this necessary?

(defun vimish-tab-select (tab &optional tabs)
  "Make TAB current in the selected window.
This function is assigned to the `select' alist entry of each tab."
  (vimish-tab-select-nth (alist-get 'index tab) tabs))

;; TODO Submit as upstream PR
(defun vimish-tab--select-tab-fix (&optional event)
  (interactive "e")
  (let* ((posnp (event-start event))
         (tab (tab-line--get-tab-property 'tab (car (posn-string posnp)))))
    (if (bufferp tab)
        (tab-line-select-tab-buffer tab (posn-window posnp))
      (let ((select (cdr (assq 'select tab))))
        (when (functionp select)
          (with-selected-window (posn-window posnp)
            (funcall select tab)
            (force-mode-line-update)))))))

(advice-add 'vimish-tab--select-tab-fix :around #'vimish-tab--mode-line-update-hack)
(advice-add 'tab-line-select-tab :override #'vimish-tab--select-tab-fix)


;;; Adding tabs

(defvaralias 'vimish-tab-new-button-show 'tab-line-new-button-show)

(defun vimish-tab-new (&optional n)
  "Function to call when adding a new window tab.
The new tab is inserted after the Nth tab (default current) and
is selected after insertion."
  (interactive "P")
  (let* ((tabs (vimish-tab-list))
         (index (if (integerp n) n
                  (1+ (vimish-tab-index))))
         (buffer (funcall vimish-tab-new-buffer-function))
         (tab (vimish-tab--make-tab buffer index))
         (left (seq-take tabs index))
         (right (mapc (lambda (tab)
                        (cl-incf (alist-get 'index tab)))
                      (nthcdr index tabs)))
         (new-tabs (append left (list tab) right)))
    (vimish-tab-select-nth index new-tabs)))

(vimish-tab--set 'tab-line-new-tab-choice #'vimish-tab-new)


;;; Closing tabs

(defvaralias 'vimish-tab-close-button-show 'tab-line-close-button-show)

(defun vimish-tab-close-nth (n &optional tabs)
  "Close the Nth tab in TABS.
Selects another tab if the Nth tab is currently selected."
  (unless tabs
    (setq tabs (vimish-tab-list)))
  (unless (< -1 n (length tabs))
    (error "Out of bounds tab closing index"))
  (when (= 1 (length tabs))
    (user-error "Cannot close last tab in window"))
  (let* ((index (vimish-tab-index))
         (left (seq-take tabs n))
         (right (mapc (lambda (tab)
                        (cl-decf (alist-get 'index tab)))
                      (nthcdr (1+ n) tabs)))
         (new-tabs (append left right)))
    (if (<= n index)
        (vimish-tab-select-nth (max 0 (1- index)) new-tabs)
      (set-window-parameter nil 'vimish-tab-list new-tabs)
      (vimish-tab-redisplay))))

(defun vimish-tab-close (tab &optional tabs)
  "Close TAB in the selected window.
In order for this function to be called with an appropriate TAB argument in
`tab-line-close-tab', observe the following usage patterns:

  Do NOT assign `vimish-tab-close' to the `close' alist entry of a tab.
  DO assign `vimish-tab-close' to the `tab-line-close-tab-function' variable."
  (vimish-tab-close-nth (alist-get 'index tab) tabs))

(defun vimish-tab-close-current ()
  "Close current tab in selected window."
  (interactive)
  (vimish-tab-close-nth (vimish-tab-index)))

(defcustom vimish-tab-close-window-function #'delete-window
  "Function to close a window when no tabs remain.
Used by `vimish-tab-close-tab-or-window'."
  :type 'function
  :group 'vimish-tab)

(defun vimish-tab-close-window ()
  "Close the current window regardless of how many tabs."
  (interactive)
  (funcall vimish-tab-close-window-function))

(defun vimish-tab-close-tab-or-window ()
  "Close the current tab or window (if only one tab left)."
  (interactive)
  (if (and vimish-tab-mode (> (length (vimish-tab-list)) 1))
      (vimish-tab-close-current)
    (funcall vimish-tab-close-window-function)))

(cl-defun vimish-tab-close-other-tabs
    (&optional (n (vimish-tab-index)) (tabs (vimish-tab-list)))
  "Close all but the Nth tab in TABS.
N defaults to the index of the selected tab."
  (interactive)
  (unless (< -1 n (length tabs))
    (error "Out of bounds tab closing index"))
  (let ((tab (nth n tabs)))
    (setf (alist-get 'index tab) 0)
    (set-window-parameter nil 'vimish-tab-list (list tab))
    (set-window-parameter nil 'vimish-tab-index 0)
    (vimish-tab-redisplay)))

(vimish-tab--set 'tab-line-close-tab-function #'vimish-tab-close)
(advice-add 'tab-line-close-tab :around #'vimish-tab--mode-line-update-hack)


;;; Cycling tabs

(defvaralias 'vimish-tab-switch-cycling 'tab-line-switch-cycling)

(defun vimish-tab-next (&optional event)
  "Make the next window tab current.
This function is influenced by `vimish-tab-switch-cycling'."
  (interactive (list last-nonmenu-event))
  (with-selected-window
      (or (and (listp event) (posn-window (event-start event)))
          (selected-window))
    (if-let
        ((tabs (vimish-tab-list))
         (index (vimish-tab-index))
         (tab (if (and vimish-tab-switch-cycling
                       (<= (length tabs) (1+ index)))
                  (car tabs)
                (nth (1+ index) tabs))))
        (vimish-tab-select tab tabs)
      (message "Unable to select next tab"))))

(advice-add 'tab-line-switch-to-next-tab :override #'vimish-tab-next)

(defun vimish-tab-prev (&optional event)
  "Make the previous window tab current.
This function is influenced by `vimish-tab-switch-cycling'."
  (interactive (list last-nonmenu-event))
  (with-selected-window
      (or (and (listp event) (posn-window (event-start event)))
          (selected-window))
    (if-let
        ((tabs (vimish-tab-list))
         (index (vimish-tab-index))
         (tab (if (and vimish-tab-switch-cycling
                       (<= index 0))
                  (car (last tabs))
                (nth (1- index) tabs))))
        (vimish-tab-select tab tabs)
      (message "Unable to select previous tab"))))

(advice-add 'tab-line-switch-to-prev-tab :override #'vimish-tab-prev)


;;; Moving tabs

(defun vimish-tab-move (&optional n)
  "Move the current window tab to the Nth position.
If N is omitted, default to the last position."
  (interactive)
  (let* ((tabs (vimish-tab-list))
         (index (vimish-tab-index))
         (n (or n (1- (length tabs))))
         (ascendingp (< index n)))
    (unless (< -1 n (length tabs))
      (error "Out of bounds tab moving index"))
    (unless (= index n)
      (let ((left (seq-take tabs (if ascendingp index n)))
            (right (nthcdr (1+ (if ascendingp n index)) tabs))
            (middle (mapc (lambda (tab)
                            (if ascendingp
                                (cl-decf (alist-get 'index tab))
                              (cl-incf (alist-get 'index tab))))
                          (seq-take (nthcdr (if ascendingp (1+ index) n) tabs)
                                    (if ascendingp (- n index) (- index n)))))
            (tab (nth index tabs)))
        (setf (alist-get 'index tab) n)
        (setq tabs (append left
                           (if ascendingp
                               (append middle (list tab))
                             (push tab middle))
                           right))
        (set-window-parameter nil 'vimish-tab-list tabs)
        (set-window-parameter nil 'vimish-tab-index n)))))


;;; Minor modes

(defvaralias 'vimish-tab-exclude 'tab-line-exclude)
(defvaralias 'vimish-tab-exclude-modes 'tab-line-exclude-modes)

(defcustom vimish-tab-exclude-names nil
  "List of buffer names regexps for which `vimish-tab' is not enabled.
Buffers under any of these major modes will not show the tab line in
their windows, even if `global-vimish-tab-mode' is enabled."
  :type 'list
  :group 'vimish-tab)

;;;###autoload
(define-minor-mode vimish-tab-mode
  "Toggle Vim-like tabs for the current buffer.
You probably want to use `global-vimish-tab-mode' instead."
  :lighter nil
  (if vimish-tab-mode
      (progn
        (vimish-tab--update)
        (add-hook 'window-buffer-change-functions #'vimish-tab-update-window nil t)
        (setq-local tab-line-format '(:eval (tab-line-format)))
        (tab-line-mode +1))
    (remove-hook 'window-buffer-change-functions #'vimish-tab-update-window t)
    (tab-line-mode -1)))

(defun vimish-tab-mode--turn-on ()
  "Similar to `tab-line-mode--turn-on', except enable 'vimish-tab-mode'.
Also respects the `vimish-tab-exclude-names' variable."
  (unless (or (minibufferp)
              (let ((buffer-name (buffer-name)))
                (cl-some (lambda (re) (string-match-p re buffer-name))
                         (cons "\\` " vimish-tab-exclude-names)))
              (memq major-mode vimish-tab-exclude-modes)
              (get major-mode 'vimish-tab-exclude)
              (buffer-local-value 'vimish-tab-exclude (current-buffer)))
    (vimish-tab-mode +1)))

;;;###autoload
(define-globalized-minor-mode global-vimish-tab-mode
  vimish-tab-mode vimish-tab-mode--turn-on
  :group 'vimish-tab)

(provide 'vimish-tab)

;;; vimish-tab.el ends here
