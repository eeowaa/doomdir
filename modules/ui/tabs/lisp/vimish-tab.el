;;; vimish-tab.el --- Vim-like window-local tabs using tab-line.el -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'tab-line)


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

;; TODO See about adding window parameters set by `tab-line':
;; `tab-line-groups', `tab-line-group', `tab-line-hscroll', `tab-line-cache'
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


;;; Tab buffers

(defvar vimish-tab-new-buffer-function #'vimish-tab-default-buffer
  "Function to return a buffer in a newly-created tab.")

(defun vimish-tab-default-buffer ()
  (get-buffer-create "*scratch*"))

(defun vimish-tab--update ()
  "Update the selected tab with current buffer info.
Creates new window parameters if they are missing or corrupted."
  (when vimish-tab-mode
    (if-let
        ((index (vimish-tab-index t))
         (tabs (vimish-tab-list t))
         (selected-tab (nth index tabs))
         (buffer (current-buffer)))
        (progn
          (setf (alist-get 'buffer selected-tab) buffer)
          (setf (alist-get 'name selected-tab) (buffer-name buffer))
          (set-window-parameter nil 'vimish-tab-list tabs))
      (set-window-parameter nil 'vimish-tab-index 0)
      (set-window-parameter nil 'vimish-tab-list
       (list (vimish-tab--make-tab (current-buffer) 0 t))))))

;; Ensure that the tab list gets updated appropriately
;; TODO Make this work across perspectives
(add-hook 'buffer-list-update-hook #'vimish-tab--update)


;;; Listing tabs

;; Used by `tab-line-format', `tab-line-format-template', and `tab-line-hscroll'.
(vimish-tab--set 'tab-line-tabs-function #'vimish-tab-list)

(defun vimish-tab-current ()
  "Return the current tab of the selected window."
  (nth (vimish-tab-index) (vimish-tab-list)))

(defun vimish-tab--property-hack (prop string)
  (when (and (eq prop 'tab) (null string))
    (vimish-tab-current)))

(advice-add 'tab-line--get-tab-property :before-until #'vimish-tab--property-hack)


;;; Displaying tabs

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

(defvar vimish-tab-show 1
  "Analogous to `tab-bar-show', except for the tab-line.")

(defun vimish-tab-show-p (&rest _)
  "Whether or not to display tabs for the current window."
  (or (eq vimish-tab-show t)
      (and (integerp vimish-tab-show)
           (> (length (vimish-tab-list)) vimish-tab-show))))

;; TODO Completely disable the display of the tab-line instead of just showing
;; an empty tab-line (like the following forms do):
;;
;;   (advice-add 'tab-line-format-template :before-while #'vimish-tab-show-p)
;;   (advice-add 'tab-line-format :before-while #'vimish-tab-show-p)
;;
;; Unfortunately, unlike the tab-bar, which can be hidden by setting the
;; tab-bar-lines frame parameter to 0, tab-line does not have such a construct.
;; Instead, the `tab-line-format' variable is consulted instead; see
;; `mode-line-format' for how it is evaluated.
;;
;; After some trail and error, it seems that setting `tab-line-format' to nil is
;; the only way to hide the tab line; setting `tab-line-format' to (:eval
;; (tab-line-format)) and making that function return nil will just display an
;; empty tab line.
;;
;; Unfortunately, let-binding `tab-line-format' to nil and performing
;; `force-mode-line-update' is not a valid workaround; neither is temporarily
;; setting the buffer-local `tab-line-format' in a similar fashion.
;;
;;   (let ((tab-line-format
;;          (when (vimish-tab-show-p) '(:eval tab-line-format))))
;;     (force-mode-line-update))
;;
;; This seems like a problem for #emacs or the mailing list. I don't think I can
;; resolve it myself.


;;; Selecting tabs

(defun vimish-tab-select-nth (n &optional tabs)
  "Select the Nth tab in TABS."
  (unless tabs
    (setq tabs (vimish-tab-list)))
  (unless (< -1 n (length tabs))
    (error "Out of bounds tab selection index"))
  (let (buffer result)
    (dolist (tab tabs)
      (if (eq (alist-get 'index tab) n)
          (let ((buf (alist-get 'buffer tab)))
            (setq buffer
                  (if (bufferp buf) buf
                    (warn "Selected tab does not specify a live buffer")
                    (funcall vimish-tab-new-buffer-function)))
            (setf (alist-get 'selected tab) t))
        (setf (alist-get 'selected tab) nil))
      (push tab result))
    (set-window-parameter nil 'vimish-tab-list (reverse result))
    (set-window-parameter nil 'vimish-tab-index n)
    (let ((display-buffer-overriding-action '(display-buffer-same-window)))
      (pop-to-buffer buffer)
      (vimish-tab-mode +1)))) ;; REVIEW Why is this necessary?

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

(advice-add 'tab-line-select-tab :override #'vimish-tab--select-tab-fix)


;;; Adding tabs

(defvaralias 'vimish-tab-new-button-show 'tab-line-new-button-show)

(defun vimish-tab-new ()
  "Function to call when adding a new window tab.
Currently only supports adding to the end of the list."
  (interactive)
  (let* ((tabs (vimish-tab-list))
         (buffer (funcall vimish-tab-new-buffer-function))
         (index (length tabs))
         (tab (vimish-tab--make-tab buffer index)))
    (vimish-tab-select-nth index
                           (append tabs (list tab)))))

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
      (set-window-parameter nil 'vimish-tab-list new-tabs))))

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

(vimish-tab--set 'tab-line-close-tab-function #'vimish-tab-close)


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


;;; Minor modes

;;;###autoload
(define-minor-mode vimish-tab-mode
  "Toggle Vim-like tabs for the current buffer.
You probably want to use `global-vimish-tab-mode' instead."
  :lighter nil
  (if vimish-tab-mode
      (when (vimish-tab--update)
        (tab-line-mode +1))
    (tab-line-mode -1)))

(defun vimish-tab-mode--turn-on ()
  "The same as `tab-line-mode--turn-on', except enable 'vimish-tab-mode'."
  (unless (or (minibufferp)
              (string-match-p "\\` " (buffer-name))
              (memq major-mode tab-line-exclude-modes)
              (get major-mode 'tab-line-exclude)
              (buffer-local-value 'tab-line-exclude (current-buffer)))
    (vimish-tab-mode +1)))

;;;###autoload
(define-globalized-minor-mode global-vimish-tab-mode
  vimish-tab-mode vimish-tab-mode--turn-on
  :group 'vimish-tab)

(provide 'vimish-tab)

;;; vimish-tab.el ends here