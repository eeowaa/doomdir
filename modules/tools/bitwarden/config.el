;;; tools/bitwarden/config.el -*- lexical-binding: t; -*-

(use-package! bitwarden
  :defer t
  :init
  (defvar +bitwarden-item-name-width 40
    "The width of item name strings displayed by `bitwarden-list-all'.")
  (defvar +bitwarden-user-name-width 32
    "The width of user name strings displayed by `bitwarden-list-all'.")
  (defvar +bitwarden-revision-date-width 24
    "The width of revision date strings displayed by `bitwarden-list-all'.")

  ;; XXX It is dangerous to leave passwords in memory!
  (defvar +bitwarden-list-buffer-action-fn #'quit-windows-on
    "The function to run after using `bitwarden-list-all'.
The function must take a buffer object as its first argument.")

  :config
  (defun +bitwarden--list-all ()
    (interactive)
    (bitwarden-list-dialog "*bitwarden-list*"
      (tree-widget-set-theme "folder")
      (apply 'widget-create
             `(group
               :indent 0
               :format "%v\n"
               ,@(bitwarden-list-all-items
                  (bitwarden-search))))
      (widget-create
       'push-button
       :notify 'bitwarden-list-cancel-dialog
       "Cancel")
      (goto-char (point-min))))

  (defadvice! +bitwarden-list-all-a ()
    :override #'bitwarden-list-all
    (interactive)
    (let ((buffer (get-buffer "*bitwarden-list*")))
      (cond
       ((buffer-live-p buffer)
        (pop-to-buffer buffer))
       ((bitwarden-unlocked-p)
        (+bitwarden--list-all))
       (t
        (bitwarden-login)
        (set-process-sentinel
         (get-process "bitwarden")
         (lambda (process event)
           (when (string-match-p "\\`finished" event)
             (+bitwarden--list-all))))))))

  (defadvice! +bitwarden--list-all-make-element-a (item)
    :override #'bitwarden-list-all-make-element
    (let* ((folder-id (gethash "folderId" item))
           (login-item (gethash "login" item)))
      (cons folder-id
            (list (cons (concat
                         (bitwarden-pad-to-width (gethash "name" item) +bitwarden-item-name-width)
                         (bitwarden-pad-to-width
                          (if login-item (gethash "username" login-item) "")
                          +bitwarden-user-name-width)
                         (format-time-string
                          "%Y-%m-%d %T"
                          (date-to-time (bitwarden-pad-to-width
                                         (gethash "revisionDate" item) +bitwarden-revision-date-width))))
                        item)))))

  (defadvice! +bitwarden--list-cancel-dialog-a (&rest _ignore)
    :override #'bitwarden-list-cancel-dialog
    (funcall +bitwarden-list-buffer-action-fn (current-buffer))
    (bitwarden--message "dialog canceled" nil t))

  (defadvice! +bitwarden--list-all-item-action-a (widget &rest _ignore)
    :override #'bitwarden-list-all-item-action
    (bitwarden-list-all-kill-ring-save (widget-value widget))
    (funcall +bitwarden-list-buffer-action-fn (current-buffer))))
