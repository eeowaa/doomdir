;;; ui/tabs/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (modulep! :editor evil)

;;;###autoload (autoload '+tabs:new-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:new-window-tab (&optional count file)
  "Ex wrapper around `vimish-tab-new'."
  (interactive "<c><f>")
  (if file
      (let ((vimish-tab-new-buffer-function
             (lambda () (vimish-tab-file-buffer file))))
        (funcall #'vimish-tab-new count))
    (funcall #'vimish-tab-new count)))

;;;###autoload (autoload '+tabs:new-project-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:new-project-window-tab (&optional count)
  "Ex wrapper to open a new tab containing a `projectile' project file."
  (interactive "<c>")
  (let ((vimish-tab-new-buffer-function #'vimish-tab-projectile-file-buffer))
    (funcall #'vimish-tab-new count)))

;;;###autoload (autoload '+tabs:new-blank-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:new-blank-window-tab (&optional count)
  "Ex wrapper to open a new blank window tab."
  (interactive "<c>")
  (let ((vimish-tab-default-buffer vimish-tab-fallback-buffer)
        (vimish-tab-new-buffer-function #'vimish-tab-default-buffer))
    (funcall #'vimish-tab-new count)))

;;;###autoload (autoload '+tabs:new-duplicate-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:new-duplicate-window-tab (&optional count)
  "Ex wrapper to open a new window tab displaying the current buffer."
  (interactive "<c>")
  (let ((vimish-tab-new-buffer-function #'current-buffer))
    (funcall #'vimish-tab-new count)))


;;;###autoload (autoload '+tabs:new-ffap-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:new-ffap-window-tab (&optional count)
  "Ex wrapper to open a new window tab displaying the file at point."
  (interactive "<c>")
  (let ((vimish-tab-new-buffer-function
         (lambda ()
           ;; TODO Remove dependency on `doom-thing-at-point-or-region'
           (vimish-tab-file-buffer (doom-thing-at-point-or-region)))))
    (funcall #'vimish-tab-new count)))

;;;###autoload (autoload '+tabs:new-ffap-with-line-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:new-ffap-with-line-window-tab (&optional count)
  "Ex wrapper to open a new window tab displaying the file/line/col at point.
This function heavily borrows from `evil-find-file-at-point-with-line'."
  (interactive "<c>")
  (require 'ffap)
  (let* ((fname (or (with-no-warnings (ffap-file-at-point))
                    (user-error "File does not exist.")))
         (vimish-tab-new-buffer-function (lambda ()
                                           (vimish-tab-file-buffer fname)))
         (line-number-pattern ":\\([0-9]+\\)\\=" )
         (line-and-column-numbers-pattern ":\\([0-9]+\\):\\([0-9]+\\)\\=")
         (get-number (lambda (pattern match-number)
                       (save-excursion
                         (goto-char (cadr ffap-string-at-point-region))
                         (and (re-search-backward pattern (line-beginning-position) t)
                              (string-to-number (match-string match-number))))))
         (line-number (or (funcall get-number line-and-column-numbers-pattern 1)
                          (funcall get-number line-number-pattern 1)))
         (column-number (funcall get-number line-and-column-numbers-pattern 2)))
      (message "line: %s, column: %s" line-number column-number)
      (funcall #'vimish-tab-new count)
      (when line-number
        (goto-char (point-min))
        (forward-line (1- line-number))
        (when column-number
          (move-to-column (1- column-number))))))


;;;###autoload (autoload '+tabs:close-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:close-window-tab (&optional count)
  "Ex wrapper around `vimish-tab-close-nth'."
  (interactive "<N>")
  (let ((index (if count (1- count) (vimish-tab-index))))
    (funcall #'vimish-tab-close-nth index)))

;;;###autoload (autoload '+tabs:close-other-window-tabs "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:close-other-window-tabs (&optional count)
  "Ex wrapper around `vimish-tab-close-other-tabs'."
  (interactive "<N>")
  (let ((index (if count (1- count) (vimish-tab-index))))
    (funcall #'vimish-tab-close-other-tabs index)))


;;;###autoload (autoload '+tabs:next-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:next-window-tab (&optional count)
  "Ex wrapper around `tab-line-switch-to-next-tab'."
  (interactive "<c>")
  (dotimes (_ (or count 1))
    (funcall #'tab-line-switch-to-next-tab)))

;;;###autoload (autoload '+tabs:prev-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:prev-window-tab (&optional count)
  "Ex wrapper around `tab-line-switch-to-prev-tab'."
  (interactive "<c>")
  (dotimes (_ (or count 1))
    (funcall #'tab-line-switch-to-prev-tab)))


;;;###autoload (autoload '+tabs:first-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:first-window-tab ()
  "Ex wrapper to select first `vimish-tab' tab."
  (interactive)
  (funcall #'vimish-tab-select-nth 0))

;;;###autoload (autoload '+tabs:last-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:last-window-tab ()
  "Ex wrapper to select last `vimish-tab' tab."
  (interactive)
  (funcall #'vimish-tab-select-nth (1- (length (vimish-tab-list)))))


;;;###autoload (autoload '+tabs:move-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:move-window-tab (&optional count)
  "Ex wrapper around `vimish-tab-move'."
  (interactive "<N>")
  (funcall #'vimish-tab-move count))
