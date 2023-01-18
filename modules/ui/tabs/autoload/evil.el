;;; ui/tabs/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (modulep! :editor evil)

;;;###autoload (autoload '+tabs:new-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:new-window-tab ()
  "Ex wrapper around `tab-line-new-tab'."
  (interactive)
  (funcall #'tab-line-new-tab))

;;;###autoload (autoload '+tabs:new-blank-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:new-blank-window-tab ()
  "Ex wrapper to open a new blank window tab."
  (interactive)
  (let ((vimish-tab-default-buffer-name "*scratch*")
        (vimish-tab-new-buffer-function #'vimish-tab-default-buffer))
    (funcall #'vimish-tab-new)))


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
