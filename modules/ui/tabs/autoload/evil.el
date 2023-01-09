;;; ui/tabs/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (modulep! :editor evil)

;;;###autoload (autoload '+tabs:new-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:new-window-tab ()
  "Ex wrapper around `tab-line-new-tab'."
  (interactive)
  (funcall #'tab-line-new-tab))

;;;###autoload (autoload '+tabs:close-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:close-window-tab ()
  "Ex wrapper around `tab-line-close-tab'."
  (interactive)
  (funcall #'tab-line-close-tab))

;;;###autoload (autoload '+tabs:next-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:next-window-tab (&optional count)
  "Ex wrapper around `tab-line-switch-to-next-tab'."
  (interactive "<c>")
  (unless count
    (setq count 1))
  (while (> count 0)
    (funcall #'tab-line-switch-to-next-tab)
    (setq count (1- count))))

;;;###autoload (autoload '+tabs:prev-window-tab "ui/tabs/autoload/evil" nil t)
(evil-define-command +tabs:prev-window-tab (&optional count)
  "Ex wrapper around `tab-line-switch-to-prev-tab'."
  (interactive "<c>")
  (unless count
    (setq count 1))
  (while (> count 0)
    (funcall #'tab-line-switch-to-prev-tab)
    (setq count (1- count))))
