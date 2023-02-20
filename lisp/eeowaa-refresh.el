;;; eeowaa-refresh.el -*- lexical-binding: t; -*-

(defvar-local eeowaa-refresh-force nil
  "Non-nil means to always call `revert-buffer' in `eeowaa-refresh-buffer'.")

(defun eeowaa-refresh-buffer (&optional buffer-or-name)
  "Refresh non-file-visiting buffer contents with `revert-buffer'.
BUFFER-OR-NAME defaults to the current buffer. The behavior of
this function can be modified via the buffer-local variable
`eeowaa-refresh-force'."
  (interactive)
  (with-current-buffer (or buffer-or-name (current-buffer))
    (cond
     (eeowaa-refresh-force (revert-buffer))
     (buffer-file-name (prog1 nil (message "Refusing to revert file-visiting buffer.")))
     ((local-variable-p 'revert-buffer-function) (revert-buffer))
     (t (prog1 nil (message "No buffer-local `revert-buffer-function'"))))))

(defun eeowaa-refresh-display ()
  "Redraw the display."
  (interactive)
  (redraw-display)
  (set-window-buffer nil (current-buffer))
  (when (fboundp 'vimish-tab-force-tab-line-update)
    (vimish-tab-force-tab-line-update)))

(defun eeowaa-refresh-buffer-and-display (&optional buffer-or-name)
  "Refresh buffer contents and redraw the display.
BUFFER-OR-NAME defaults to the current buffer."
  (interactive)
  (let ((inhibit-redisplay t))
    (eeowaa-refresh-buffer buffer-or-name))
  (eeowaa-refresh-display))

(provide 'eeowaa-refresh)
;;; eeowaa-refresh.el ends here
