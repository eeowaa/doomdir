;;; eeowaa-refresh.el -*- lexical-binding: t; -*-

;; TODO Compare and contrast with `revert-buffer'. For reference, see how
;; `docker-container-mode' (derived from `tabulated-list-mode') has
;; `revert-buffer-function' set to `tabulated-list-revert', which runs the
;; `tabulated-list-revert-hook' containing `docker-container-refresh'. This
;; builtin functionality might be better (just use `revert-buffer' -- bound to
;; `SPC b r' in Doom -- to refresh contents of non-file-visiting buffers).

;;
;;; Configuration

(defgroup eeowaa-refresh nil
  "Buffer and display refresh functionality."
  :group 'convenience)

(defcustom eeowaa-refresh-mode-alist nil
  "Alist of major modes to buffer-refresh functions."
  :group 'eeowaa-refresh
  :type '(alist :key-type symbol
                :value-type function))

;;
;;; Functions

(defun eeowaa-refresh-buffer (&optional buffer-or-name)
  "Refresh buffer contents via `eeowaa-refresh-mode-alist'.
BUFFER-OR-NAME defaults to the current buffer."
  (interactive)
  (catch 'match
    (with-current-buffer (or buffer-or-name (current-buffer))
      (dolist (entry eeowaa-refresh-mode-alist)
        (let ((mode (car entry))
              (fn (cdr entry)))
          (when (eq major-mode mode)
            (throw 'match (funcall fn))))))))

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
