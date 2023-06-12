;;; eeowaa-refresh.el --- Refresh buffers and the display -*- lexical-binding: t; -*-

;;; Commentary:

;; Sometimes, all you want to do is refresh the contents of the current buffer
;; and update all display elements to match the current state of Emacs. This
;; library provides the `eeowaa-refresh-buffer-and-display' command to do that.

;; `eeowaa-refresh-buffer-and-display' works by calling `eeowaa-refresh-buffer',
;; which calls `revert-buffer' for non-file buffers, so long as the buffer-local
;; `revert-buffer-function' is set to a function. It then calls
;; `eeowaa-refresh-display' unconditionally, which updates display elements
;; outside of the buffer to represent the current state of Emacs.

;; The aforementioned implementation of `eeowaa-refresh-buffer-and-display'
;; allows it to be assigned to a global keybinding and used fairly mindlessly in
;; the event of any display problem, without the risk of inadvertently reverting
;; an unsaved buffer to older file content on disk. The `eeowaa-refresh-force'
;; buffer-local variable can be used to override this behavior.

;; Note that the usefulness of this library relies heavily on user
;; configuration. Specifically, major modes for non-file buffers that do not set
;; a buffer-local `revert-buffer-function' should have their mode hooks
;; configured to do so (as appropriate, and sometimes largely to taste).

;; Finally, if you are using this library to work around bugs in other packages,
;; strongly consider the negative implications of doing so. Bugs that do not
;; get attention do not get fixed. That said, part of the reason I wrote this
;; package was to buy me some time in fixing bugs in another package of mine...
;; Some of those bugs are still unsquashed :/

;;; Todo:

;; - Import obvious major-mode hook modifications from my private config
;; - Remove references to `vimish-tab' before publishing, unless it is already
;;   on MELPA by then

;;; Code:

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
