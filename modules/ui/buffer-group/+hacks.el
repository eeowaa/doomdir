;;; ui/buffer-group/+hacks.el -*- lexical-binding: t; -*-

;;
;;; Core functions

;; Don't try to resize side windows
(advice-add #'balance-windows :around #'+buffer-group-side-windows-save-a)


;;
;;; External functions

;;;###package evil
(progn
  ;; Open the evil command window according to buffer display rules
  (defadvice! +buffer-group--evil-command-window-a (hist cmd-key execute-fn)
    "See `+popup--evil-command-window-a'.
Monkey patch the evil command window to use `pop-to-buffer' and to not
split the selected window."
    :override #'evil-command-window
    (when (eq major-mode 'evil-command-window-mode)
      (user-error "Cannot recursively open command line window"))
    (dolist (win (window-list))
      (when (equal (buffer-name (window-buffer win))
                   "*Command Line*")
        (kill-buffer (window-buffer win))
        (delete-window win)))
    (setq evil-command-window-current-buffer (current-buffer))
    (ignore-errors (kill-buffer "*Command Line*"))
    (with-current-buffer (pop-to-buffer "*Command Line*")
      (setq-local evil-command-window-execute-fn execute-fn)
      (setq-local evil-command-window-cmd-key cmd-key)
      (evil-command-window-mode)
      (evil-command-window-insert-commands hist)))

  (defadvice! +buffer-group--evil-command-window-execute-a ()
    "See `+popup--evil-command-window-execute-a'.
Execute the command under the cursor in the appropriate buffer, rather than
the command buffer. Also use `quit-window' instead of `delete-window'."
    :override #'evil-command-window-execute
    (interactive)
    (let ((result (buffer-substring (line-beginning-position)
                                    (line-end-position)))
          (execute-fn evil-command-window-execute-fn)
          (execute-window (get-buffer-window evil-command-window-current-buffer))
          (command-window (selected-window)))
      (if execute-window
          (select-window execute-window)
        (user-error "Originating buffer is no longer active"))
      (quit-window t command-window)
      (funcall execute-fn result)
      (setq evil-command-window-current-buffer nil)))

  ;; Prevent windows from moving into side windows
  (dolist (cmd (list #'+evil--window-swap
                     #'evil-window-move-very-bottom
                     #'evil-window-move-very-top
                     #'evil-window-move-far-left
                     #'evil-window-move-far-right))
    (advice-add cmd :around #'+buffer-group-side-windows-save-a)))

;;;###package man
;; Use `switch-to-buffer' to open new Man buffers
(setq Man-notify-method 'pushy)

;;;###package org
(after! org
  ;; Suppress deletion of other windows
  (defadvice! +buffer-group--suppress-delete-other-windows-a (fn &rest args)
    "See `+popup--suppress-delete-other-windows-a'.
Org has a scorched-earth window management policy I'm not fond of. i.e. it
kills all other windows just so it can monopolize the frame. No thanks. We can
do better."
    :around #'org-add-log-note
    :around #'org-capture-place-template
    :around #'org-export--dispatch-ui
    :around #'org-agenda-get-restriction-and-command
    :around #'org-goto-location
    :around #'org-fast-tag-selection
    :around #'org-fast-todo-selection
    (letf! ((#'delete-other-windows #'ignore)
            (#'delete-window        #'ignore))
      (apply fn args)))
 ;; Display tag/todo selection beneath selected window and shrink to fit
 (defadvice! +buffer-group--org-fix-popup-window-shrinking-a (fn &rest args)
     "See `+popup--org-fix-popup-window-shrinking-a'.
Hides the mode-line in *Org tags* buffer so you can actually see its
content and displays it in a side window without deleting all other windows.
Ugh, such an ugly hack."
      :around #'org-fast-tag-selection
      :around #'org-fast-todo-selection
      (letf! ((defun read-char-exclusive (&rest args)
                (message nil)
                (apply read-char-exclusive args))
              (defun split-window-vertically (&optional _size)
                (funcall split-window-vertically (- 0 window-min-height 1)))
              (defun org-fit-window-to-buffer (&optional window max-height min-height shrink-only)
                (when (> (window-buffer-height window)
                         (window-height window))
                  (fit-window-to-buffer window (window-buffer-height window)))))
        (apply fn args)))
  ;; Save org source edit buffers when switching workspaces
  (defadvice! +buffer-group--org-edit-src-exit-a (fn &rest args)
    "See `+popup--org-edit-src-exit-a'.
If you switch workspaces or the src window is recreated..."
    :around #'org-edit-src-exit
    (let* ((window (selected-window))
           (side-p (+buffer-group-side-window-p window)))
      (prog1 (apply fn args)
        (when (and side-p (window-live-p window))
          (delete-window window))))))

;;;###package treemacs
(progn
  ;; TODO Advise `treemacs--popup-window'
  ;; This is the default, but it's good to specify it
  (setq treemacs-display-in-side-window t
        treemacs-position 'left))

;;;###package undo-tree
;; Use `display-buffer' to show diffs instead of splitting a window.
(defadvice! +buffer-group--undo-tree-diff-display-buffer-a (&optional node)
  "Display an undo-tree diff buffer using `display-buffer'."
  :override '(undo-tree-visualizer-show-diff undo-tree-visualizer-update-diff)
  (setq undo-tree-visualizer-diff t)
  (let ((buff (with-current-buffer undo-tree-visualizer-parent-buffer
                (undo-tree-diff node))))
    (display-buffer buff)))

;;;###package which-key
;; Use the minibuffer instead of a side window to get full horizontal width.
(after! which-key
  (which-key-setup-minibuffer))

;;;###package windmove
;; Users should be able to hop into side windows easily, but Elisp shouldn't.
(defadvice! +buffer-group--ignore-window-parameters-a (fn &rest args)
  "Allow *interactive* window moving commands to traverse side windows."
  :around '(windmove-up windmove-down windmove-left windmove-right)
  (letf! (defun windmove-find-other-window (dir &optional arg window)
           (window-in-direction
            (pcase dir (`up 'above) (`down 'below) (_ dir))
            window t arg windmove-wrap-around t))
    (apply fn args)))
