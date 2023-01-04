;;; ui/tags/config.el -*- lexical-binding: t; -*-

(use-package! tab-bar
  :when (modulep! :ui workspaces)
  :hook (persp-mode . tab-bar-mode)
  :preface
  (defun my/+workspace-tabs-fn ()
    (let ((current (+workspace-current-name)) result)
      (dolist (workspace (+workspace-list-names) (reverse result))
        (push (cons (if (string= workspace current) 'current-tab 'tab)
                    (list (cons 'name workspace)))
              result))))

  (defun my/+workspace-tab-face-fn (tab)
    (if (eq (car tab) 'current-tab)
        '+workspace-tab-selected-face
      '+workspace-tab-face))

  (defun my/+workspace-tab-name-format-fn (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat " "
               (if tab-bar-tab-hints (format "[%d] " i) "")
               (alist-get 'name tab)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        (concat " " tab-bar-close-button))
                   "")
               " ")
       'face (funcall tab-bar-tab-face-function tab))))

  (defun my/+workspace-name-to-index (&optional name)
    "Return the workspace index corresponding to its name.
NAME defaults to the current workspace name.
If NAME is not a workspace name, return nil."
    (when-let ((index (cl-position (or name (+workspace-current-name))
                                   (+workspace-list-names)
                                   :test #'string=)))
      (1+ index)))

  (defun my/+workspace-index-to-name (&optional index)
    "Return the workspace name corresponding to its 1-based index.
INDEX defaults to the current workspace index.
If INDEX is not a workspace index, return nil."
    (if (and (integerp index) (cl-plusp index))
        (elt (+workspace-list-names) (1- index))
      (+workspace-current-name)))

  :init
  (when (modulep! :ui modeline)
    (setq doom-modeline-display-misc-in-all-mode-lines nil
          doom-modeline-time nil)
    (after! doom-modeline
      (remove-hook 'display-battery-mode-hook #'doom-modeline-override-battery-modeline)
      (remove-hook 'doom-modeline-mode-hook #'doom-modeline-override-battery-modeline)
      (advice-remove #'battery-update #'doom-modeline-update-battery-status)
      (dolist (var '(doom-modeline-fn-alist doom-modeline-var-alist))
        (when-let* ((alist (eval var))
                    (element (assq 'battery alist)))
          (set var (remove element alist))))))

  :custom
  (tab-bar-tab-name-function #'+workspace-current-name)
  (tab-bar-tab-name-format-function #'my/+workspace-tab-name-format-fn)

  :config
  (setq tab-bar-tabs-function #'my/+workspace-tabs-fn
        tab-bar-tab-face-function #'my/+workspace-tab-face-fn
        tab-bar-tab-hints t
        tab-bar-new-button-show nil
        tab-bar-close-button-show nil
        tab-bar-close-button (propertize "x"
                                         'close-tab t
                                         :help "Click to close tab")
        tab-bar-format '(tab-bar-format-tabs
                         tab-bar-separator
                         tab-bar-format-add-tab
                         tab-bar-format-align-right
                         tab-bar-format-global))

  (defadvice! my/+workspace--tabline-a ()
    "Only display the current workspace name in the echo area."
    :before-until #'+workspace--tabline
    (when tab-bar-mode
      (message (+workspace-current-name))))

  (defadvice! my/+workspace--redisplay-tabs-a (&optional _count)
    "Redisplay tabs when `tab-bar-mode' is already enabled."
    :after #'+workspace/swap-left
    (when tab-bar-mode
      (tab-bar-mode +1)))

  (defadvice! my/+workspace-select-tab-a (&optional tab-number)
    :override #'tab-bar-select-tab
    (+workspace/switch-to
     (1- (or tab-number (my/+workspace-name-to-index)))))

  ;; TODO Define advice for `tab-bar-move-tab-to' (for graphical tab repositioning)
  (defadvice! my/+workspace-move-tab-to-a (to-number &optional from-number)
    :override #'tab-bar-move-tab-to
    (error "Unimplemented"))

  (defadvice! my/+workspace-new-tab-a (&optional _arg _from-number)
    :override #'tab-bar-new-tab
    (+workspace/new))

  ;; FIXME Message: "<workspace> | 'nil' workspace doesn't exist"
  (defadvice! my/+workspace-close-tab-a (&optional tab-number to-number)
    :override #'tab-bar-close-tab
    (+workspace/delete (my/+workspace-index-to-name tab-number))
    (when (and (integerp to-number) (cl-plusp to-number))
      (+workspace/switch-to (1- to-number))))

  (defadvice! my/+workspace-rename-tab-a (name &optional tab-number)
    :override #'tab-bar-rename-tab
    (if tab-number
        (warn "Unhandled argument: tab-number")
      (+workspace/rename name)))

  ;; FIXME Doesn't work quite as expected with graphical menu
  (defadvice! my/+workspace-duplicate-tab-a (&optional _arg from-number)
    :override #'tab-bar-duplicate-tab
    (when (and (integerp from-number) (cl-plusp from-number))
      (+workspace/switch-to (1- from-number)))
    (+workspace/new nil t)))

;; TODO Implement desired functionality
(use-package! tab-line
  :hook (persp-mode . global-tab-line-mode)
  :preface
  ;; Two window parameters are used to track tabs:
  ;;
  ;; `my/tab-line-tab-list': A list of buffers.
  ;; `my/tab-line-tab-index': 0-based index of currently-selected tab.
  ;;
  (add-to-list 'window-persistent-parameters '(my/tab-line-tab-list . t))
  (add-to-list 'window-persistent-parameters '(my/tab-line-tab-index . t))

  (defun my/tab-line-update-tabs-h ()
    "Update the selected window's tabs."
    (when tab-line-mode
      (let* ((window (selected-window))
             (buffer (window-buffer window))
             (tabs (window-parameter window 'my/tab-line-tab-list))
             (index (window-parameter window 'my/tab-line-tab-index)))
        (setf (nth index tabs) buffer)
        (set-window-parameter window 'my/tab-line-tab-list tabs))))

  (defun my/tab-line-tabs-fn ()
    "Return the selected window's tabs."
    (let ((window (selected-window)))
      (or (window-parameter window 'my/tab-line-tab-list)
          (prog1
            (set-window-parameter window 'my/tab-line-tab-list (list (window-buffer window)))
            (set-window-parameter window 'my/tab-line-tab-index 0)))))

  (defun my/tab-line-new-tab-choice-fn ()
    "Function to call when adding a new window tab."
    (let* ((buffer (generate-new-buffer "*new*"))
           (window (selected-window))
           (old-tabs (window-parameter window 'my/tab-line-tab-list))
           (new-tabs (set-window-parameter window 'my/tab-line-tab-list
                                           (append old-tabs (list buffer))))
           (tab-count (length new-tabs)))
      (set-window-parameter window 'my/tab-line-tab-index (1- tab-count))
      (set-window-buffer window buffer)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode)))))

  (defun my/tab-line-close-tab-fn (_tab)
    "Close the current tab."
    (let* ((window (selected-window))
           (tabs (window-parameter window 'my/tab-line-tab-list))
           (index (window-parameter window 'my/tab-line-tab-index)))
      (set-window-parameter window 'my/tab-line-tab-list
                            (delq (setcar (nthcdr index tabs)
                                          (cons nil nil))
                                  tabs))
      (set-window-parameter window 'my/tab-line-tab-index
                            (max 0 (1- (length tabs))))))

  (defun my/tab-line-tab-name-format-fn (tab tabs)
    (let* ((buffer-p (bufferp tab))
           (selected-p (if buffer-p
                           (eq tab (window-buffer))
                         (cdr (assq 'selected tab))))
           (name (if buffer-p
                     (funcall tab-line-tab-name-function tab tabs)
                   (cdr (assq 'name tab))))
           (face (if selected-p
                     (if (eq (selected-window) (old-selected-window))
                         'tab-line-tab-current
                       'tab-line-tab)
                   'tab-line-tab-inactive)))
      (dolist (fn tab-line-tab-face-functions)
        (setf face (funcall fn tab tabs face buffer-p selected-p)))
      (apply 'propertize
             (concat " "
                     (propertize name
                                 'keymap tab-line-tab-map
                                 ;; Don't turn mouse-1 into mouse-2 (bug#49247)
                                 'follow-link 'ignore)
                     (or (and (or buffer-p (assq 'buffer tab) (assq 'close tab))
                              tab-line-close-button-show
                              (not (eq tab-line-close-button-show
                                       (if selected-p 'non-selected 'selected)))
                              tab-line-close-button)
                         "")
                     " ")
             `(tab ,tab
               ,@(if selected-p '(selected t))
               face ,face
               mouse-face tab-line-highlight))))

  :custom
  (tab-line-tabs-function #'my/tab-line-tabs-fn)
  (tab-line-new-tab-choice #'my/tab-line-new-tab-choice-fn)
  (tab-line-close-tab-function #'my/tab-line-close-tab-fn)
  (tab-line-tab-name-format-function #'my/tab-line-tab-name-format-fn)

  :config
  (add-hook 'doom-switch-buffer-hook #'my/tab-line-update-tabs-h)

  (setq tab-line-new-button-show nil
        tab-line-close-button-show nil
        tab-line-switch-cycling t)

  ;; TODO Add ":tabnew"
  (map! :n "gt" #'tab-line-switch-to-next-tab
        :n "gT" #'tab-line-switch-to-prev-tab)

  (evil-define-command my/tab-line:new ()
    "Ex wrapper around `tab-line-new-tab'."
    (interactive)
    (funcall #'tab-line-new-tab))

  (evil-define-command my/tab-line:close ()
    "Ex wrapper around `tab-line-close-tab'."
    (interactive)
    (funcall #'tab-line-close-tab))

  (evil-define-command my/tab-line:next (&optional count)
    "Ex wrapper around `tab-line-switch-to-next-tab'."
    (interactive "<c>")
    (while (> count 0)
      (funcall #'tab-line-switch-to-next-tab)
      (setq count (1- count))))

  (evil-define-command my/tab-line:prev (&optional count)
    "Ex wrapper around `tab-line-switch-to-prev-tab'."
    (interactive "<c>")
    (while (> count 0)
      (funcall #'tab-line-switch-to-prev-tab)
      (setq count (1- count))))

  (evil-ex-define-cmd "tabnew"     #'my/tab-line:new)
  (evil-ex-define-cmd "tabc[lose]" #'my/tab-line:close)
  (evil-ex-define-cmd "tabn[ext]"  #'my/tab-line:next)
  (evil-ex-define-cmd "tabp[rev]"  #'my/tab-line:prev))
