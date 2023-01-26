;;; ui/tabs/config.el -*- lexical-binding: t; -*-

;;
;;; tab-bar

(use-package! tab-bar
  :unless noninteractive
  :when (modulep! :ui workspaces)
  :hook (persp-mode . tab-bar-mode)
  :preface
  (defun +tabs-workspace-tabs-fn ()
    (let ((current (+workspace-current-name)) result)
      (dolist (workspace (+workspace-list-names) (reverse result))
        (push (cons (if (string= workspace current) 'current-tab 'tab)
                    (list (cons 'name workspace)))
              result))))

  (defun +tabs-workspace-tab-face-fn (tab)
    (if (eq (car tab) 'current-tab)
        '+workspace-tab-selected-face
      '+workspace-tab-face))

  (defun +tabs-workspace-tab-name-format-fn (tab i)
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

  (defun +tabs--workspace-name-to-index (&optional name)
    "Return the workspace index corresponding to its name.
NAME defaults to the current workspace name.
If NAME is not a workspace name, return nil."
    (when-let ((index (cl-position (or name (+workspace-current-name))
                                   (+workspace-list-names)
                                   :test #'string=)))
      (1+ index)))

  (defun +tabs--workspace-index-to-name (&optional index)
    "Return the workspace name corresponding to its 1-based index.
INDEX defaults to the current workspace index.
If INDEX is not a workspace index, return nil."
    (if (and (integerp index) (cl-plusp index))
        (elt (+workspace-list-names) (1- index))
      (+workspace-current-name)))

  :init
  (when (and (modulep! :ui modeline) initial-window-system)
    (setq doom-modeline-display-misc-in-all-mode-lines nil
          doom-modeline-time nil
          doom-modeline-battery nil)
    (after! doom-modeline
      ;; Battery
      (advice-remove #'battery-update #'doom-modeline-update-battery-status)
      (remove-hook! '(display-battery-mode-hook doom-modeline-mode-hook)
        #'doom-modeline-override-battery-modeline)

      ;; Time
      (remove-hook! '(display-time-mode-hook doom-modeline-mode-hook)
        #'doom-modeline-override-display-time-modeline)

      ;; Battery and Time
      (dolist (var '(doom-modeline-fn-alist doom-modeline-var-alist))
        (when-let* ((alist (eval var))
                    (element (or (assq 'battery alist)
                                 (assq 'time alist))))
          (set var (remove element alist))))))

  :custom
  (tab-bar-tab-name-function #'+workspace-current-name)
  (tab-bar-tab-name-format-function #'+tabs-workspace-tab-name-format-fn)

  :config
  (when initial-window-system
    (display-time-mode +1)
    (display-battery-mode +1))

  (setq tab-bar-tabs-function #'+tabs-workspace-tabs-fn
        tab-bar-tab-face-function #'+tabs-workspace-tab-face-fn
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

  (defadvice! +tabs-workspace-tabline-a ()
    "Only display the current workspace name in the echo area."
    :before-until #'+workspace--tabline
    (when tab-bar-mode
      (message (+workspace-current-name))))

  (defadvice! +tabs-workspace-redisplay-tabs-a (&optional _count)
    "Redisplay tabs when `tab-bar-mode' is already enabled."
    :after #'+workspace/swap-left
    (when tab-bar-mode
      (tab-bar-mode +1)))

  (defadvice! +tabs-workspace-select-tab-a (&optional tab-number)
    :override #'tab-bar-select-tab
    (+workspace/switch-to
     (1- (or tab-number (+tabs--workspace-name-to-index)))))

  ;; TODO Define advice for `tab-bar-move-tab-to' (for graphical tab repositioning)
  (defadvice! +tabs-workspace-move-tab-to-a (_to-number &optional _from-number)
    :override #'tab-bar-move-tab-to
    (error "Unimplemented"))

  (defadvice! +tabs-workspace-new-tab-a (&optional _arg _from-number)
    :override #'tab-bar-new-tab
    (+workspace/new))

  ;; FIXME Message: "<workspace> | 'nil' workspace doesn't exist"
  (defadvice! +tabs-workspace-close-tab-a (&optional tab-number to-number)
    :override #'tab-bar-close-tab
    (+workspace/delete (+tabs--workspace-index-to-name tab-number))
    (when (and (integerp to-number) (cl-plusp to-number))
      (+workspace/switch-to (1- to-number))))

  (defadvice! +tabs-workspace-rename-tab-a (name &optional tab-number)
    :override #'tab-bar-rename-tab
    (if tab-number
        (warn "Unhandled argument: tab-number")
      (+workspace/rename name)))

  ;; FIXME Doesn't work quite as expected with graphical menu
  (defadvice! +tabs-workspace-duplicate-tab-a (&optional _arg from-number)
    :override #'tab-bar-duplicate-tab
    (when (and (integerp from-number) (cl-plusp from-number))
      (+workspace/switch-to (1- from-number)))
    (+workspace/new nil t)))

;;
;;; vimish-tab

;; TODO Implement desired functionality
(use-package! vimish-tab
  :unless noninteractive
  :hook (doom-init-ui . global-vimish-tab-mode)
  :config
  (defun +tabs-project-scratch-buffer-fn ()
    "Return the current project scratch buffer."
    (save-window-excursion
      (doom-scratch-buffer
       nil
       (cond ((eq doom-scratch-initial-major-mode t)
                    (unless (or buffer-read-only
                                (derived-mode-p 'special-mode)
                                (string-match-p "^ ?\\*" (buffer-name)))
                      major-mode))
                   ((null doom-scratch-initial-major-mode)
                    nil)
                   ((symbolp doom-scratch-initial-major-mode)
                    doom-scratch-initial-major-mode))
       default-directory
       (doom-project-name))))

  (setq vimish-tab-default-buffer #'+tabs-project-scratch-buffer-fn
        vimish-tab-new-button-show nil
        vimish-tab-close-button-show nil
        vimish-tab-switch-cycling t)

  (if (not (modulep! :ui workspaces))
      (setq vimish-tab-new-buffer-function #'vimish-tab-select-buffer
            vimish-tab-close-window-function #'delete-window)
    (setq vimish-tab-new-buffer-function #'vimish-tab-persp-buffer
          vimish-tab-close-window-function #'+workspace/close-window-or-workspace)
    (after! persp-mode
      (define-key! persp-mode-map
        [remap delete-window] #'vimish-tab-close-tab-or-window
        [remap evil-window-delete] #'vimish-tab-close-tab-or-window))))
