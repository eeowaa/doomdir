;;; transient-childframe.el -*- lexical-binding: t; -*-


;;; Childframe

(setq fit-frame-to-buffer t
      frame-resize-pixelwise t
      x-gtk-resize-child-frames 'resize-mode)

(defun my/display-buffer-in-child-frame (buffer alist)
  "A modified version of `display-buffer-in-child-frame'.
- The `share-child-frame' parameter is properly compared.
- Floating-point `top' and `left' frame parameters work as
  advertised even if the frame is resized to fit the buffer."
  (letf!
    ((defadvice my/display-buffer-in-child-frame-fix-a (buffer alist)
       :override #'display-buffer-in-child-frame
       (let* ((parameters (append (alist-get 'child-frame-parameters alist)
                                  `((parent-frame . ,(selected-frame)))))
              (parent (or (alist-get 'parent-frame parameters)
                          (selected-frame)))
              (share (alist-get 'share-child-frame parameters))
              share1 frame window type)
         (with-current-buffer buffer
           (when (frame-live-p parent)
             (catch 'frame
               (dolist (frame1 (frame-list))
                 (when (eq (frame-parent frame1) parent)
                   (setq share1 (alist-get 'share-child-frame
                                           (frame-parameters frame1)))
                   (when (eq share share1)
                     (setq frame frame1)
                     (throw 'frame t))))))
           (if frame
               (progn
                 (setq window (frame-selected-window frame))
                 ;(setq type 'reuse)) ;; REVIEW
                 (setq type 'frame))
             (setq frame (make-frame parameters))
             (setq window (frame-selected-window frame))
             (setq type 'frame))) ;; REVIEW
         (letf! ((defadvice my/respect-frame-offset-a (args)
                   :filter-args #'modify-frame-parameters
                   (let* ((parameters (alist-get 'child-frame-parameters alist))
                          (left (alist-get 'left-ratio parameters))
                          (top (alist-get 'top-ratio parameters)))
                     (when (and left (floatp left) (<= 0.0 left 1.0))
                       (setf (alist-get 'left (cadr args)) left))
                     (when (and top (floatp top) (<= 0.0 top 1.0))
                       (setf (alist-get 'top (cadr args)) top))
                     args)))
           (prog1 (let ((window-resize-pixelwise t)
                        (frame-resize-pixelwise t)
                        (frame-inhibit-implied-resize t)
                        (x-gtk-resize-child-frames 'hide)
                        window-size-fixed)
                    (window--display-buffer buffer window type alist))
             (unless (cdr (assq 'inhibit-switch-frame alist))
               (window--maybe-raise-frame frame)))))))
    (display-buffer-in-child-frame buffer alist)))


;;; Transient

(defun my/transient-childframe ()
  "Return the child frame used by `transient'."
  (let ((parent (or (frame-parameter nil 'parent-frame)
                    (selected-frame))))
    (catch 'transient-childframe
      (dolist (frame (frame-list))
        (when (and (eq (frame-parameter frame 'parent-frame) parent)
                   (eq (frame-parameter frame 'share-child-frame) 'transient))
          (throw 'transient-childframe frame))))))

(defadvice! my/transient-delete-window-a ()
  :override #'transient--delete-window
  (when-let ((transient-childframe (my/transient-childframe))
             (transient-window (frame-selected-window transient-childframe))
             (transient-buffer (window-buffer transient-window)))
    (when (or (memq this-command '(transient-suspend transient-quit-all))
              (not transient--stack))
      (modify-frame-parameters transient-childframe ;; REVIEW
                               '((width . 0)
                                 (height . 0)
                                 (min-width . 0)
                                 (min-height . 0)
                                 (visibility)))
      (bury-buffer transient-buffer) ;; REVIEW
      (select-frame (frame-parameter transient-childframe 'parent-frame)))))

(defadvice! my/transient--show-reduce-flicker-a (fn &rest args)
  :around #'transient--show
  (let ((inhibit-redisplay t))
    (apply fn args)))

;; REVIEW
(defadvice! my/transient--fit-window-to-buffer-a (window)
  :override #'transient--fit-window-to-buffer
  (let ((window-resize-pixelwise t)
        (frame-resize-pixelwise t)
        (frame-inhibit-implied-resize t)
        (x-gtk-resize-child-frames 'hide)
        window-size-fixed)
    (fit-window-to-buffer window)
    (my/transient-childframe--reposition)))

(defgroup my/transient-childframe nil
  "Show transient buffers in childframes."
  :group 'transient)

(defcustom my/transient-childframe-parameters nil
  "Overrides for `my/transient-childframe-default-parameters'."
  :type '(alist :key-type symbol)
  :group 'my/transient-childframe)

(defvar my/transient-childframe-default-parameters
  '((share-child-frame . transient)

    ;; Position
    (left . 0.5)
    (top . 0.5)
    (keep-ratio . (nil . t))
    (keep-ratio . t)
    (user-position . t)

    ;; Size
    (width . 0)
    (height . 0)
    (min-width . 0)
    (min-height . 0)

    ;; Layout
    (child-frame-border-width . 8)
    (vertical-scroll-bars)
    (horizontal-scroll-bars)
    (left-fringe . 8)
    (right-fringe . 8)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (tab-bar-lines-keep-state . t)
    (no-special-glyphs . t)

    ;; Buffer
    (minibuffer)
    (unsplittable . t)

    ;; Frame Internation
    (no-other-frame . t)
    (auto-hide-function . make-frame-invisible)

    ;; Window Management
    (undecorated . t)))

(defun my/transient-childframe--reposition ()
  (let* ((parameters (append my/transient-childframe-parameters
                             my/transient-childframe-default-parameters))
         (left (alist-get 'left parameters))
         (top (alist-get 'top parameters))
         window-resize-pixelwise
         frame-resize-pixelwise
         frame-inhibit-implied-resize
         window-size-fixed)
  (modify-frame-parameters (my/transient-childframe)
                           `((left . ,left) (top . ,top)))))

(defun my/transient-childframe--display-buffer-action ()
  "Construct a display buffer action for `transient'.

The action function is `my/display-buffer-in-child-frame', which
causes the transient buffer to appear in a child frame,
potentially reusing an existing child frame.

The `window-size' alist entry is set to `fit-window-to-buffer',
which will resize the childframe to fit the contents of the
buffer when `fit-frame-to-buffer' is non-nil.

If the `left' and `top' entries of `child-frame-parameters' are
floating point values between 0.0 and 1.0, they are assigned to
additional entries `left-ratio' and `top-ratio', to be used by
advice to reposition the frame appropriately. This is required
because Emacs converts `left' and `top' to integers after
creating a frame.
"
  (let* ((parameters (append my/transient-childframe-parameters
                             my/transient-childframe-default-parameters))
         (left (alist-get 'left parameters))
         (top (alist-get 'top parameters)))
    (when (and left (floatp left) (<= 0.0 left 1.0))
      (push `(left-ratio . ,left) parameters))
    (when (and top (floatp top) (<= 0.0 top 1.0))
      (push `(top-ratio . ,top) parameters))
    (cons #'my/display-buffer-in-child-frame
          `((window-size . fit-window-to-buffer)
            (window-parameters
             (mode-line-format . none)
             (header-line-format . none)
             (tab-line-format . none))
            ;; REVIEW `quit-restore'
            (child-frame-parameters . ,parameters)))))

(setq transient-mode-line-format nil
      transient-enable-popup-navigation nil
      transient-display-buffer-action (my/transient-childframe--display-buffer-action))


;;; Tests

;; (my/display-buffer-in-child-frame
;;  (get-buffer doom-fallback-buffer-name)
;;  (my/transient-childframe--display-buffer-action))
