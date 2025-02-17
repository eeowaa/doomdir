;;; transient-childframe.el --- Display transient windows in childframes -*- lexical-binding: t; -*-

;;; Commentary:

;; This library configures `transient' to display its windows in a childframe.
;; The `transient-posframe' package does something similar, but it is incomplete
;; and depends on `posframe'. Instead of contributing to that package, I created
;; `transient-childframe' to gain a better understanding of window and frame
;; handling in Emacs, specifically with childframes.

;;; Example:

;; (tc-display-buffer-in-child-frame
;;  (get-buffer "*scratch*")
;;  (tc-transient-childframe--display-buffer-action))

;;; Todo:

;; - Fix bugs when displaying magit popups
;; - Address REVIEW comments
;; - Remove dependency on `doom-lib'
;; - Contribute upstream fixes to `display-buffer-in-child-frame'

;;; Code:

(require 'doom-lib)
(require 'transient)


;;; Transient

(setq transient-show-popup nil
      transient-semantic-coloring t
      transient-detect-key-conflicts t
      transient-mode-line-format nil
      transient-enable-popup-navigation nil)

;; Use `SPC' to pop up the menu immediately (in addition to the default `C-t'
;; key binding); otherwise wait for `transient-show-popup' seconds.
(define-key transient-map (kbd "SPC") #'transient-show)


;;; Childframe

(setq fit-frame-to-buffer t
      frame-resize-pixelwise t
      x-gtk-resize-child-frames 'resize-mode)

(defun tc-display-buffer-in-child-frame (buffer alist)
  "A modified version of `display-buffer-in-child-frame'.
- The `share-child-frame' parameter is properly compared.
- Floating-point `top' and `left' frame parameters work as
  advertised even if the frame is resized to fit the buffer.
- Problematic frame creation hooks are ignored."
  (letf!
    ((defun tc-display-buffer-in-child-frame-fix-a (buffer alist)
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
         (letf! ((defun tc-respect-frame-offset-a (args)
                   (let* ((parameters (alist-get 'child-frame-parameters alist))
                          (left (alist-get 'left-ratio parameters))
                          (top (alist-get 'top-ratio parameters)))
                     (when (and left (floatp left) (<= 0.0 left 1.0))
                       (setf (alist-get 'left (cadr args)) left))
                     (when (and top (floatp top) (<= 0.0 top 1.0))
                       (setf (alist-get 'top (cadr args)) top))
                     args))
                 (defadvice #'modify-frame-parameters :filter-args
                            #'tc-respect-frame-offset-a))
           (prog1 (let ((window-resize-pixelwise t)
                        (frame-resize-pixelwise t)
                        (frame-inhibit-implied-resize t)
                        (x-gtk-resize-child-frames 'hide)
                        window-size-fixed)
                    (window--display-buffer buffer window type alist))
             (unless (cdr (assq 'inhibit-switch-frame alist))
               (window--maybe-raise-frame frame))))))
     (defadvice #'display-buffer-in-child-frame :override
                #'tc-display-buffer-in-child-frame-fix-a))
    (let (after-make-frame-functions
          before-make-frame-hook)
      (display-buffer-in-child-frame buffer alist))))


;;; Transient with childframe

(defun tc-transient-childframe ()
  "Return the child frame used by `transient'."
  (let ((parent (or (frame-parameter nil 'parent-frame)
                    (selected-frame))))
    (catch 'transient-childframe
      (dolist (frame (frame-list))
        (when (and (eq (frame-parameter frame 'parent-frame) parent)
                   (eq (frame-parameter frame 'share-child-frame) 'transient))
          (throw 'transient-childframe frame))))))

(defadvice! tc-transient-delete-window-a ()
  :override #'transient--delete-window
  (when-let ((transient-childframe (tc-transient-childframe))
             (transient-window (frame-selected-window transient-childframe))
             (transient-buffer (window-buffer transient-window)))
    (when (or (memq this-command '(transient-suspend
                                   transient-quit-all
                                   handle-switch-frame))
              (not transient--stack))
      (modify-frame-parameters transient-childframe ;; REVIEW
                               '((width . 0)
                                 (height . 0)
                                 (min-width . 0)
                                 (min-height . 0)
                                 (visibility)))
      (bury-buffer transient-buffer) ;; REVIEW
      (select-frame (frame-parameter transient-childframe 'parent-frame)))))

(add-hook! 'transient-exit-hook
  (defun tc-transient-debug-exit-h ()
    (when transient--debug
      (message "transient-exit-hook: this-command: %S" this-command)
      (message "transient-exit-hook: transient--stack: %S" transient--stack)
      (message "transient-exit-hook: transient--exitp: %S" transient--exitp))))

(defadvice! tc-transient--show-reduce-flicker-a (fn &rest args)
  :around #'transient--show
  (let ((inhibit-redisplay t))
    (apply fn args)))

;; REVIEW
(defadvice! tc-transient--fit-window-to-buffer-a (window)
  :override #'transient--fit-window-to-buffer
  (let ((window-resize-pixelwise t)
        (frame-resize-pixelwise t)
        (frame-inhibit-implied-resize t)
        (x-gtk-resize-child-frames 'hide)
        window-size-fixed)
    (fit-window-to-buffer window)
    (tc-transient-childframe--reposition)))

(defgroup tc-transient-childframe nil
  "Show transient buffers in childframes."
  :group 'transient)

(defcustom tc-transient-childframe-parameters nil
  "Overrides for `tc-transient-childframe-default-parameters'."
  :type '(alist :key-type symbol)
  :group 'tc-transient-childframe)

(defvar tc-transient-childframe-default-parameters
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
    (no-accept-focus . t)
    (undecorated . t)))

(defun tc-transient-childframe--reposition ()
  (let* ((parameters (append tc-transient-childframe-parameters
                             tc-transient-childframe-default-parameters))
         (left (alist-get 'left parameters))
         (top (alist-get 'top parameters))
         window-resize-pixelwise
         frame-resize-pixelwise
         frame-inhibit-implied-resize
         window-size-fixed)
  (modify-frame-parameters (tc-transient-childframe)
                           `((left . ,left) (top . ,top)))))

(defun tc-transient-childframe--display-buffer-action ()
  "Construct a display buffer action for `transient'.

The action function is `tc-display-buffer-in-child-frame', which
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
creating a frame."
  (let* ((parameters (append tc-transient-childframe-parameters
                             tc-transient-childframe-default-parameters))
         (left (alist-get 'left parameters))
         (top (alist-get 'top parameters)))
    (when (and left (floatp left) (<= 0.0 left 1.0))
      (push `(left-ratio . ,left) parameters))
    (when (and top (floatp top) (<= 0.0 top 1.0))
      (push `(top-ratio . ,top) parameters))
    (cons #'tc-display-buffer-in-child-frame
          `((window-size . fit-window-to-buffer)
            (window-parameters
             (mode-line-format . none)
             (header-line-format . none)
             (tab-line-format . none))
            ;; REVIEW `quit-restore'
            (child-frame-parameters . ,parameters)))))

(setq transient-display-buffer-action
      (tc-transient-childframe--display-buffer-action))


(provide 'transient-childframe)
;;; transient-childframe.el ends here
