;;; ui/buffer-group/autoload/buffer-group.el -*- lexical-binding: t; -*-

(defun +buffer-group--window-next-siblings (window)
  "Return list of WINDOW's next siblings."
  (when-let ((next (window-next-sibling window)))
    (cons next (+buffer-group--window-next-siblings next))))


;;
;;; Public library

;;;###autoload
(defun +buffer-group-side-window-p (window)
  "Return non-nil if WINDOW is a side window."
  (and (window-live-p window)
       (window-parameter window 'window-side)))

;;;###autoload
(defun +buffer-group-side-windows-active-p (frame)
  "Return non-nil if FRAME contains side windows."
  (window-with-parameter 'window-side nil frame))

;;;###autoload
(defun +buffer-group-side-windows (frame)
  "Return list of side windows in FRAME."
  (let (result)
    (dolist (window (window-list frame) result)
      (when (window-parameter window 'window-side)
        (push window result)))
    result))

;;;###autoload
(defun +buffer-group-window-children (window)
  "Return list of WINDOW's direct children."
  (let ((head (window-child window)))
    (cons head (+buffer-group--window-next-siblings head))))

;;;###autoload
(defun +buffer-group-window-sibling-group (window)
  "Return a list including WINDOW and all its siblings."
  (+buffer-group-window-children (window-parent window)))


;;
;;; Commands

;;;###autoload
(defun +buffer-group/side-windows-hide (frame)
  "Hide side windows in FRAME."
  (when (+buffer-group-side-windows-active-p frame)
    (window-toggle-side-windows frame)))

;;;###autoload
(defun +buffer-group/side-windows-show (frame)
  "Show side windows in FRAME."
  (unless (+buffer-group-side-windows-active-p frame)
    (window-toggle-side-windows frame)))


;;
;;; Advice

;;;###autoload
;; To be used in similar places as Doom's `+popup-save-a'
(defun +buffer-group-side-windows-save-a (fn &rest args)
  "Close side windows, execute FN with ARGS, then restore side windows."
  (let* ((window-or-frame (car-safe args))
         (window (when (windowp window-or-frame)
                   window-or-frame))
         (frame (cond ((framep window-or-frame) window-or-frame)
                      (window (window-frame window))
                      (t (selected-frame)))))
    (if (not (+buffer-group-side-windows-active-p frame))
        (apply fn args)
      ;; NOTE For whatever reason, `save-excursion' does not work here.
      (let ((restorep (window-parameter nil 'window-side))
            (buffer (current-buffer)))
        ;; XXX If the first element in `args' is a window, we need to ensure
        ;; that `fn' is given an argument that refers to a window that exists
        ;; after hiding the side windows. This window may or may not refer to
        ;; the same window! Useful when advising `balance-windows'.
        (if window
            (let ((child (-first
                          (lambda (window) (not (+buffer-group-side-window-p window)))
                          (+buffer-group-window-children window))))
              (window-toggle-side-windows frame)
              (apply fn (cons (window-parent child) (cdr args)))
              (window-toggle-side-windows frame))
          (window-toggle-side-windows frame)
          (apply fn args)
          (window-toggle-side-windows frame))
        (when restorep
          (pop-to-buffer buffer '((display-buffer-reuse-window) . nil)))))))
