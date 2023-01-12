;;; ui/buffer-group/+hacks.el -*- lexical-binding: t; -*-

;;
;;; Core functions

;; Don't try to resize side windows
(advice-add #'balance-windows :around #'+buffer-group-side-windows-save-a)


;;
;;; External functions

;;;###package windmove
;; Users should be able to hop into side windows easily, but Elisp shouldn't.
(defadvice! +buffer-group/ignore-window-parameters-a (fn &rest args)
  "Allow *interactive* window moving commands to traverse side windows."
  :around '(windmove-up windmove-down windmove-left windmove-right)
  (letf! (defun windmove-find-other-window (dir &optional arg window)
           (window-in-direction
            (pcase dir (`up 'above) (`down 'below) (_ dir))
            window t arg windmove-wrap-around t))
    (apply fn args)))
