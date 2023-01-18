;;; ui/ilist/config.el -*- lexical-binding: t; -*-

;;
;;; imenu-list

(use-package! imenu-list
  :after imenu
  :init
  (define-key! doom-leader-open-map "i" #'imenu-list-minor-mode)
  (after! which-key
    (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
      (cl-pushnew `((,(format "\\`%s o i\\'" prefix-re)) nil . "Ilist")
                  which-key-replacement-alist))))
