;;; lang/kubernetes/doctor.el -*- lexical-binding: t; -*-

;; For Helm template syntax highlighting and folding

;; TODO: Use go-template mode as the basis of k8s-helm-mode to get syntax
;; highlighting without `tree-sitter'
(assert! (modulep! :tools tree-sitter)
         "This module requires (:tools tree-sitter)")
