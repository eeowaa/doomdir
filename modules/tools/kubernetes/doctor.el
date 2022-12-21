;;; lang/kubernetes/doctor.el -*- lexical-binding: t; -*-

(if (modulep! +tree-sitter)
    (assert! (modulep! :tools tree-sitter)
             "This module requires (:tools tree-sitter)")
  (warn! "The +tree-sitter flag is required for most Helm chart features"))

(assert! (or (not (modulep! +lsp))
             (and (modulep! :tools lsp)
                  (not (modulep! :tools lsp +eglot))))
         "This module requires (:tools tree-sitter) without the +eglot flag")
