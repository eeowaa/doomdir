;;; lang/kubernetes/config.el -*- lexical-binding: t; -*-


;;; Kubernetes

(use-package! kubedoc)
(use-package! k8s-mode
  :magic

  ("\\`\\(?:.*\n\\)\\{0,10\\}apiVersion:" . k8s-mode)

  :init

  ;;; Kubernetes: lsp-mode

  (when (modulep! +lsp)
    (after! lsp-mode
      (add-hook! 'k8s-mode-local-vars-hook :append
        (list
         ;; NOTE: Doom's `lsp!' does not work well here
         `(#'lsp
           ,(defun my/k8s-mode-set-yaml-schema ()
              "Enable the built-in Kubernetes YAML schema."
              (lsp-yaml-set-buffer-schema
               (alist-get 'url lsp-yaml--built-in-kubernetes-schema)))))))))


;;; Helm

(load! "package/kubernetes-helm-mode.el")
(pushnew! auto-mode-alist
          '("/templates/.+\\.\\(?:ya?ml\\|tpl\\)\\'" . kubernetes-helm-mode))

;;; Helm: projectile

(after! projectile
  (pushnew! projectile-project-root-files "Chart.yaml"))

;;; Helm: lsp-mode

(when (and (modulep! +lsp)
           (modulep! :tools lsp)
           (not (modulep! :tools lsp +eglot)))
  (load! "staging/lsp-kubernetes-helm.el")
  (add-hook 'kubernetes-helm-mode-hook #'lsp! 0 t))

;;; Helm: tree-sitter

(when (modulep! +tree-sitter)

  (add-hook 'kubernetes-helm-mode-local-vars-hook #'tree-sitter! 'append)
  (after! tree-sitter
    (add-to-list 'tree-sitter-major-mode-language-alist
                 '(kubernetes-helm-mode . gotmpl)))

  ;;; Helm: tree-sitter-hl

  (setq-hook! 'kubernetes-helm-mode-hook tree-sitter-hl-use-font-lock-keywords t)

  (after! tree-sitter-cli
    (defvar +tree-sitter-hl-queries-dir
      (file-name-as-directory (concat (tree-sitter-cli-directory) "queries"))
      "The directory used by the tree-sitter CLI to store highlight queries."))

  (defadvice! my/tree-sitter-langs--hl-query-path-local-a (lang-symbol &optional mode)
    "Search `tree-sitter-cli-directory' for a highlights file first."
    :before-until #'tree-sitter-langs--hl-query-path
    (when-let* ((highlights-file (concat (file-name-as-directory
                                          (concat +tree-sitter-hl-queries-dir
                                                  (symbol-name lang-symbol)))
                                         (if mode
                                             (format "highlights.%s.scm" mode)
                                           "highlights.scm")))
                (exists (file-exists-p highlights-file)))
      highlights-file))

  ;;; Helm: ts-fold

  (after! ts-fold
    (defun ts-fold-parsers-gotmpl ()
      "Rule sets for Go templates."
      ;; FIXME: Comments and "else" directives
      '((_comment_action  . ts-fold-range-block-comment)
        (_pipeline_action . ts-fold-range-seq)
        (if_action        . ts-fold-range-seq)
        (range_action     . ts-fold-range-seq)
        (template_action  . ts-fold-range-seq)
        (define_action    . ts-fold-range-seq)
        (block_action     . ts-fold-range-seq)
        (with_action      . ts-fold-range-seq)))
    (customize-set-variable 'ts-fold-range-alist
                            (cons `(kubernetes-helm-mode . ,(ts-fold-parsers-gotmpl))
                                  (assq-delete-all 'kubernetes-helm-mode ts-fold-range-alist)))
    (defun ts-fold-summary-gotmpl (doc-str)
      "Extract summary from DOC-STR in Go template block."
      (let ((first-line (nth 0 (split-string doc-str "\n"))))
        (string-match "\\`{-? *\\(.*?\\)\\(?: *-?}}\\)? *\\'" first-line)
        (match-string 1 first-line)))
    (customize-set-variable 'ts-fold-summary-parsers-alist
                            (cons `(kubernetes-helm-mode . ts-fold-summary-gotmpl)
                                  ts-fold-summary-parsers-alist)))

  ;;; Helm: evil-textobj-tree-sitter

  (after! evil-textobj-tree-sitter
    (add-to-list 'evil-textobj-tree-sitter-major-mode-language-alist
                 '(kubernetes-helm-mode . gotmpl))))
