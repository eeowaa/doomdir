;;; lang/kubernetes/config.el -*- lexical-binding: t; -*-


;;; Kubernetes

(use-package! k8s-mode
  :magic
  ;; TODO: Open manifest files in `k8s-mode'
  ("\\`\\(?:.*\n\\)\\{0,10\\}apiVersion:" . k8s-mode)

  :init
  ;; TODO: Automatically start LSP in `k8s-mode'
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(k8s-mode . "yaml")))
  (add-hook 'k8s-mode-local-vars-hook #'lsp 'append) ;; `lsp!' does not work well

  ;; TODO: Use the Kubernetes YAML schema in `k8s-mode'
  (add-hook 'k8s-mode-local-vars-hook
            (lambda ()
              (lsp-yaml-set-buffer-schema
               (alist-get 'url lsp-yaml--built-in-kubernetes-schema))) 'append))

(use-package! kubedoc)


;;; Helm

;; TODO: Write `use-package!' statements for `k8s-helm-mode' and `lsp-k8s-helm'
(require 'k8s-helm-mode)
(require 'lsp-k8s-helm)

(pushnew! auto-mode-alist
          '("/templates/.+\\.\\(?:ya?ml\\|tpl\\)\\'" . k8s-helm-mode))
(add-hook 'k8s-helm-mode-hook #'lsp! 0 t)

;; Recognize Helm chart directories as projects
(after! projectile
  (pushnew! projectile-project-root-files "Chart.yaml"))


;; Configure tree-sitter
(add-hook 'k8s-helm-mode-local-vars-hook #'tree-sitter! 'append)
(after! tree-sitter
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(k8s-helm-mode . gotmpl)))

;; Configure evil-textobj-tree-sitter
(after! evil-textobj-tree-sitter
  (add-to-list 'evil-textobj-tree-sitter-major-mode-language-alist
               '(k8s-helm-mode . gotmpl)))

;; Configure ts-fold
(after! ts-fold

  ;; FIXME: Comments and "else" directives
  (defun ts-fold-parsers-gotmpl ()
    "Rule sets for Go templates."
    '((_comment_action  . ts-fold-range-block-comment)
      (_pipeline_action . ts-fold-range-seq)
      (if_action        . ts-fold-range-seq)
      (range_action     . ts-fold-range-seq)
      (template_action  . ts-fold-range-seq)
      (define_action    . ts-fold-range-seq)
      (block_action     . ts-fold-range-seq)
      (with_action      . ts-fold-range-seq)))
  (customize-set-variable 'ts-fold-range-alist
                          (cons `(k8s-helm-mode . ,(ts-fold-parsers-gotmpl))
                                (assq-delete-all 'k8s-helm-mode ts-fold-range-alist)))

  (defun ts-fold-summary-gotmpl (doc-str)
    "Extract summary from DOC-STR in Go template block."
    (let ((first-line (nth 0 (split-string doc-str "\n"))))
      (string-match "\\`{-? *\\(.*?\\)\\(?: *-?}}\\)? *\\'" first-line)
      (match-string 1 first-line)))
  (customize-set-variable 'ts-fold-summary-parsers-alist
                          (cons `(k8s-helm-mode . ts-fold-summary-gotmpl)
                                ts-fold-summary-parsers-alist)))

;; Configure tree-sitter-hl
(setq-hook! 'k8s-helm-mode-hook tree-sitter-hl-use-font-lock-keywords t)

(after! tree-sitter-cli
  (defun my/tree-sitter-cli-queries-directory ()
    "Return the directory used by tree-sitter CLI to store highlight queries."
    (file-name-as-directory
     (concat (tree-sitter-cli-directory) "queries"))))

(defadvice! my/tree-sitter-langs--hl-query-path-local-a (lang-symbol &optional mode)
  "Search `tree-sitter-cli-directory' for a highlights file first."
  :before-until #'tree-sitter-langs--hl-query-path
  (when-let* ((highlights-file (concat (file-name-as-directory
                                        (concat (my/tree-sitter-cli-queries-directory)
                                                (symbol-name lang-symbol)))
                                       (if mode
                                           (format "highlights.%s.scm" mode)
                                         "highlights.scm")))
              (exists (file-exists-p highlights-file)))
    highlights-file))
