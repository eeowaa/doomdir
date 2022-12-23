;;; lang/kubernetes/config.el -*- lexical-binding: t; -*-


;;; Kubernetes

(use-package! kubedoc
  :defer t)

(use-package! k8s-mode
  :defer t
  :preface
  (defvar +kubernetes-k8s-mode-match-files
    '("\\.ya?ml$" (not . "/templates/\\|\\(?:chart\\.ya?ml$\\)"))
    "List of file name regexps to check for `k8s-mode' activation.
Each element looks like REGEXP or (not . REGEXP), respectively
describing positive and negative regexp patterns on file names.
Elements in other forms are silently ignored. The regexps are matched
case-insensitively.

If a newly-opened file-visiting buffer has any positive regexp
matches against its file name, the negative regexps are checked.
If there are no negative matches, `+kubernetes--k8s-mode-match-fn'
will proceed with its checks against the buffer content.")

  (defvar +kubernetes-k8s-mode-match-lines 10)
  (defvar +kubernetes-k8s-mode-match-regexp "^apiVersion:"
    "File content regexp to check for `k8s-mode' activation.
If a new file-visiting buffer meets the conditions imposed by
`+kubernetes-k8s-mode-match-files', the first `+kubernetes-k8s-mode-match-lines'
lines of the buffer are checked against this regexp. If there is a match,
`k8s-mode' is activated in the buffer.")

  (defun +kubernetes--k8s-mode-search-lines (line-count)
    (goto-char (bol))
    (and (cl-plusp line-count)
         (or (looking-at-p +kubernetes-k8s-mode-match-regexp)
             (and (line-move 1 t)
                  (+kubernetes--k8s-mode-search-lines (1- line-count))))))

  (defun +kubernetes--k8s-mode-match-fn ()
    (let* ((file-name (buffer-file-name))
           (file-name-string-match-p
            (lambda (re)
              (let ((case-fold-search t))
                (string-match-p re file-name))))
           positives negatives)
      (dolist (element +kubernetes-k8s-mode-match-files)
        (cond
         ((stringp element)
          (push element positives))
         ((and (eq 'not (car-safe element))
               (stringp (cdr element)))
          (push (cdr element) negatives))))
      (and
       (cl-some file-name-string-match-p positives)
       (cl-notany file-name-string-match-p negatives)
       (save-excursion
         (goto-char 1)
         (+kubernetes--k8s-mode-search-lines +kubernetes-k8s-mode-match-lines)))))

  (when (modulep! +lsp)
    (defun +kubernetes--k8s-mode-set-yaml-schema-h ()
      (let ((buffer (current-buffer)))
        (with-eval-after-load 'lsp-yaml
          (with-current-buffer buffer
            ;; REVIEW The `lsp-yaml-schemas' variable associates YAML schemas to file
            ;; paths relative to an LSP workspace directory, but the variable is shared
            ;; by all workspaces. Therefore, it seems plausible that `lsp-yaml' would
            ;; incorrect apply the schema of a YAML file opened in one workspace to all
            ;; subsequently-opened YAML files sharing the same relative path in other
            ;; workspaces, unless `lsp-yaml-set-buffer-schema' is explicitly called for
            ;; the new buffers. I need to verify this behavior.
            (lsp-yaml-set-buffer-schema
             (alist-get 'url lsp-yaml--built-in-kubernetes-schema))
            (remove-hook 'lsp-mode-hook #'+kubernetes--k8s-mode-set-yaml-schema-h t)))))

    (defun +kubernetes--k8s-mode-lsp-init-h ()
      (add-hook 'lsp-mode-hook #'+kubernetes--k8s-mode-set-yaml-schema-h 'append t)
      (lsp!)))

  :init
  (add-to-list 'magic-mode-alist '(+kubernetes--k8s-mode-match-fn . k8s-mode))
  (when (modulep! +lsp)
    (add-hook 'k8s-mode-local-vars-hook #'+kubernetes--k8s-mode-lsp-init-h 'append)))


;;; Helm

(require 'kubernetes-helm-mode)
(pushnew! auto-mode-alist
          '("/templates/.+\\.\\(?:ya?ml\\|tpl\\)\\'" . kubernetes-helm-mode))

;;; Helm: projectile

(after! projectile
  (pushnew! projectile-project-root-files "Chart.yaml"))

;;; Helm: lsp-mode

(when (and (modulep! +lsp)
           (modulep! :tools lsp)
           (not (modulep! :tools lsp +eglot)))
  (require 'lsp-kubernetes-helm)
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
    (setq! ts-fold-range-alist
           (cons `(kubernetes-helm-mode . ,(ts-fold-parsers-gotmpl))
                 (assq-delete-all 'kubernetes-helm-mode ts-fold-range-alist)))
    (defun ts-fold-summary-gotmpl (doc-str)
      "Extract summary from DOC-STR in Go template block."
      (let ((first-line (nth 0 (split-string doc-str "\n"))))
        (string-match "\\`{-? *\\(.*?\\)\\(?: *-?}}\\)? *\\'" first-line)
        (match-string 1 first-line)))
    (setq! ts-fold-summary-parsers-alist
           (cons `(kubernetes-helm-mode . ts-fold-summary-gotmpl)
                 ts-fold-summary-parsers-alist)))

  ;;; Helm: evil-textobj-tree-sitter

  (after! evil-textobj-tree-sitter
    (add-to-list 'evil-textobj-tree-sitter-major-mode-language-alist
                 '(kubernetes-helm-mode . gotmpl))))
