;;; tools/kubernetes/config.el -*- lexical-binding: t; -*-


;;; Kubedoc

(use-package! kubedoc
  :commands kubedoc--view-resource
  :init
  (defun +kubernetes/kubedoc-current ()
    "Describe the Kubernetes resource of the current YAML document."
    (interactive)
    (save-excursion
      (let ((pos (point))
            (end (re-search-forward "^---" (point-max) 'ignore)))
        (goto-char pos)
        (re-search-backward "^---" (point-min) 'ignore)
        (re-search-forward "^[[:space:]]*kind:[[:space:]]+\\(.+\\)" end t)))
    (kubedoc--view-resource (match-string 1)))

  (defun +kubernetes/kubedoc-yank-resource-path ()
    "Yank the canonical path of the current resource in `kubedoc'."
    (interactive)
    (cl-assert (eq major-mode 'kubedoc-mode))
    (kill-new
     (substring-no-properties
      (apply #'string-join
             `(,(buffer-local-value 'kubedoc--buffer-path (current-buffer)) ".")))))

  :config
  (defadvice! +kubernetes-kubedoc-up-message-a ()
    :after-until '(kubedoc-up kubedoc-top)
    (message "Already at a top-level resource."))

  (when (modulep! :editor evil)
    (defadvice! +kubernetes-kubedoc-inhibit-view-mode-a (fn &rest args)
      :around #'kubedoc-mode
      (letf! ((#'view-mode #'ignore))
        (apply fn args)))
    (map! :map kubedoc-mode-map
          :n "u"  #'kubedoc-up
          :n "t"  #'kubedoc-top
          :n "gm" #'kubedoc
          (:localleader
           :desc "Yank resource path" "y" #'+kubernetes/kubedoc-yank-resource-path))
    (when (modulep! :completion vertico)
      (map! :map kubedoc-mode-map
            :localleader
            :desc "Jump to field" "."  #'consult-imenu))))


;;; Kubernetes

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
will proceed with its checks against the buffer content.

The `+kubernetes-helm-output-dir' variable overrides the negative
regexps. That is, if a positively-matched file resides in the
directory specified by `+kubernetes-helm-output-dir', negative
regexps are not checked.")

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
    (when-let* ((file-name (buffer-file-name)))
      (let ((file-name-string-match-p
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
         (or (and (not (string-empty-p +kubernetes-helm-output-dir))
                  (string-prefix-p (file-name-as-directory +kubernetes-helm-output-dir)
                                   file-name))
             (cl-notany file-name-string-match-p negatives))
         (save-excursion
           (goto-char 1)
           (+kubernetes--k8s-mode-search-lines +kubernetes-k8s-mode-match-lines))))))

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

  :config
  (when (modulep! +lsp)
    (add-hook 'k8s-mode-local-vars-hook #'+kubernetes--k8s-mode-lsp-init-h 'append))

  (when (modulep! :editor fold)
    (after! hideshow
      (push (cons 'k8s-mode (alist-get 'yaml-mode hs-special-modes-alist))
            hs-special-modes-alist)))

  (map! :map k8s-mode-map
        :localleader
        :desc "explain" "e" #'+kubernetes/kubedoc-current))


;;; Helm

(use-package! kubernetes-helm-mode
  :defer t
  :preface
  (defvar +kubernetes-helm-executable "helm"
    "Path to helm executable.")
  (defvar +kubernetes-helm-output-dir
    (let ((helm-env (shell-command-to-string (format "%s env" +kubernetes-helm-executable))))
      (when (string-match "^HELM_CACHE_HOME=\"\\(.*\\)\"$" helm-env)
        (concat (file-name-as-directory (match-string 1 helm-env)) "output")))
    "Output directory for `helm template'")

  (when (modulep! +tree-sitter)
    (require 'tree-sitter-cli)
    (defvar +kubernetes--tree-sitter-hl-queries-dir
            (file-name-as-directory (concat (tree-sitter-cli-directory) "queries"))
            "The directory used by the tree-sitter CLI to store highlight queries."))

  :init
  ;; REVIEW The `use-package!' declaration for `web-mode' adds an entry to
  ;; `auto-mode-alist' that conflicts with ours. We must wait for that entry to
  ;; be present in the list before we add our own entry. `defer-until!' works,
  ;; but I don't know if that is the best method. For one, if the entry's regexp
  ;; changes, this workaround will break.
  (when (modulep! :lang web)
    (defer-until! (member '("\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'" . web-mode) auto-mode-alist)
      (add-to-list 'auto-mode-alist '("/templates/.+\\.\\(?:ya?ml\\|tpl\\)\\'" . kubernetes-helm-mode))))

  (add-to-list 'auto-mode-alist '("/Chart\\.lock\\'" . yaml-mode))
  (when (modulep! +lsp)
    ;; NOTE `lsp-yaml--get-supported-schemas' returns a list with the following
    ;; entry, so automatic YAML schema selection *should* work, but it doesn't:
    ;; '((name . "Helm Chart.lock")
    ;;   (description . "The Chart.lock file locks dependencies from Chart.yaml")
    ;;   (fileMatch . ["Chart.lock"])
    ;;   (url . "https://json.schemastore.org/chart-lock.json"))
    (with-eval-after-load 'lsp-yaml
      (let* ((schema (cl-find "Helm Chart.lock"
                              (lsp-yaml--get-supported-schemas)
                              :test (lambda (name item)
                                      (string= name (alist-get 'name item)))))
             (uri-string (alist-get 'url schema))
             (uri (intern uri-string)))
        (add-to-list 'lsp-yaml-schemas `(,uri . ["/Chart.lock"])))))

  (after! projectile
    (add-to-list 'projectile-project-root-files "Chart.yaml")
    (add-to-list 'projectile-project-search-path
                 `(,(concat (file-name-as-directory (xdg-cache-home)) "helm/repository") . 1)))

  :config
  (when (modulep! +lsp)
    (add-hook 'kubernetes-helm-mode-local-vars-hook #'lsp! 'append)
    (after! lsp-mode (require 'lsp-kubernetes-helm)))

  (when (modulep! +tree-sitter)
    (add-hook 'kubernetes-helm-mode-local-vars-hook #'tree-sitter! 'append)
    (setq-hook! 'kubernetes-helm-mode-hook tree-sitter-hl-use-font-lock-keywords t)
    (after! tree-sitter
      (add-to-list 'tree-sitter-major-mode-language-alist
                   '(kubernetes-helm-mode . gotmpl)))

    ;; tree-sitter-hl
    (defadvice! +kubernetes--tree-sitter-hl-local-maybe-a (lang-symbol &optional mode)
      "Search `tree-sitter-cli-directory' for a highlights file first."
      :before-until #'tree-sitter-langs--hl-query-path
      (when-let*
          ((highlights-file (concat (file-name-as-directory
                                     (concat +kubernetes--tree-sitter-hl-queries-dir
                                             (symbol-name lang-symbol)))
                                    (if mode
                                        (format "highlights.%s.scm" mode)
                                      "highlights.scm")))
           (exists (file-exists-p highlights-file)))
        highlights-file))

    ;; ts-fold
    (after! ts-fold
      (defun +kubernetes--ts-fold-parsers-gotmpl ()
        "Rule sets for Go templates."
        ;; FIXME Comments and "else" directives
        '((_comment_action  . ts-fold-range-block-comment)
          (_pipeline_action . ts-fold-range-seq)
          (if_action        . ts-fold-range-seq)
          (range_action     . ts-fold-range-seq)
          (template_action  . ts-fold-range-seq)
          (define_action    . ts-fold-range-seq)
          (block_action     . ts-fold-range-seq)
          (with_action      . ts-fold-range-seq)))
      (setq! ts-fold-range-alist
             (cons `(kubernetes-helm-mode . ,(+kubernetes--ts-fold-parsers-gotmpl))
                   (assq-delete-all 'kubernetes-helm-mode ts-fold-range-alist)))
      (defun +kubernetes--ts-fold-summary-gotmpl (doc-str)
        "Extract summary from DOC-STR in Go template block."
        (let ((first-line (nth 0 (split-string doc-str "\n"))))
          (string-match "\\`{-? *\\(.*?\\)\\(?: *-?}}\\)? *\\'" first-line)
          (match-string 1 first-line)))
      (setq! ts-fold-summary-parsers-alist
             (cons `(kubernetes-helm-mode . +kubernetes--ts-fold-summary-gotmpl)
                   ts-fold-summary-parsers-alist)))

    ;; evil-textobj-tree-sitter
    (after! evil-textobj-tree-sitter
      (add-to-list 'evil-textobj-tree-sitter-major-mode-language-alist
                   '(kubernetes-helm-mode . gotmpl)))))
