;;; eeowaa-treemacs-themes.el --- eeowaa's Treemacs themes -*- lexical-binding: t; -*-

;;; Code:

(require 'treemacs)


;; eeowaa-minimal

;; Based on "default" theme.
;; Recommended `treemacs-indentation' is 2.
(treemacs-create-theme "eeowaa-minimal"
  :icon-directory (treemacs-join-path treemacs-dir "icons/default")
  :config
  (progn
    ;; Root
    (treemacs-create-icon :file "vsc/root-closed.png"   :extensions (root-closed) :fallback "")
    (treemacs-create-icon :file "vsc/root-open.png"     :extensions (root-open)   :fallback "")

    ;; Directory
    (treemacs-create-icon :file "vsc/dir-closed.png"    :extensions (dir-closed)  :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon :file "vsc/dir-open.png"      :extensions (dir-open)    :fallback (propertize "- " 'face 'treemacs-term-node-face))

    ;; Tag
    (treemacs-create-icon :file "tags-leaf.png"         :extensions (tag-leaf)    :fallback (propertize "• " 'face 'font-lock-constant-face))
    (treemacs-create-icon :file "tags-open.png"         :extensions (tag-open)    :fallback (propertize "▸ " 'face 'font-lock-string-face))
    (treemacs-create-icon :file "tags-closed.png"       :extensions (tag-closed)  :fallback (propertize "▾ " 'face 'font-lock-string-face))

    ;; Status
    (treemacs-create-icon :file "error.png"             :extensions (error)       :fallback (propertize "• " 'face 'font-lock-string-face))
    (treemacs-create-icon :file "warning.png"           :extensions (warning)     :fallback (propertize "• " 'face 'font-lock-string-face))
    (treemacs-create-icon :file "info.png"              :extensions (info)        :fallback (propertize "• " 'face 'font-lock-string-face))

    ;; File
    (treemacs-create-icon :file "txt.png"               :extensions (fallback)    :fallback "~ ")))


;; eeowaa-nerd-icons

(unless (getenv "SSH_CLIENT")
  (require 'treemacs-nerd-icons)

  ;; Based on "nerd-icons" theme.
  ;; Recommended `treemacs-indentation' is 3.
  (treemacs-create-theme "eeowaa-nerd-icons"
    :config
    (progn
      (dolist (item nerd-icons-extension-icon-alist)
        (let* ((extension (car item))
               (func (cadr item))
               (args (append (list (cadr (cdr item))) '(:v-adjust -0.05 :height 1.0) (cdr (cddr item))))
               (icon (apply func args)))
          ;; XXX: Leading whitespace removed from `icon-pair' components
          (let* ((icon-pair (cons (format "%s%s" icon treemacs-nerd-icons-tab) (format "%s%s" icon treemacs-nerd-icons-tab)))
                 (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
                 (tui-icons (treemacs-theme->tui-icons treemacs--current-theme))
                 (gui-icon  (car icon-pair))
                 (tui-icon  (cdr icon-pair)))
            (ht-set! gui-icons extension gui-icon)
            (ht-set! tui-icons extension tui-icon))))

      ;; Root (no change)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-repo"   :face 'treemacs-nerd-icons-root-face) treemacs-nerd-icons-tab)
                            :extensions (root-closed root-open)
                            :fallback 'same-as-icon)

      ;; Directory (chevrons removed)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder_open"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (dir-open)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (dir-closed)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder_open"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("src-open")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("src-closed")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder_open"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("build-open")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("build-closed")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder_open"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("test-open")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("test-closed")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder_open"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("bin-open")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("bin-closed")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder_open"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("git-open")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("git-closed")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder_open"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("github-open")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("github-closed")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder_open"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("public-open")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("public-closed")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder_open"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("private-open")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("private-closed")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder_open"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("temp-open" "tmp-open")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("temp-closed" "tmp-closed")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder_open"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("readme-open" "docs-open")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("readme-closed" "docs-closed")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder_open"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("screenshots-open" "icons-open")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions ("screenshots-closed" "icons-closed")
                            :fallback 'same-as-icon)

      ;; Tag (chevrons removed)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-package"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (tag-open)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-package"  :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (tag-closed)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-tag"   :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (tag-leaf)
                            :fallback 'same-as-icon)

      ;; Status (no change)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-flame"  :face 'nerd-icons-red) treemacs-nerd-icons-tab)
                            :extensions (error)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-stop"  :face 'nerd-icons-yellow) treemacs-nerd-icons-tab)
                            :extensions (warning)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-info"   :face 'nerd-icons-blue) treemacs-nerd-icons-tab)
                            :extensions (info)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-mdicon "nf-md-mail"   :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (mail)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-bookmark"   :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (bookmark)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-mdicon "nf-md-monitor"   :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (screen)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-mdicon "nf-md-home"   :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (house)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-list"   :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (list)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-mdicon "nf-md-repeat"   :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (repeat)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-suitcase"   :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (suitcase)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-mdicon "nf-md-close"   :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (close)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-octicon "nf-oct-calendar"   :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (calendar)
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-briefcase"   :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (briefcase)
                            :fallback 'same-as-icon)

      ;; File (leading whitespace removed)
      (treemacs-create-icon :icon (format "%s%s" (nerd-icons-faicon "nf-fa-file_o" :face 'treemacs-nerd-icons-file-face) treemacs-nerd-icons-tab)
                            :extensions (fallback)
                            :fallback 'same-as-icon))))


(provide 'eeowaa-treemacs-themes)
;;; eeowaa-treemacs-themes.el ends here
