(use-package! nginx-mode
  :mode "nginx.*\\.conf"
  :config
  (when (modulep! :tools lsp)
    (add-hook 'nginx-mode-local-vars-hook #'lsp! 'append))
  (set-docsets! 'nginx-mode "Nginx"))

(use-package! company-nginx
  :when (modulep! :completion company)
  :after nginx-mode
  :config
  (set-company-backend! 'nginx-mode 'company-nginx))
