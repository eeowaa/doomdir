;;; tools/elasticsearch/config.el -*- lexical-binding: t; -*-

(use-package! es-mode
  :mode ("\\.es\\'" . es-mode)
  :config
  (set-company-backend! 'es-mode 'es-company-backend)
  (after! ob
    (add-to-list 'org-babel-load-languages '(elasticsearch . t))))
