;; -*- no-byte-compile: t; -*-
;;; tools/nginx/packages.el

(package! nginx-mode)
(when (modulep! :completion company)
  (package! company-nginx
    :recipe (:host github
             :repo "emacsmirror/company-nginx")))
