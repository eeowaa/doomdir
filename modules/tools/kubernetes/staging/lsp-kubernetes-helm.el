;;; lsp-kubernetes-helm.el --- lsp-mode kubernetes-helm integration -*- lexical-binding: t; -*-

;;; Code:

(require 'lsp-mode)
(require 'json)

(defgroup lsp-kubernetes-helm nil
  "Settings for the Helm Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/mrjosh/helm-ls")
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-kubernetes-helm-language-server-command
  '("helm_ls" "serve")
  "The command that starts the Helm language server."
  :type '(repeat :tag "List of string values" string)
  :group 'lsp-kubernetes-helm
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-kubernetes-helm-download-url
  "https://github.com/mrjosh/helm-ls/releases/download/master/helm_ls_linux_amd64"
  "Automatic download url for Helm language server."
  :type 'string
  :group 'lsp-kubernetes-helm
  :package-version '(lsp-mode . "8.0.1"))

(lsp-dependency 'helm-ls
                '(:system "helm_ls")
                `(:download :url lsp-kubernetes-helm-download-url
                            :store-path ,(f-join lsp-server-install-dir "kubernetes-helm/helm_ls")
                            :set-executable? t))

(add-to-list 'lsp-language-id-configuration '(kubernetes-helm-mode . "kubernetes-helm"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(or (executable-find
                             (cl-first lsp-kubernetes-helm-language-server-command))
                            (lsp-package-path 'helm-ls))
                       ,@(cl-rest lsp-kubernetes-helm-language-server-command))))
  :priority 1 ; Higher priority than YAML
  :activation-fn (lsp-activate-on "kubernetes-helm")
  :server-id 'helm-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'helm-ls callback error-callback))))

(provide 'lsp-kubernetes-helm)

;;; lsp-kubernetes-helm.el ends here
