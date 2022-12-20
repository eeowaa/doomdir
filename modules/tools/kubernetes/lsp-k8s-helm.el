;;; lsp-k8s-helm.el --- lsp-mode k8s-helm integration -*- lexical-binding: t; -*-

;;; Code:

(require 'lsp-mode)
(require 'json)

(defgroup lsp-k8s-helm nil
  "Settings for the Helm Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/mrjosh/helm-ls")
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-k8s-helm-language-server-command
  '("helm_ls" "serve")
  "The command that starts the Helm language server."
  :type '(repeat :tag "List of string values" string)
  :group 'lsp-k8s-helm
  :package-version '(lsp-mode . "8.0.1"))

(defcustom lsp-k8s-helm-download-url
  "https://github.com/mrjosh/helm-ls/releases/download/master/helm_ls_linux_amd64"
  "Automatic download url for Helm language server."
  :type 'string
  :group 'lsp-k8s-helm
  :package-version '(lsp-mode . "8.0.1"))

(lsp-dependency 'helm-ls
                '(:system "helm_ls")
                `(:download :url lsp-k8s-helm-download-url
                            :store-path ,(f-join lsp-server-install-dir "k8s-helm/helm_ls")
                            :set-executable? t))

(add-to-list 'lsp-language-id-configuration '(k8s-helm-mode . "k8s-helm"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(or (executable-find
                             (cl-first lsp-k8s-helm-language-server-command))
                            (lsp-package-path 'helm-ls))
                       ,@(cl-rest lsp-k8s-helm-language-server-command))))
  :priority 1 ; Higher priority than YAML
  :activation-fn (lsp-activate-on "k8s-helm")
  :server-id 'helm-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'helm-ls callback error-callback))))

(provide 'lsp-k8s-helm)

;;; lsp-k8s-helm.el ends here
