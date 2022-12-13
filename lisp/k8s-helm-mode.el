;;; k8s-helm-mode.el --- Major mode for Kubernetes Helm files -*- lexical-binding: t; -*-

;;; Code:

(require 'k8s-mode)

(defgroup k8s-helm nil
  "Major mode of Kubernetes Helm template file."
  :group 'languages
  :prefix "k8s-helm-")

(defcustom k8s-helm-mode-hook nil
  "*Hook run by `k8s-helm-mode'."
  :type 'hook
  :group 'k8s-helm)

(defcustom k8s-helm-mode-lighter "Helm"
  "k8s-helm-mode lighter."
  :type 'string
  :group 'k8s-helm)

(define-derived-mode k8s-helm-mode k8s-mode k8s-helm-mode-lighter
  "Major mode for editing Kubernetes Helm templates.")

(provide 'k8s-helm-mode)

;;; k8s-helm-mode.el ends here
