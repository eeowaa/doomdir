;;; kubernetes-helm-mode.el --- Major mode for Kubernetes Helm files -*- lexical-binding: t; -*-

;;; Code:

(require 'k8s-mode)

(defgroup kubernetes-helm nil
  "Major mode of Kubernetes Helm template file."
  :group 'languages
  :prefix "kubernetes-helm-")

(defcustom kubernetes-helm-mode-hook nil
  "Hook run by `kubernetes-helm-mode'."
  :type 'hook
  :group 'kubernetes-helm)

(defcustom kubernetes-helm-mode-lighter "K8s Helm"
  "kubernetes-helm-mode lighter."
  :type 'string
  :group 'kubernetes-helm)

;;;###autoload
(define-derived-mode kubernetes-helm-mode k8s-mode kubernetes-helm-mode-lighter
  "Major mode for editing Kubernetes Helm templates.")

(provide 'kubernetes-helm-mode)

;;; kubernetes-helm-mode.el ends here
