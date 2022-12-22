;; -*- no-byte-compile: t; -*-
;;; lang/kubernetes/packages.el

(package! kubedoc)

(when (package! k8s-mode)
  (package! kubernetes-helm-mode :recipe
    (:local-repo "lisp"
     :files ("kubernetes-helm-mode.el")
     :build (:not compile))))

(when (and (modulep! :tools lsp)
           (not (modulep! :tools lsp +eglot))
           (modulep! +lsp))
  (package! lsp-kubernetes-helm :recipe
    (:local-repo "lisp"
     :files ("lsp-kubernetes-helm.el")
     :build (:not compile))))

(package! go-template-mode :recipe
  (:host nil
   :repo "https://gist.github.com/eeowaa/3c18e0d7ad06ef8032e07ee3229f96e3"))
