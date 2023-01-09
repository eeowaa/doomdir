;; -*- no-byte-compile: t; -*-
;;; ui/tabs/packages.el

(package! tab-bar)

(package! vimish-tab :recipe
  (:local-repo "lisp"
   :files ("vimish-tab.el")
   :build (:not compile)))
