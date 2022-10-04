;; -*- no-byte-compile: t; -*-
;;; DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;;; :input

;;; :completion

;;; :ui

(package! col-highlight)
(package! page-break-lines)
(package! imenu-list)

;; More pretty themes by prot
(package! ef-themes)

;;; :editor

;;; :emacs

;;; :term

;;; :checkers

;;; :tools

(when EMACS28+
  (package! xref :pin "a82f459b37b31546bf274388baf8aca79e9c30d9"))

;;; :os

;;; :lang

;; Markup
(package! adoc-mode)

;; Filtering
(package! sed-mode)
(package! jq-mode)

;; Config
(package! systemd)
(package! nginx-mode)
(when (modulep! :completion company)
  (package! company-nginx))

;; Builds
(package! jenkinsfile-mode)

;; Applescript
(when IS-MAC
  (package! applescript-mode)
  (package! ob-applescript))

;; MongoDB
(package! ob-mongo)
(package! inf-mongo)

(when (modulep! :lang org +jupyter)
  (package! ox-ipynb
    :recipe (:host github :repo "jkitchin/ox-ipynb")))

;;; :email

;;; :app

;; Music
(package! counsel-spotify)

;; Weather
(package! noaa)

;; Ebooks
(package! nov)

;; Kubernetes
(package! k8s-mode)   ;; highlighting, snippets, and `k8s-goto-documents'
(package! kubedoc)    ;; `kubedoc' command
(package! kubel)      ;; interaction with pods
(package! kubernetes) ;; interaction with a cluster
(when (modulep! :editor evil +everywhere)
  (package! kubel-evil)
  (package! kubernetes-evil))

;; Browsing
(package! w3m) ;; Better than `eww' IMO

(package! elfeed-goodies :disable t)

;;; :config

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
