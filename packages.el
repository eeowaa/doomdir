;; -*- no-byte-compile: t; -*-
;;; DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;;; :input

;;; :completion

(unpin! (:completion company))

;;; :ui

(package! col-highlight)
(package! page-break-lines)

;; Needed by `doom-themes-ext-treemacs'
(package! all-the-icons)

;; Collection of themes by prot
(package! ef-themes)

;; More coloration in *info* buffers
(package! info-colors)

;; Vim-like diff
(package! vdiff)
(package! vdiff-magit)

(unpin! doom-modeline)

(unpin! (:ui treemacs))

;;; :editor

;;; :emacs

;;; :term

;;; :checkers

;;; :tools

(when EMACS28+
  (package! xref :pin "a82f459b37b31546bf274388baf8aca79e9c30d9"))

;; If this causes issues, try `(unpin! (:tools lsp))' instead
(unpin! lsp-mode)

(package! rfc-mode)

(unpin! tree-sitter-langs)

(package! tree-sitter-indent
  :recipe (:host github
           :repo "emacsattic/tree-sitter-indent"))

;; HACK: Do not install ts-fold mode
(package! ts-fold :disable t)

(package! x509-mode)

;;; :os

;;; :lang

;; Markup
(package! adoc-mode)

;; Filtering
(package! sed-mode)

;; Config
(package! systemd)
(package! nix-mode)

;; Builds
(package! jenkinsfile-mode)
;;(package! gitlab-ci-mode)
;;(package! gitlab-ci-mode-flycheck)

;; Applescript
(when (featurep :system 'macos)
  (package! applescript-mode)
  (package! ob-applescript))

;; MongoDB
;;(package! ob-mongo)
;;(package! inf-mongo)

;; Metasyntax
(package! ebnf-mode)
(package! protobuf-mode)

(when (modulep! :lang org +jupyter)
  (package! ox-ipynb
    :recipe (:host github :repo "jkitchin/ox-ipynb")))

;; (package! zmq :pin "00dea2f9f88cd3e723d3b909aea26313dd33f9c6")
(unpin! zmq)
(unpin! jupyter)

;;; :email

;;; :app

;;; Music playback
;;(package! bluesound)
;;(package! bongo)
;;(package! counsel-spotify)
;;(package! dionysos)
;;(package! lyrics)
;;(package! lyrics-fetcher)

;;; Music creation
;;(package! abc-mode)
;;(package! csound-mode)
;;(package! metronome)

;;; Fun and games
;;(package! crossword)
;;(package! dad-joke)
;;(package! dilbert)

;;; Weather
(package! noaa
  :recipe (:host github :repo "emacsmirror/noaa"))
;;(package! aqi)

;;; Clocks and timers
;; TODO Configure `world-clock'
;;(package! countdown)

;;; Ebooks
(package! nov
  :recipe (:host github :repo "emacsmirror/nov"))
;;(package! sicp)

;;; Browsing
(package! w3m) ;; Better than `eww' IMO
;;(package! arxiv-mode)
;;(package! devdocs-browser)

;; Packaging
;;(package! archive-rpm)
;;(package! conventional-changelog)

;; System configuration
;;(package! crontab-mode) ;; Beware of existing entry in `auto-mode-alist'
;;(package! daemons)
;;(package! etc-sudoers-mode)
;;(package! su)
;;(package! udev-mode)

;; System
;;(package! backlight)
;;(package! battery-notifier)
;;(package! bluetooth)
;;(package! desktop-environment)

;; External programs
;;(package! desktop-mail-user-agent)

;; Inferior processes
;;(package! detached)

;; Jupyter
;; REVIEW jupytext: https://jupytext.readthedocs.io/en/latest/
;;(package! code-cells)

;; Kubernetes
(package! kubel)      ;; interaction with pods
(package! kubernetes) ;; interaction with a cluster
(when (modulep! :editor evil +everywhere)
  (package! kubel-evil)
  (package! kubernetes-evil))

;; Git
;;(package! blamer)
;;(package! browse-at-remote)
;;(package! magit-annex)
;;(package! magit-lfs)

;; Projects
;;(package! bookmark-in-project)

;; Logging
;;(package! logview)
;;(package! cabledolphin)

;; Test-driven development
;;(package! cov)

;; Diffs
;;(package! diff-ansi)
;;(package! diffview)

;; Self-improvement
;;(package! leetcode)
;;(package! metrics-tracker)

;; Finance
;;(package! ledger-mode)
;;(package! btc-ticker)
;;(package! coin-ticker)

;; Social
;;(package! mastadon)

(package! gptel)

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
