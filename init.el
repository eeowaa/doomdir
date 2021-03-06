;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       (company +childframe)
                         ; the ultimate code completion backend
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       (ivy +icons)      ; a search engine for love and life

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       ;;doom-dashboard    ; a nifty splash screen for Emacs
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       (emoji +unicode)  ; 🙂
       fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       ;;indent-guides     ; highlighted indent columns
       (ligatures +extra)  ; ligatures and symbols to make your code pretty again
       minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;;tabs              ; a tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       ;;(format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       ;; PREREQ brew install coreutils
       (dired +icons)    ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)  ; interactive buffer management
       ;; PREREQ brew install zstd
       ;; FIXME (find-file (doom-module-locate-path :emacs "undo" "README.org"))
       (undo +tree)      ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;; PREREQ brew install fish
       eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       ;; PREREQ brew install libvterm cmake
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax            ; tasing you for every semicolon you forget
       ;; PREREQ brew install aspell
       spell             ; tasing you for misspelling mispelling
       ;; PREREQ brew install languagetool
       ;; PREREQ sudo ln -sfn /usr/local/opt/openjdk@11/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-11.jdk
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;; PREREQ brew install ansible
       ansible
       ;; PREREQ brew install gdb
       ;; REVIEW https://stackoverflow.com/questions/18423124/please-check-gdb-is-codesigned-see-taskgated8-how-to-get-gdb-installed-w
       ;;(debugger +lsp)   ; FIXME stepping through code, to help you add bugs
       ;; PREREQ brew install direnv
       direnv
       ;; PREREQ brew install docker
       ;; PREREQ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash
       ;; PREREQ npm install -g dockerfile-language-server-nodejs
       (docker +lsp)
       ;; PREREQ brew install editorconfig
       editorconfig      ; let someone else argue about tabs vs spaces
       ;; PREREQ brew install python
       ;; PREREQ pip install jupyter
       ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       ;; PREREQ brew install sqlite3
       (lookup +dictionary +docsets)
                         ; navigate your code and its documentation
       (lsp +peek)
       ;; PREREQ cd ~/stow-dotfiles && make perl
       ;; PREREQ brew install perl git-absorb
       ;; PREREQ cpan install App::Git::Autofixup
       (magit +forge)      ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       ;; PREREQ brew install pkg-config poppler automake
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       ;; PREREQ brew install terraform
       terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;; PREREQ brew install ccls gdb glslang
       ;; NOTE `+eglot' adds commands for displaying inheritance type hierarchies
       (cc +lsp)         ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;; PREREQ curl --create-dirs -o ~/.config/emacs/.local/etc/lsp/xmlls/org.eclipse.lemminx-0.13.1-uber.jar https://repo.eclipse.org/content/repositories/lemminx-releases/org/eclipse/lemminx/org.eclipse.lemminx/0.13.1/org.eclipse.lemminx-0.13.1-uber.jar
       data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;; PREREQ cd ~/stow-dotfiles && make go
       ;; PREREQ brew install go gopls golangci-lint
       ;; PREREQ FIXME bash < <(curl -s https://raw.githubusercontent.com/rocky/ssa-interp/master/gub-installer)
       ;;        See https://github.com/rocky/ssa-interp
       ;; PREREQ go get -v -u github.com/motemen/gore/cmd/gore
       ;; PREREQ go get -v -u github.com/stamblerre/gocode
       ;; PREREQ go get -v -u golang.org/x/tools/cmd/godoc
       ;; PREREQ go get -v -u golang.org/x/tools/cmd/goimports
       ;; PREREQ go get -v -u golang.org/x/tools/cmd/gorename
       ;; PREREQ go get -v -u golang.org/x/tools/cmd/guru
       ;; PREREQ go get -v -u github.com/cweill/gotests/...
       ;; PREREQ go get -v -u github.com/fatih/gomodifytags
       (go +lsp)         ; the hipster dialect
       ;;(haskell +dante)  ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       ;; PREREQ brew install jq
       ;; PREREQ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash
       ;; PREREQ npm install -g vscode-json-languageserver
       (json +lsp)       ; At least it ain't XML
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;; PREREQ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash
       ;; PREREQ npm install -g typescript javascript-typescript-langserver eslint trepan-ni
       ;; TODO What about installing eslint and the language server for that?
       (javascript +lsp) ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;; PREREQ brew install mactex texlab wget
       (latex +lsp)      ; writing papers in Emacs has never been so fun
       ;;lean
       ;;factor
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       ;; PREREQ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash
       ;; PREREQ npm install -g markdownlint marked
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       ;; PREREQ brew install gnuplot pandoc graphviz pngpaste
       ;; PREREQ mkdir -p ~/Documents/org/roam
       (org +dragndrop +gnuplot +jupyter +noter +pandoc +pomodoro +roam)
                         ; organize your plain life in plain text
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       ;; PREREQ brew install python
       ;; PREREQ pip install pylint flake8 pytest nose pyflakes isort ipython jupyter
       ;; PREREQ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash
       ;; PREREQ npm install -g pyright
       (python +lsp +pyright)
                         ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       rest              ; Emacs as a REST client
       rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;scheme            ; a fully conniving family of lisps
       ;; PREREQ brew install bash zsh fish powershell bashdb zshdb shellcheck
       ;; PREREQ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash
       ;; PREREQ npm install -g bash-language-server
       (sh +lsp +fish +powershell)
                         ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;; TODO Install language servers (and debuggers?)
       ;; (when (featurep! +lsp)
       ;;   (add-hook! '(html-mode-local-vars-hook
       ;;                web-mode-local-vars-hook)
       ;;              #'lsp!))
       ;; (when (featurep! +lsp)
       ;;   (add-hook! '(css-mode-local-vars-hook
       ;;                scss-mode-local-vars-hook
       ;;                sass-mode-local-vars-hook
       ;;                less-css-mode-local-vars-hook)
       ;;              #'lsp!))
       web               ; the tubes
       ;; PREREQ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash
       ;; PREREQ npm install -g yaml-language-server
       (yaml +lsp)         ; JSON, but readable

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       literate
       (default +bindings +smartparens))
