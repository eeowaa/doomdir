;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

;; HACK Load code that must be evaluated in init.el but outside of the `doom!'
;; block (e.g. CLI modifications and `use-package-hook!' forms)
(load! "early-init.el" doom-user-dir t)

(doom! :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
;;LEAN (company +childframe)
       (company)
                         ; the ultimate code completion backend
       ;;(corfu +orderless)  ; complete with cap(f), cape and a flying feather!
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;;(ivy +icons)      ; a search engine for love and life
       (vertico +icons)  ; the search engine of the future

       :ui
       buffer-group      ; XXX user module
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       ;;doom-dashboard    ; a nifty splash screen for Emacs
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;; FIXME Cannot download Emojis without errors
       (emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       ilist             ; XXX user module
       indent-guides     ; highlighted indent columns
       (ligatures +extra)  ; ligatures and symbols to make your code pretty again
;;GONE minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       ;;popup             ; tame sudden yet inevitable temporary windows
       tabs              ; XXX user module
;;LEAN (treemacs +lsp)   ; a project drawer, like neotree but cooler
       (treemacs)
       ;;unicode           ; extended unicode support for various languages
       (vc-gutter +diff-hl)
                         ; vcs diff in the fringe
       ;;vi-tilde-fringe   ; fringe tildes to mark beyond EOB
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
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)    ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ;;eww               ; the internet is gross
       (ibuffer +icons)  ; interactive buffer management
       ;; FIXME (find-file (doom-module-locate-path :emacs "undo" "README.org"))
       (undo +tree)      ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       vterm             ; the best terminal emulation in Emacs

       :checkers
;;LEAN (syntax +childframe)
       (syntax)
                         ; tasing you for every semicolon you forget
;;GONE (spell +hunspell +flyspell) ; tasing you for misspelling mispelling
;;GONE grammar           ; tasing grammar mistake every you make

       :tools
       ansible
       ;;biblio            ; Writes a PhD for you (citation needed)
;;GONE bitwarden           ; XXX user module
       ;;cloudformation  ; TODO XXX user module
       ;;collab            ; buffers with friends
;;GONE (debugger +lsp)   ; FIXME stepping through code, to help you add bugs
       direnv
;;LEAN (docker +lsp)
       (docker)
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       ;;elasticsearch       ; XXX user module
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
;;GONE (kubernetes +lsp +tree-sitter) ; XXX user module
;;LEAN (lookup +dictionary +offline +docsets)
       (lookup)
                         ; navigate your code and its documentation
;;GONE (lsp +peek)         ; M-x vscode
;;LEAN (magit +forge)      ; a git porcelain for Emacs
       (magit)
       make              ; run make tasks from Emacs
;;GONE nginx               ; XXX user module
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
;;GONE (terraform +lsp)  ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       tree-sitter       ; syntax and parsing, sitting in a tree...
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if (featurep :system 'macos) macos) ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       ;; NOTE `+eglot' adds commands for displaying inheritance type hierarchies
;;LEAN (cc +lsp +tree-sitter) ; C > C++ == 1
       (cc +tree-sitter)
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
;;LEAN ;;(csharp +lsp +tree-sitter +dotnet)
       (csharp +tree-sitter)
                         ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;dhall
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fortran           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
;;GONE (go +lsp +tree-sitter) ; the hipster dialect
       ;;(graphql +lsp)    ; Give queries a REST
       ;;(haskell +lsp +tree-sitter) ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
;;LEAN (json +lsp +tree-sitter) ; At least it ain't XML
       (json +tree-sitter)
       ;;(java +lsp)       ; the poster child for carpal tunnel syndrome
;;LEAN (javascript +lsp +tree-sitter) ; all(hope(abandon(ye(who(enter(here))))))
       (javascript +tree-sitter)
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
;;GONE (latex +lsp)      ; writing papers in Emacs has never been so fun
       ;;lean              ; for folks with too much to prove
       ;;ledger            ; be audit you can be
;;GONE (lua +lsp +tree-sitter) ; one-based indices? one-based indices
;;LEAN (markdown +grip)  ; writing docs for people to ignore
       (markdown)
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
;;LEAN (org +dragndrop +journal +noter +pandoc +pomodoro +present +roam2)
       (org +pandoc)
                         ; organize your plain life in plain text
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
;;GONE graphviz          ; diagrams for confusing yourself even more
       ;;purescript        ; javascript, but functional
;;LEAN (python +lsp +tree-sitter +pyright)
       (python +tree-sitter)
                         ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;(racket +lsp +xp) ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       (rest +jq)        ; Emacs as a REST client
       rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
;;GONE (rust +lsp +tree-sitter) ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
;;LEAN (sh +lsp +tree-sitter +fish +powershell)
       (sh +tree-sitter +powershell)
                         ; she sells {ba,z,fi}sh shells on the C xor
                         ; FIXME +tree-sitter breaks markdown code blocks
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
;;LEAN (web +lsp +tree-sitter) ; the tubes
       (web +tree-sitter)
;;LEAN (yaml +lsp +tree-sitter) ; JSON, but readable
       (yaml +tree-sitter)
       ;;zig               ; C, but simpler

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere        ; *leave* Emacs!? You must be joking
       ;;irc               ; how neckbeards socialize
       ;;rss               ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       literate
       (default +bindings))
