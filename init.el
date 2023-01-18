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

;; HACK Load code that must be evaluated in init.el but outside of the `doom!'
;; block (e.g. CLI modifications and `use-package-hook!' forms)
(load! "early-init.el" doom-user-dir t)

(doom! :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       (company +childframe)
                         ; the ultimate code completion backend
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
       ;;indent-guides     ; highlighted indent columns
       (ligatures +extra)  ; ligatures and symbols to make your code pretty again
       minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       ;;popup             ; tame sudden yet inevitable temporary windows
       tabs              ; XXX user module
       (treemacs +lsp)   ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       (vc-gutter +pretty +diff-hl)
                         ; vcs diff in the fringe
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
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)    ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
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
       syntax            ; tasing you for every semicolon you forget
       spell             ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ansible
       ;;biblio            ; Writes a PhD for you (citation needed)
       ;;cloudformation  ; TODO XXX user module
       (debugger +lsp)   ; FIXME stepping through code, to help you add bugs
       direnv
       (docker +lsp)
       editorconfig      ; let someone else argue about tabs vs spaces
       ein               ; tame Jupyter notebooks with emacs
       elasticsearch       ; XXX user module
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       (kubernetes +lsp +tree-sitter) ; XXX user module
       (lookup +dictionary +docsets)
                         ; navigate your code and its documentation
       (lsp +peek)         ; M-x vscode
       (magit +forge)      ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       nginx               ; XXX user module
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       (terraform +lsp)  ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       tree-sitter       ; syntax and parsing, sitting in a tree...
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       ;; NOTE `+eglot' adds commands for displaying inheritance type hierarchies
       (cc +lsp +tree-sitter) ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       (csharp +lsp +tree-sitter +dotnet)
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
       (go +lsp)         ; the hipster dialect
       ;;(graphql +lsp)    ; Give queries a REST
       (haskell +lsp)    ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       (json +lsp +tree-sitter) ; At least it ain't XML
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       (javascript +lsp +tree-sitter) ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       (latex +lsp)      ; writing papers in Emacs has never been so fun
       ;;lean              ; for folks with too much to prove
       ;;ledger            ; be audit you can be
       (lua +lsp)          ; one-based indices? one-based indices
       (markdown +grip)  ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       (nix +tree-sitter) ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org +dragndrop +gnuplot +jupyter +noter +pandoc +pomodoro +present +roam2)
                         ; organize your plain life in plain text
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python +lsp +tree-sitter +pyright)
                         ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       (racket +lsp +xp) ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       rest              ; Emacs as a REST client
       rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp +tree-sitter) ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       (sh +lsp +tree-sitter +fish +powershell)
                         ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       (web +lsp +tree-sitter) ; the tubes
       (yaml +lsp)         ; JSON, but readable
       ;;zig               ; C, but simpler

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       everywhere        ; *leave* Emacs!? You must be joking
       irc               ; how neckbeards socialize
       rss               ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       literate
       (default +bindings))
