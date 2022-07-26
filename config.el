(setq company-idle-delay nil)

(setq counsel-org-goto-all-outline-path-prefix 'buffer-name)

(defun my/toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(define-key! evil-window-map
  ;; replaces `+workspace/close-window-or-workspace'
  "d" #'my/toggle-window-dedicated)

;; NOTE For whatever reason, I cannot use :defer to lazy-load `imenu-list'
;; without it breaking
(use-package! imenu-list
  :init
  (define-key! doom-leader-open-map "i" #'imenu-list-minor-mode)
  :after imenu)

(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`%s o i\\'" prefix-re)) nil . "Ilist")
                which-key-replacement-alist)))

(setq all-the-icons-scale-factor 1.0)

(setq extended-command-suggest-shorter nil)

;; `always' is just a no-op that returns `t'
(advice-add 'hide-mode-line-mode :override #'always)
(advice-add 'doom-themes-hide-modeline :override #'always)

(setq doom-theme
      (if initial-window-system
          'doom-outrun-electric
        'eeowaa-henna))

(after! (solaire-mode ivy)
  (custom-theme-set-faces! 'doom-outrun-electric
    `(solaire-hl-line-face :background
                           ,(face-attribute 'ivy-current-match :background))))

(setq doom-font (font-spec :family "Comic Mono" :size 16))

(after! hl-todo
  (setq hl-todo-keyword-faces
        (append '(("TESTME" font-lock-constant-face bold)
                  ("PREREQ" font-lock-doc-face bold)
                  ("DEBUG" font-lock-preprocessor-face bold))
                hl-todo-keyword-faces)))

(after! projectile
  (global-set-key (kbd "C-c r") 'hydra-run/body)
  (defhydra hydra-run (:color blue :hint none)
    "
confi_g_ure -> ?g?
_c_ompile ---> ?c?
_t_est ------> ?t?
_r_un -------> ?r?
_i_nstall ---> ?i?
_p_ackage ---> ?p?
"
    ("g" (let ((compilation-read-command)) (funcall #'projectile-configure-project nil))
     (format "%s" projectile-project-configure-cmd))
    ("c" (let ((compilation-read-command)) (funcall #'projectile-compile-project nil))
     (format "%s" projectile-project-compilation-cmd))
    ("t" (let ((compilation-read-command)) (funcall #'projectile-test-project nil))
     (format "%s" projectile-project-test-cmd))
    ("r" (let ((compilation-read-command)) (funcall #'projectile-run-project nil))
     (format "%s" projectile-project-run-cmd))
    ("i" (let ((compilation-read-command)) (funcall #'projectile-install-project nil))
     (format "%s" projectile-project-install-cmd))
    ("p" (let ((compilation-read-command)) (funcall #'projectile-package-project nil))
     (format "%s" projectile-project-package-cmd))))

(global-set-key (kbd "C-c g") 'hydra-game/body)
(defhydra hydra-game (:color blue :hint nil)
  "
^Arcade^      ^Puzzle^        ^Board^          ^Text^        ^Self-Playing^
^-^-----------^-^--------------------------------------------^-^-----------
_t_: Tetris   _5_: 5x5        _g_: Gomoku      _a_: Dunnet   _l_: Life
_s_: Snake    _b_: Blackbox   _i_: Solitaire   _d_: Doctor   _h_: Hanoi
_p_: Pong     _m_: Mpuz       ^ ^              ^ ^           _z_: Zone
^ ^           _o_: Bubbles
"
  ;; Arcade
  ("t" tetris)
  ("s" snake)
  ("p" pong)

  ;; Puzzle
  ("5" 5x5)
  ("b" blackbox)
  ("m" mpuz)
  ("o" bubbles)

  ;; Board
  ("i" solitaire)
  ("g" gomoku)

  ;; Text
  ("a" dunnet)
  ("d" doctor)

  ;; Self-Playing
  ("l" life)
  ("h" hanoi)
  ("z" zone)

  ;; Other
  ("q" nil))

(global-set-key (kbd "C-c s") 'hydra-spotify/body)
(defhydra hydra-spotify (:color blue :hint nil)
  "
^Playback control^   ^Collection^     ^Song^           ^Open Spotify^
^---^----------------^-^--------------^-^-------------------------------
_SPC_: Play/Pause    _l_: Playlist    _s_: By name     _o_: Application
  _n_: Next          _a_: Artist      _A_: By artist   _w_: Web player
  _p_: Previous      _r_: Record      _R_: By record   _i_: Integrations
"
  ;; Playback Control
  ("SPC" counsel-spotify-toggle-play-pause :color red)
  ("n" counsel-spotify-next :color red)
  ("p" counsel-spotify-previous :color red)

  ;; Collection
  ("l" counsel-spotify-search-playlist)
  ("a" counsel-spotify-search-artist)
  ("r" counsel-spotify-search-album)

  ;; Song
  ("s" counsel-spotify-search-track)
  ("A" counsel-spotify-search-tracks-by-artist)
  ("R" counsel-spotify-search-tracks-by-album)

  ;; Open Spotify
  ("o" (cond
        (IS-MAC (call-process "open" nil nil nil "-a" "spotify"))
        (IS-LINUX (call-process "xdg-open" nil nil nil "spotify"))
        (t (user-error! "Unsupported operating system"))))
  ("w" (browse-url "https://open.spotify.com"))
  ("i" (browse-url "https://developer.spotify.com/my-applications"))

  ;; Other
  ("q" nil))

(global-set-key (kbd "C-c c") 'hydra-timeclock/body)
(defhydra hydra-timeclock (:color blue)
  "Timeclock"
  ("i" timeclock-in "In")
  ("o" timeclock-out "Out")
  ("c" timeclock-change "Change")
  ("e" timeclock-visit-timelog "Edit")
  ("g" timeclock-reread-log "Reload")
  ("s" timeclock-status-string "Status")
  ("r" timeclock-generate-report "Report")
  ("q" nil "Quit"))

(global-set-key (kbd "C-c t") 'hydra-table/body)
(defhydra hydra-table ()
  "table.el"
  ("n" hydra-table-navigate/body "Navigate" :exit t)
  ("i" hydra-table-insert/body "Insert" :exit t)
  ("d" hydra-table-delete/body "Delete" :exit t)
  ("s" hydra-table-span-or-split/body "Span or Split" :exit t)
  ("r" hydra-table-resize/body "Resize" :exit t)
  ("j" hydra-table-justify/body "Justify" :exit t)
  ("e" hydra-table-export/body "Export" :exit t)
  ("SPC" ignore nil :color red))

(defhydra hydra-table-navigate ()
  "Navigation"
  ("1" (progn (table-goto-top-left-corner)
              (forward-char) (next-line)))
  ("2" (progn (table-goto-top-right-corner)
              (backward-char) (next-line)))
  ("3" (progn (table-goto-bottom-left-corner)
              (forward-char) (previous-line)))
  ("4" (progn (table-goto-bottom-right-corner)
              (backward-char) (previous-line)))
  ("f" table-forward-cell)
  ("b" table-backward-cell)
  ("SPC" hydra-table/body "Menu" :exit 1))

(defhydra hydra-table-insert ()
  "Insert"
  ("t" table-insert "table")
  ("r" table-insert-row "row")
  ("c" table-insert-column "column")
  ("s" table-insert-sequence "sequence")
  ("SPC" hydra-table/body "Menu" :exit 1))

(defhydra hydra-table-delete ()
  "Delete"
  ("r" table-delete-row "row")
  ("c" table-delete-column "column")
  ("SPC" hydra-table/body "Menu" :exit 1))

(defhydra hydra-table-span-or-split ()
  "Span or Split"
  ("h" (table-span-cell 'left))
  ("j" (table-span-cell 'below))
  ("k" (table-span-cell 'above))
  ("l" (table-span-cell 'right))
  ("|" table-split-cell-horizontally)
  ("-" table-split-cell-vertically)
  ("SPC" hydra-table/body "Menu" :exit 1))

(defhydra hydra-table-resize ()
  "Resize"
  ("}" table-heighten-cell "heighten")
  ("{" table-shorten-cell "shorten")
  (">" table-widen-cell "widen")
  ("<" table-narrow-cell "narrow")
  ("SPC" hydra-table/body "Menu" :exit 1))

(defhydra hydra-table-justify ()
  "Justify"
  ("a" hydra-table-justify-cell/body "Cell" :exit t)
  ("r" hydra-table-justify-row/body "Row" :exit t)
  ("c" hydra-table-justify-column/body "Column" :exit t)
  ("SPC" hydra-table/body "Menu" :exit 1))

(defhydra hydra-table-justify-cell ()
  "Justify Cell"
  ("h" (table-justify-cell 'left))
  ("j" (table-justify-cell 'bottom))
  ("k" (table-justify-cell 'top))
  ("l" (table-justify-cell 'right))
  ("c" (table-justify-cell 'center) "center")
  ("m" (table-justify-cell 'middle) "middle")
  ("n" (table-justify-cell 'none) "none")
  ("SPC" hydra-table/body "Menu" :exit 1))

(defhydra hydra-table-justify-row ()
  "Justify Row"
  ("h" (table-justify-row 'left))
  ("j" (table-justify-row 'bottom))
  ("k" (table-justify-row 'top))
  ("l" (table-justify-row 'right))
  ("c" (table-justify-row 'center) "center")
  ("m" (table-justify-row 'middle) "middle")
  ("n" (table-justify-row 'none) "none")
  ("SPC" hydra-table/body "Menu" :exit 1))

(defhydra hydra-table-justify-column ()
  "Justify Column"
  ("h" (table-justify-column 'left))
  ("j" (table-justify-column 'bottom))
  ("k" (table-justify-column 'top))
  ("l" (table-justify-column 'right))
  ("c" (table-justify-column 'center) "center")
  ("m" (table-justify-column 'middle) "middle")
  ("n" (table-justify-column 'none) "none")
  ("SPC" hydra-table/body "Menu" :exit 1))

(defhydra hydra-table-export ()
  "Export to"
  ("h" (table-generate-source 'html) "HTML")
  ("l" (table-generate-source 'latex) "LaTeX")
  ("c" (table-generate-source 'cals) "CALS")
  ("SPC" hydra-table/body "Menu" :exit 1))

(setq +ligatures-extras-in-modes '(org-mode))

(setq column-number-indicator-zero-based nil)

(map! :leader
      :desc "Raise popup"
      "^" #'+popup/raise)

;; Allow any letter to be used a manual section for Man (AWS CLI uses "a")
(setq Man-section-regexp "[a-zA-Z0-9+]+")

;; Consider "AVAILABLE.*" page sections to be "SEE ALSO"
(setq Man-see-also-regexp
      (format "\\(%s\\)"
              (string-join '("SEE ALSO"
                             "VOIR AUSSI"
                             "SIEHE AUCH"
                             "VÉASE TAMBIÉN"
                             "VEJA TAMBÉM"
                             "VEDERE ANCHE"
                             "ZOBACZ TAKŻE"
                             "İLGİLİ BELGELER"
                             "参照"
                             "参见 SEE ALSO"
                             "參見 SEE ALSO"
                             "AVAILABLE.*") ;; For AWS CLI man pages
                           "\\|")))

;; Allow buttons to be properly overlayed on AWS CLI man page references
(after! man
  (setq
   Man-reference-regexp
   (concat
    ;; Ignore bullet points
    "\\(?:^\\.IP \\\\(bu 2\\n\\|o \\)?"
    ;; This is the <name> part
    "\\(" Man-name-regexp
         "\\("
              ;; This allow line-continuations for long man page names
              ;;
              ;; SEE ALSO
              ;;     foo(1), bar(1), line-
              ;;     continuation(1)
              ;;
              "\\([-‐]\n\\)?"
              "[ \t]+" Man-name-regexp
         "\\)*"
    "\\)"
    ;; This is the (<section>) part
    "[ \t]*(\\(" Man-section-regexp "\\))")))

(setq +popup--display-buffer-alist
      (delq (assoc "^\\*\\(?:Wo\\)?Man " +popup--display-buffer-alist)
            +popup--display-buffer-alist))
(when (bound-and-true-p +popup-mode)
  (setq display-buffer-alist +popup--display-buffer-alist))

(setq Man-notify-method 'pushy)

(setq Man-width-max nil
      woman-fill-frame t)

(after! imenu-list
  (set-popup-rule! "^\\*Ilist\\*"
    :side 'right :size 35 :modeline "Ilist")
  (remove-hook 'imenu-list-major-mode-hook #'imenu-list--set-mode-line))

(setq doom-themes-treemacs-enable-variable-pitch nil)

(require 'ace-window)

(after! treemacs-evil
  (defun my/treemacs-visit-next ()
    "Open the next node in another window."
    (interactive)
    (treemacs-next-line 1)
    (treemacs-visit-node-no-split 1))
  (defun my/treemacs-visit-previous ()
    "Open the previous node in another window."
    (interactive)
    (treemacs-previous-line 1)
    (treemacs-visit-node-no-split 1))
  (define-key! evil-treemacs-state-map
    "J" #'my/treemacs-visit-next
    "K" #'my/treemacs-visit-previous))

(when (and (featurep! :ui vc-gutter)
           (featurep! :ui hydra))

  (define-key! doom-leader-git-map
    "v" #'+vc/gutter-hydra/body)

  (after! which-key
    (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
      (cl-pushnew `((,(format "\\`%s g v\\'" prefix-re)) nil . "VCGutter")
                  which-key-replacement-alist))))

(after! projectile
  (define-key! projectile-mode-map
    "C-c p" #'projectile-command-map))

;; Display ^L characters as horizontal lines
(use-package! page-break-lines
  :config (global-page-break-lines-mode))

;; Have C-l send the current line to the top of the window
(setq recenter-positions '(top bottom middle))

;; Perform a line feed after jumping to a ^L character
(defun my/recenter-top (&rest r) (recenter 0))
(advice-add 'forward-page :after #'my/recenter-top)

(setq default-input-method "latin-postfix")

(setq-default truncate-lines t)

(pushnew! evil-emacs-state-modes 'noaa-mode)

(after! 5x5
  (map! :mode 5x5-mode
    :e "k" #'5x5-up
    :e "j" #'5x5-down
    :e "h" #'5x5-left
    :e "l" #'5x5-right))

(after! blackbox
  (map! :mode blackbox-mode
    :e "k" #'bb-up
    :e "j" #'bb-down
    :e "h" #'bb-left
    :e "l" #'bb-right))

(after! bubbles
  (map! :mode bubbles-mode
    :e "k" #'previous-line
    :e "j" #'next-line
    :e "h" #'backward-char
    :e "l" #'forward-char))

(after! pong
  (advice-add 'pong-init :after (lambda () (evil-emacs-state)))
  (map! :map pong-mode-map
    :e "k" #'pong-move-up
    :e "j" #'pong-move-down
    :e "h" #'pong-move-left
    :e "l" #'pong-move-right))

(after! snake
  (map! :mode snake-mode
    :e "l" #'snake-move-right
    :e "h" #'snake-move-left
    :e "k" #'snake-move-up
    :e "j" #'snake-move-down))

(after! solitaire
  (map! :mode solitaire-mode
    :e "l" #'solitaire-right
    :e "h" #'solitaire-left
    :e "k" #'solitaire-up
    :e "j" #'solitaire-down
    :e "L" #'solitaire-move-right
    :e "H" #'solitaire-move-left
    :e "K" #'solitaire-move-up
    :e "J" #'solitaire-move-down))

(after! projectile

  (defun my/projectile-skel-variable-cons ()
    "Insert a variable-name and a value in a cons-cell.

This function is better than `projectile-skel-variable-cons'
because it allows `keyboard-quit' to exit skeleton insertion
without deleting what has already been inserted. Additionally,
this function constructs cons cells atomically (both the car and
cdr must be present), and a newline is inserted after each cons
cell for better formatting at the end of the skeleton inserted by
`my/projectile-skel-dir-locals'."
    (condition-case err
        (let* ((variable (projectile-read-variable))
               (value (string-trim (read-from-minibuffer
                                    (format "Value of [%s]: " variable)))))
          (format "(%s . %s)\n" variable value))
      (quit nil)))

  (define-skeleton my/projectile-skel-dir-locals
    "Insert a .dir-locals.el template.

This function fixes `projectile-skel-dirs-locals' by relying on
`my/projectile-skel-variable-cons' for cons insertion, allowing
for atomic insertion of cons cells and escaping at any time via
`keyboard-quit' (\\[keyboard-quit]).

Furthermore, trailing parentheses at the end of the lisp data are
properly inserted without a leading linebreak. This is acheived
by using the `>' skeleton token in conjunction with literal
newlines (rather than the `\\n' skeleton token), and then
deleting the final newline before inserting the \")))\"."
    nil
    ;; Ensure that EOL is represented by "\n" in this buffer
    '(setq buffer-file-coding-system 'utf-8-unix)
    "((nil . ("
    ("" > (skeleton-read #'my/projectile-skel-variable-cons nil t))
    & -1 ;; If any cons cells were inserted, remove the previous "\n"
    ")))")

  (advice-add 'projectile-skel-dir-locals
              :override #'my/projectile-skel-dir-locals))

(setq disabled-command-function nil)

(when NATIVECOMP
  (setq native-comp-speed 2
        package-native-compile t))

(setq confirm-kill-processes nil)

(after! projectile
  ;; For each atom in `obarray'
  (mapatoms
   (lambda (symbol)
     ;; When the atom is a `projectile' variable
     (when
         (and (boundp symbol)
              (not (keywordp symbol))
              (string-prefix-p "projectile-" (symbol-name symbol)))
       ;; The variable is safe when ...
       (put symbol 'safe-local-variable
            (lambda (_)
              (when
                  ;; ... we are in either XDG_DOCUMENTS_DIR or DOOMDIR
                  ;; TODO See about using `projectile-project-search-path'
                  (-select
                   (lambda (dir)
                     (string-match-p dir (expand-file-name default-directory)))
                   (list (file-name-as-directory (xdg-user-dir "DOCUMENTS"))
                         doom-private-dir))
                t)))))))

;; This should already be enabled by emacs/undo/config.el
(global-undo-tree-mode)

;; Not sure what the best way is to add this hook
(add-hook 'evil-local-mode-hook #'turn-on-undo-tree-mode)

(after! git-commit
  (delq! 'overlong-summary-line git-commit-style-convention-checks))

(add-to-list 'auto-mode-alist '("/git/config\\.d/.+" . gitconfig-mode))

(defun my/with-editor-export ()
  "Run `with-editor-export-editor' for all envvars that I care about"
  (dolist (var '("EDITOR" "VISUAL"))
    (with-editor-export-editor var)))

(set-eshell-alias!
  ;; C-x [0123]
  "0" "delete-window"
  "1" "delete-other-windows"
  "2" "split-window-below"
  "3" "split-window-right"

  ;; find-file
  "e"  "find-file $1"
  "4e" "find-file-other-window $1"
  "5e" "find-file-other-frame $1"

  ;; find-file-read-only
  "r"  "find-file-read-only $1"
  "4r" "find-file-read-only-other-window $1"
  "5r" "find-file-read-only-other-frame $1"

  ;; view-file
  "v"  "view-file $1"
  "4v" "view-file-other-window"
  "5v" "view-file-other-frame"

  ;; eww-open-file
  "w" "eww-open-file $1"

  ;; shell commands
  "git" "git --no-pager -c color.ui=always -c interactive.singleKey=false $*"
  "f"   "cd $1 && ls")

(after! eshell
  (setq eshell-scroll-show-maximum-output nil))

(after! eshell
  (advice-add 'eshell/clear :override #'recenter-top-bottom))

(add-hook! eshell-mode #'my/with-editor-export)

(add-hook! vterm-mode #'my/with-editor-export)

(after! vterm
  (defalias 'vterm-send-C-m #'vterm-send-return))

(after! evil-collection-vterm
  (dolist (key '("C-j" "<M-backspace>"))
    (evil-collection-define-key 'insert 'vterm-mode-map
      (kbd key) 'vterm--self-insert)))

(after! vterm

  ;; This function is to be called in shell configuration to obtain the
  ;; directory in which to read aliases
  (defun my/vterm--write-user-emacs-directory (tmpfile)
    "Write the string evaluation of `user-emacs-directory' to TMPFILE."
    (f-write user-emacs-directory 'utf-8 tmpfile))
  (add-to-list 'vterm-eval-cmds '("my/vterm--write-user-emacs-directory"
                                  my/vterm--write-user-emacs-directory))

  (defun my/set-vterm-alias (&rest aliases)
    "Define aliases for vterm.

ALIASES is a flat list of alias -> command pairs. e.g.

  (my/set-vterm-alias
    \"e\" \"find-file\"
    \"r\" \"find-file-read-only\")"
    (or (cl-evenp (length aliases))
        (signal 'wrong-number-of-arguments (list 'even (length aliases))))
    (with-temp-file (concat user-emacs-directory "vterm-aliases.sh")
      (while aliases
        (let ((alias (pop aliases))
              (command (pop aliases)))
          (add-to-list 'vterm-eval-cmds (list command (intern command)))
          (insert (format "alias %s='vterm_cmd %s'\n" alias command))))))

  (my/set-vterm-alias
    ;; Define aliases to open files in Emacs
    "e"  "find-file"
    "4e" "find-file-other-window"
    "5e" "find-file-other-frame"

    ;; Define aliases to open files in Emacs (read-only)
    "r"  "find-file-read-only"
    "4r" "find-file-read-only-other-window"
    "5r" "find-file-read-only-other-frame"

    ;; Define aliases to prevent issues with recursive editing
    "vi"    "find-file"
    "vim"   "find-file"
    "emacs" "find-file"

    ;; Define aliases for standalone commands that have Emacs equivalents
    "man"  "man"
    "info" "info"

    ;; Define aliases for special Emacs functionality
    "w"    "eww-open-file"
    "gg"   "magit-status"))

(after! flycheck
  (defun my/flycheck-set-level (level)
    "Set the Flycheck error level"
    (interactive
     (list (flycheck-read-error-level
            "Minimum error level (errors at lower levels will be hidden): ")))
    (when (and level (not (flycheck-error-level-p level)))
      (user-error "Invalid level: %s" level))

    ;; Hide errors in the error list that have a level lower than `level'
    (flycheck-error-list-set-filter level)
    (with-current-buffer (or flycheck-error-list-source-buffer (current-buffer))
      (setq-local flycheck-error-list-minimum-level level)

      ;; Only navigate between errors in the source buffer than have a level of
      ;; at least `level' (other errors will still be displayed)
      (setq-local flycheck-navigation-minimum-level level)))

  (defun my/flycheck-reset-level (&optional refresh)
    "Reset the Flycheck error level"
    (interactive '(t))

    ;; Refresh the error list according to the global value of
    ;; `flycheck-error-list-minimum-level'
    (flycheck-error-list-reset-filter refresh)
    (with-current-buffer (or flycheck-error-list-source-buffer (current-buffer))
      (kill-local-variable 'flycheck-error-list-minimum-level)

      ;; Refresh navigation between errors in the source buffer according to the
      ;; global value of `flycheck-navigation-minimum-level'
      (kill-local-variable 'flycheck-navigation-minimum-level))))

(mapc (lambda (config-file-dir)
        (add-to-list '+emacs-lisp-disable-flycheck-in-dirs config-file-dir))
      ;; Unique directory components of canonical config file paths
      (delete-dups
       (mapcar (lambda (config-file)
                 (file-name-directory (file-chase-links config-file)))
               ;; Config file paths in canonical config directories
               (mapcan (lambda (config-dir)
                         (directory-files config-dir t "\\.el"))
                       (list (file-truename doom-emacs-dir)
                             (file-truename doom-private-dir))))))

(when (and (featurep! :checkers spell)
           (not (featurep! :checkers spell +flyspell)))
  (remove-hook 'text-mode-hook 'spell-fu-mode))

(require 'spell-fu)

(setq ispell-dictionary "english")

(after! lsp
  (setq lsp-restart 'auto-restart))

(setq magit-repository-directories
      '(("~/Documents/src" . 2)
        ("~/Documents/ref" . 1)))

(when IS-MAC
  (setq ;; Comfortable keys that work most of the time
        mac-command-modifier 'control
        mac-right-command-modifier 'meta

        ;; Workaround for when system keybindings take precedence
        mac-control-modifier 'control
        mac-right-control-modifier 'meta

        ;; For exotic mappings
        mac-option-modifier 'super
        mac-right-option-modifier 'hyper))

(after! cc-mode
  (unless (stringp c-default-style)
    (if (assoc 'c-mode c-default-style)
        ;; Modify existing `c-mode' cons cell in `c-default-style'
        (setf (alist-get 'c-mode c-default-style) "linux")
      ;; Insert a new `c-mode' cons cell into `c-default-style'
      (setq c-default-style
            (cons '(c-mode . "linux")
                  c-default-style))))

  ;; REVIEW Not sure why this is necessary
  (add-hook! 'c-mode-hook
   (setq tab-width
         (alist-get 'c-basic-offset (assoc "linux" c-style-alist)))))

(after! ws-butler
  (pushnew! ws-butler-global-exempt-modes 'tsv-mode))

(add-to-list 'auto-mode-alist '("Cask\\'" . lisp-data-mode))

(add-to-list 'auto-mode-alist '("\\.npmignore\\'" . gitignore-mode))

(add-to-list 'font-lock-extra-managed-props 'display)
(font-lock-add-keywords
 'markdown-mode
 '(("\\(\\\\\\)[[().-]" 1 '(face nil display ""))
   ("&copy;" 0 '(face nil display "©"))
   ("<a name=\".*\"></a>" 0 '(face nil display ""))))

(after! markdown
  (defun my/markdown-preview (f &rest r)
    (let ((browse-url-browser-function #'browse-url-default-browser))
      (apply f r)))
  (advice-add 'markdown-preview :around #'my/markdown-preview))

;; Prevent flycheck from being automatically enabled
(if (or (not (boundp 'flycheck-global-modes))
        (not (eq 'not (car flycheck-global-modes))))
    (setq flycheck-global-modes '(not markdown-mode))
  (let ((modes (cdr flycheck-global-modes)))
    (setcdr flycheck-global-modes (pushnew! modes 'markdown-mode))))

;; Prevent lsp diagnostics from being enabled
(if (boundp 'lsp-diagnostics-disabled-modes)
    (pushnew! lsp-diagnostics-disabled-modes 'markdown-mode)
  (setq lsp-diagnostics-disabled-modes '(markdown-mode)))

;; Don't bother checking for an LSP diagnostics provider in sh-mode
(setq-hook! 'markdown-mode-hook
  lsp-diagnostics-provider :none)

(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))

(after! org
  (setq org-hide-leading-stars nil
        org-startup-indented nil
        org-adapt-indentation nil))

(after! org
  (setcdr (assoc 'heading org-blank-before-new-entry) nil)
  (defun my/org-insert-heading-spacing ()
    "Surround an Org heading with appropriate whitespace.

This is the general idea:

* A
* B
Entry content

** B.1
** B.2
:PROPERTIES:...

** B.3
* C (intentional blank line in entry)

* D
"
    ;; Delete all blank lines following the heading
    (delete-blank-lines)
    ;; Set spacing appropriately before the heading
    (save-excursion
      (forward-line -1)
      ;; What immediately precedes the heading line?
      (cond
       ;; Another heading line (or same heading at beginning of buffer) => do nothing
       ((org-at-heading-p) t)
       ;; Blank line => squeeze consecutive blank lines
       ((looking-at-p "[[:blank:]]*$") (delete-blank-lines))
       ;; Non-blank non-heading line => insert a blank line before the heading
       (t (forward-line 1) (newline)))))
  (defun my/org-insert-heading-visibility ()
    "Redisplay the previous Org heading.

I cannot figure out where the visibility state is saved, so I
just perform a complete cycle of `org-cycle'."
    (save-window-excursion
      (save-excursion
        (org-previous-visible-heading 1)
        ;; XXX Doom-specific
        (when evil-mode (evil-normal-state))
        (call-interactively #'org-cycle)
        (call-interactively #'org-cycle))))
  (defun my/org-insert-heading-evil-state ()
    "End up with the cursor in 'insert mode' at the end of the Org heading"
    ;; XXX Doom-specific
    (when evil-mode (evil-org-append-line 1)))
  (add-hook! org-insert-heading #'my/org-insert-heading-spacing
                                #'my/org-insert-heading-visibility
                                #'my/org-insert-heading-evil-state))

(after! org
  (setcdr (assoc 'file org-link-frame-setup) #'find-file-other-window))

(after! org
  (defun my/org-inherited-priority (s)
    (cond
     ;; Priority cookie in this heading
     ((string-match org-priority-regexp s)
      (* 1000 (- org-priority-lowest
                 (org-priority-to-value (match-string 2 s)))))
     ;; No priority cookie, but already at highest level
     ((not (org-up-heading-safe))
      (* 1000 (- org-priority-lowest org-priority-default)))
     ;; Look for the parent's priority
     (t
      (my/org-inherited-priority (org-get-heading)))))
  (setq org-priority-get-priority-function #'my/org-inherited-priority))

;; REVIEW See if there is a cleaner way to temporarily set `org-log-into-drawer'
(after! org
  (defun my/org-add-note-advice (f &rest r)
    (let ((restore org-log-into-drawer))
      (setq org-log-into-drawer t)
      (apply f r))
      (setq org-log-into-drawer restore))
  (advice-add 'org-add-note :around #'my/org-add-note-advice))

(after! org
  (setq org-refile-use-outline-path 'full-file-path))

(setq
      ;; Top-level directory (used by `+default/find-in-notes', etc.)
      org-directory "~/org"

      ;; Directories to search for agenda files
      my/org-directories `("work" "life" ,doom-private-dir)
      org-agenda-files (mapcar (lambda (f)
                                 (if (file-name-absolute-p f) f
                                   (expand-file-name f org-directory)))
                               `("" ,@my/org-directories))

      ;; Only "todo.org" files hold agenda items
      org-agenda-file-regexp "\\`todo.org\\'")

(after! org
  (setcar (nthcdr 4 (assoc "t" org-capture-templates)) "* TODO %?") ;; And replace "[ ]"
  (setcar (nthcdr 4 (assoc "n" org-capture-templates)) "* %u %?")
  (setcar (nthcdr 4 (assoc "j" org-capture-templates)) "* %U %?"))

(setq org-agenda-prefix-format
      '((agenda  . " %i  %l%-16:c%?-12t% s")
        (todo    . " %i  %l%-16:c")
        (tags    . " %i  %l%-16:c")))

(setq org-agenda-hide-tags-regexp "\\`work\\|life\\|doom\\|todo\\'")

(setq org-agenda-category-icon-alist
      `(("/inbox\\'"           (,(all-the-icons-faicon     "inbox"      nil nil :height 1.00 :face 'all-the-icons-dred)))
        ;; work/*
        ("\\`work/admin\\'"    (,(all-the-icons-faicon     "fax"        nil nil :height 0.85 :face 'all-the-icons-lred)))
        ("\\`work/oncall\\'"   (,(all-the-icons-faicon     "users"      nil nil :height 0.80 :face 'all-the-icons-lyellow)))
        ("\\`work/cots\\'"     (,(all-the-icons-faicon     "server"     nil nil :height 0.85 :face 'all-the-icons-dorange)))
        ("\\`work/metrics\\'"  (,(all-the-icons-faicon     "eye"        nil nil :height 0.85 :face 'all-the-icons-dmaroon)))
        ("\\`work/infra\\'"    (,(all-the-icons-faicon     "cubes"      nil nil :height 0.65 :face 'all-the-icons-lorange)))
        ("\\`work/sdlc\\'"     (,(all-the-icons-faicon     "pencil"     nil nil :height 0.95 :face 'all-the-icons-orange)))
        ;; life/*
        ("\\`life/family\\'"   (,(all-the-icons-faicon     "heart"      nil nil :height 0.85 :face 'all-the-icons-red)))
        ("\\`life/money\\'"    (,(all-the-icons-faicon     "money"      nil nil :height 0.80 :face 'all-the-icons-dgreen)))
        ("\\`life/tech\\'"     (,(all-the-icons-faicon     "laptop"     nil nil :height 0.80 :face 'all-the-icons-dsilver)))
        ;; doom/*
        ("\\`doom/upstream\\'" (,(all-the-icons-alltheicon "git"        nil nil :height 0.85 :face 'all-the-icons-lred)))
        ("\\`doom/config\\'"   (,(all-the-icons-fileicon   "emacs"      nil nil :height 0.85 :face 'all-the-icons-purple)))
        ("\\`doom/org\\'"      (,(all-the-icons-fileicon   "org"        nil nil :height 0.90 :face 'all-the-icons-lgreen)))
        ("\\`doom/markdown\\'" (,(all-the-icons-octicon    "markdown"   nil nil :height 0.85 :face 'all-the-icons-maroon)))
        ("\\`doom/yaml\\'"     (,(all-the-icons-faicon     "cogs"       nil nil :height 0.80 :face 'all-the-icons-lsilver)))
        ("\\`doom/python\\'"   (,(all-the-icons-alltheicon "python"     nil nil :height 0.85 :face 'all-the-icons-dblue)))
        ("\\`doom/ts\\'"       (,(all-the-icons-fileicon   "typescript" nil nil :height 0.85 :face 'all-the-icons-blue)))
        ("\\`doom/term\\'"     (,(all-the-icons-faicon     "terminal"   nil nil :height 0.95 :face 'all-the-icons-dgreen)))
        ("\\`doom/misc\\'"     (,(all-the-icons-fileicon   "config"     nil nil :height 0.85 :face 'all-the-icons-lblue)))))

(after! org
  (defun my/org-footnote-sort ()
    "Rearrange footnote definitions in the current buffer.
Sort footnote definitions so they match order of footnote
references.  Also relocate definitions at the end of their
relative section or within a single footnote section, according
to `org-footnote-section'.  Inline definitions are ignored."
    (let ((references (org-footnote--collect-references)))
      (org-preserve-local-variables
       (let ((definitions (org-footnote--collect-definitions 'delete)))
         (org-with-wide-buffer
          (org-footnote--clear-footnote-section)
          ;; Insert footnote definitions at the appropriate location,
          ;; separated by a blank line.  Each definition is inserted
          ;; only once throughout the buffer.
          (let (inserted)
            (dolist (cell references)
              (let ((label (car cell))
                    (nested (not (nth 2 cell)))
                    (inline (nth 3 cell)))
                (unless (or (member label inserted) inline)
                  (push label inserted)
                  (unless (or org-footnote-section nested)
                    ;; If `org-footnote-section' is non-nil, or
                    ;; reference is nested, point is already at the
                    ;; correct position.  Otherwise, move at the
                    ;; appropriate location within the section
                    ;; containing the reference.
                    (goto-char (nth 1 cell))
                    (org-footnote--goto-local-insertion-point))
                  (insert (or (cdr (assoc label definitions))
                              (format "[fn:%s] DEFINITION NOT FOUND." label))
                          "\n"))))
            ;; Insert un-referenced footnote definitions at the end.
            (pcase-dolist (`(,label . ,definition) definitions)
              (unless (member label inserted)
                (insert definition "\n")))))))))
  (advice-add 'org-footnote-sort :override #'my/org-footnote-sort))

(after! org
  (setq org-src-preserve-indentation nil
        org-edit-src-content-indentation 0))

(after! org
  (setq org-src-window-setup 'current-window)
  (setq +popup--display-buffer-alist
        (delq (assoc "^\\*Org Src" +popup--display-buffer-alist)
              +popup--display-buffer-alist))
  (when (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist)))

(after! projectile
  (pushnew! projectile-other-file-alist
            '("org" "el")
            '("el" "org")))

(setq org-ditaa-jar-path
      (cond (IS-MAC
             (file-expand-wildcards "/usr/local/Cellar/ditaa/*/libexec/ditaa-*-standalone.jar"))))

(setq org-download-method 'directory
      org-download-image-dir "images")

(setq org-ditaa-jar-path
      (cond (IS-MAC
             (file-expand-wildcards "/usr/local/Cellar/ditaa/*/libexec/ditaa-*-standalone.jar"))))

(require 'org-pomodoro)

(when (equal org-pomodoro-audio-player "/usr/bin/afplay")
  (let ((args '("-v" "0.125")))
    (setq org-pomodoro-start-sound-args args
          org-pomodoro-finished-sound-args args
          org-pomodoro-overtime-sound-args args
          org-pomodoro-ticking-sound-args args
          org-pomodoro-killed-sound-args args
          org-pomodoro-short-break-sound-args args
          org-pomodoro-long-break-sound-args args)))

(map! :map org-mode-map
      :localleader
      (:when (featurep! :lang org +pomodoro)
       (:prefix ("c" . "clock")
        "p" #'org-pomodoro
        "P" #'org-pomodoro-extend-last-clock)))

(add-hook! python-mode
  (setq fill-column 79)
  (display-fill-column-indicator-mode))

(add-to-list 'auto-mode-alist '("pylint" . conf-mode))

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(setq-default sh-shell-file "/bin/sh")

;; Prevent flycheck from being automatically enabled
(if (or (not (boundp 'flycheck-global-modes))
        (not (eq 'not (car flycheck-global-modes))))
    (setq flycheck-global-modes '(not sh-mode))
  (let ((modes (cdr flycheck-global-modes)))
    (setcdr flycheck-global-modes (pushnew! modes 'sh-mode))))

;; Prevent lsp diagnostics from being enabled
(if (boundp 'lsp-diagnostics-disabled-modes)
    (pushnew! lsp-diagnostics-disabled-modes 'sh-mode)
  (setq lsp-diagnostics-disabled-modes '(sh-mode)))

;; Don't bother checking for an LSP diagnostics provider in sh-mode
(setq-hook! 'sh-mode-hook
  lsp-diagnostics-provider :none)

(dolist (re '("/\\.config/\\(shell\\|bash\\)/.+"
              "\\.\\(env\\|cygport\\)\\'"))
  (add-to-list 'auto-mode-alist
               `(,re . shell-script-mode)))

(after! lsp-yaml
  (let ((f lsp-yaml-schema-store-local-db))
    (unless (file-exists-p f)
      (mkdir (file-name-directory f) t)
      (lsp-yaml-download-schema-store-db))))

(put 'lsp-yaml-schemas 'safe-local-variable #'always)

(after! elfeed
  (add-hook! 'elfeed-search-mode-hook #'elfeed-update))

(after! elfeed
  ;; Do not truncate RSS entry titles
  (setq elfeed-search-title-max-width 1000)

  ;; Do not truncate RSS entry tags (just need to shift left by 2 characters)
  (defun my/elfeed-format-column (string width &optional align)
    "Return STRING truncated or padded to WIDTH - 2 following ALIGNment.
  Align should be a keyword :left or :right."
    (if (<= width 0)
        ""
      (format (format "%%%s%d.%ds" (if (eq align :left) "-" "") (- width 2) (- width 2))
              string)))
  (advice-add 'elfeed-format-column :override #'my/elfeed-format-column))

(after! elfeed
  (setq elfeed-search-remain-on-entry t))

;; REVIEW See if there is a cleaner way to flatten the `mapcan' list result
(after! projectile
  (eval
   `(pushnew!
     projectile-globally-ignored-directories
     ,@(mapcan
        (lambda (f)
          (when (file-directory-p f)
            (list (abbreviate-file-name f))))
        (directory-files (format "%s/.local/straight/repos" doom-emacs-dir)
                                   t "\\`[^.]")))))

(load! "custom" doom-private-dir t)

(remove-hook 'org-mode-hook #'+literate-enable-recompile-h)

(after! smartparens
  (let* ((default-pairs (cdr (assoc t sp-pairs)))
         (default-openers (mapcar (lambda (pair) (plist-get pair :open))
                                  default-pairs)))
    (dolist (opener default-openers)
      (sp-local-pair 'minibuffer-inactive-mode opener nil :actions nil))))

(setq which-key-idle-delay 0.5
      which-key-idle-secondary-delay 0.1)

(defun my/alternate-keys (key &optional insert)
  "Print message listing equivalent alternate key sequences for KEY.
KEY is a pair (SEQ . RAW-SEQ) of key sequences, where
RAW-SEQ is the untranslated form of the key sequence SEQ.
If INSERT (the prefix arg) is non-nil, insert the message in the buffer.
While reading KEY interactively, this command temporarily enables
menu items or tool-bar buttons that are disabled to allow getting help
on them."
  (interactive
   ;; Ignore mouse movement events because it's too easy to miss the
   ;; message while moving the mouse.
   (list (car (help--read-key-sequence 'no-mouse-movement)) current-prefix-arg))
  (where-is (cadr (help--analyze-key (car key) (cdr key))) insert))

(define-key! help-map
  "C-f" #'find-function      ;; replaces `view-emacs-FAQ' b/c I rarely use it
  "C-l" #'find-library       ;; replaces `describe-language-environment'
  "C-v" #'find-variable
  "C-w" #'my/alternate-keys) ;; replaces `describe-no-warranty' b/c I never use it

(remove-hook 'text-mode-hook #'display-line-numbers-mode)

(defun my/toggle-line-numbers ()
  "Toggle line numbers.

Cycles through regular, relative and no line numbers. If you're
using Emacs 26+, and `visual-line-mode' is on, this skips relative
and uses visual instead."
  (interactive)
  (cond
   ((not display-line-numbers)
    (setq display-line-numbers t)
    (message "Switched to normal line numbers"))
   ((memq display-line-numbers '(visual relative))
    (setq display-line-numbers nil)
    (message "Switched to disabled line numbers"))
   (visual-line-mode
    (setq display-line-numbers 'visual)
    (message "Switched to visual line numbers"))
   (t
    (setq display-line-numbers 'relative)
    (message "Switched to relative line numbers"))))

(define-key! doom-leader-toggle-map
  ;; replaces `doom/toggle-line-numbers'
  "l" #'my/toggle-line-numbers)

(defun my/doom-help-search-source (&optional initial-input)
  "Perform a text search across all files in `doom-emacs-dir'."
  (interactive)
  (+ivy-file-search
    :query initial-input
    :in doom-emacs-dir
    :prompt (format "Search source for: ")))

(defun my/doom-help-search-modules (&optional initial-input)
  "Perform a text search across all files in `doom-modules-dir'."
  (interactive)
  (+ivy-file-search
    :query initial-input
    :in doom-modules-dir
    :prompt "Search modules for: "))

(define-key! help-map
  "de" #'my/doom-help-search-source
  "dM" #'my/doom-help-search-modules)

(when (fboundp 'find-sibling-file)
  ;; Same directory, same base file name, different extension
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\..*\\'" "\\1\\..*\\'"))
  (define-key! doom-leader-file-map
    "o" #'find-sibling-file)
  (after! which-key
    (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
      (cl-pushnew `((,(format "\\`%s f o\\'" prefix-re)) nil . "Find other file")
                  which-key-replacement-alist))))

;; Function to toggle 1 or 2 spaces at the end of sentences
(defun my/toggle-sentence-end-double-space ()
  (interactive)
  (if (not sentence-end-double-space)
      (progn
        (setq-local sentence-end-double-space t)
        (message "Sentences end with 2 spaces"))
    (setq-local sentence-end-double-space nil)
    (message "Sentences end with 1 space")))

;; REVIEW See if there is a better way to do this (e.g. with `map!' or a custom macro)
(define-key! doom-leader-toggle-map
  "a" #'auto-fill-mode
  "B" #'display-battery-mode
  "c" #'display-fill-column-indicator-mode
  "C" #'column-highlight-mode
  "h" #'use-hard-newlines
  "L" #'hl-line-mode
  "o" #'overwrite-mode
  "p" #'page-break-lines-mode
  "t" #'toggle-truncate-lines
  "|" #'visual-fill-column-mode
  "." #'my/toggle-sentence-end-double-space
  "SPC" #'whitespace-mode)
(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`%s t a\\'" prefix-re)) nil . "Auto fill")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`%s t B\\'" prefix-re)) nil . "Battery indicator")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`%s t c\\'" prefix-re)) nil . "Fill column indicator")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`%s t C\\'" prefix-re)) nil . "Column highlight")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`%s t h\\'" prefix-re)) nil . "Hard newlines")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`%s t L\\'" prefix-re)) nil . "Line highlight")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`%s t o\\'" prefix-re)) nil . "Overwrite")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`%s t p\\'" prefix-re)) nil . "Page break lines")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`%s t t\\'" prefix-re)) nil . "Truncate lines")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`%s t |\\'" prefix-re)) nil . "Visual fill column")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`%s t \\.\\'" prefix-re)) nil . "Sentence spacing")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`%s t SPC\\'" prefix-re)) nil . "Whitespace mode")
                which-key-replacement-alist)))
