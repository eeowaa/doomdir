;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;;; completion/ivy

;; Make it easier to jump to headlines across Org buffers
(setq counsel-org-goto-all-outline-path-prefix 'buffer-name)


;;; ui/doom

;; Set the font and theme
;; PREREQ brew install font-terminus
(setq doom-font (font-spec :family "Menlo" :size 16)
      doom-theme 'doom-henna)

;;; ui/fill-column

;; FIXME [emacs-2]
;; Do not display fill column indicator by default
(remove-hook! '(text-mode-hook prog-mode-hook conf-mode-hook)
              #'+fill-column-enable-h)

;;; ui/hl-todo

;; Add a couple different tags to highlight in programming major-modes
(after! hl-todo
  (setq hl-todo-keyword-faces
        (append '(("TESTME" font-lock-constant-face bold)
                  ("PREREQ" font-lock-doc-face bold))
                hl-todo-keyword-faces)))

;;; ui/hydra

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
(global-set-key (kbd "C-c g") #'hydra-game/body)

;;; ui/ligatures

;; Only enable extra ligatures in Org mode (for now)
(setq +ligatures-extras-in-modes '(org-mode))

;;; ui/modeline

;; Use 1-based column numbering in modeline
(setq column-number-indicator-zero-based nil)

;;; ui/popup

;; Do not open (Wo)Man buffers in a popup window
(setq +popup--display-buffer-alist
      (delq (assoc "^\\*\\(?:Wo\\)?Man " +popup--display-buffer-alist)
            +popup--display-buffer-alist))
(when (bound-and-true-p +popup-mode)
  (setq display-buffer-alist +popup--display-buffer-alist))

;; Open manpages in the current window
(setq Man-notify-method 'pushy)

;; TODO Modify popup behavior for Customize buffers

;;; ui/treemacs

;; TODO Fix size of NPM logo


;;; editor/evil

;; Define modes that should always come up in Emacs state
(pushnew! evil-emacs-state-modes 'noaa-mode)


;;; emacs/undo

;; Enable `undo-tree'
(global-undo-tree-mode)


;;; term/eshell

;; Do not scroll after every command
(setq eshell-scroll-show-maximum-output nil)

;; Add aliases
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
  "git" "TERM=eterm-color git --no-pager -c color.ui=always -c interactive.singleKey=false $*"
  "f"   "cd $1 && ls")

;; NOTE Unlike with `vterm', we do not export $EDITOR to `eshell'. This is to
;; avoid a dangerous situation in which C-c C-k not only returns an error code
;; to `eshell', it forcefully clears the file on disk.

;;; term/vterm

;; Export $EDITOR to `vterm'
(add-hook! vterm-mode #'with-editor-export-editor)

;; Let C-j and <M-backspace> pass through in insert mode
(after! evil-collection-vterm
  (dolist (key '("C-j" "<M-backspace>"))
    (evil-collection-define-key 'insert 'vterm-mode-map
      (kbd key) 'vterm--self-insert)))


;;; os/macos

;; Remap keys for macOS
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

;; TODO Experiment with Keychain as a member of `auth-sources'


;;; lang/markdown

;; FIXME [emacs-1]
;; Improve Markdown readability
;; <https://emacs.stackexchange.com/questions/3753/prettify-symbols-mode-character-replacement-regex>
(add-to-list 'font-lock-extra-managed-props 'display)
(font-lock-add-keywords
 'markdown-mode
 '(("<a name=\".*\"></a>" 0 '(face nil display "")) ; <a name=".*"></a> -> ""
   ("\\(\\\\\\)[[().-]" 1 '(face nil display ""))   ; "\?"              -> "?"
   ("&copy;" 0 '(face nil display "©"))))           ; "&copy;"          -> "©"

;;; lang/org

;; PREREQ mkdir -p ~/Documents/org && ln -s ~/Documents/org ~/org
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

;; Do not display file tags in the agenda
(setq org-agenda-hide-tags-regexp "\\`work\\|life\\|doom\\|todo\\'")

;; Widen the agenda prefix and indent subtasks
(setq org-agenda-prefix-format
      '((agenda  . " %i  %l%-16:c%?-12t% s")
        (todo    . " %i  %l%-16:c")
        (tags    . " %i  %l%-16:c")))

;; TODO Display weekly/daily agenda view properly (what's wrong?)

;; Set icons for agenda prefix
(setq org-agenda-category-icon-alist
      `(("/inbox\\'"           (,(all-the-icons-faicon     "inbox"      nil nil :height 1.00 :face 'all-the-icons-dred)))
        ;; work/*
        ("\\`work/admin\\'"    (,(all-the-icons-faicon     "fax"        nil nil :height 0.85 :face 'all-the-icons-lred)))
        ("\\`work/oncall\\'"   (,(all-the-icons-faicon     "users"      nil nil :height 0.80 :face 'all-the-icons-lyellow)))
        ("\\`work/cots\\'"     (,(all-the-icons-faicon     "server"     nil nil :height 0.85 :face 'all-the-icons-dorange)))
        ("\\`work/metrics\\'"  (,(all-the-icons-faicon     "eye"        nil nil :height 0.85 :face 'all-the-icons-dmaroon)))
        ("\\`work/infra\\'"    (,(all-the-icons-faicon     "cubes"      nil nil :height 0.65 :face 'all-the-icons-lorange)))
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
        ("\\`doom/misc\\'"     (,(all-the-icons-fileicon   "config"     nil nil :height 0.85 :face 'all-the-icons-lblue)))))

(after! org
  ;; Proper indentation for source blocks
  ;; <https://emacs.stackexchange.com/a/9483/21977>
  (setq org-src-preserve-indentation nil
        org-edit-src-content-indentation 0)

  ;; TODO Prefix refile targets with the directory name (file tag)
  (setq org-refile-use-outline-path 'full-file-path)

  ;; Inherit priority so that subtasks appear under their parents in the agenda
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
  (setq org-priority-get-priority-function #'my/org-inherited-priority)

  ;; Do not indent org headlines
  (setq org-hide-leading-stars nil
        org-startup-indented nil
        org-adapt-indentation nil)

  ;; Show edit buffer in the current window
  (setq org-src-window-setup 'current-window)

  ;; REVIEW See if there is a cleaner way to temporarily set `org-log-into-drawer'
  ;; Insert notes into :LOGBOOK: drawer without logging state changes
  (defun my/org-add-note-advice (f &rest r)
    (let ((restore org-log-into-drawer))
      (setq org-log-into-drawer t)
      (apply f r))
      (setq org-log-into-drawer restore))
  (advice-add #'org-add-note :around #'my/org-add-note-advice)

  ;; Remove file links from personal org capture templates
  (setcar (nthcdr 4 (assoc "t" org-capture-templates)) "* TODO %?") ;; And replace "[ ]"
  (setcar (nthcdr 4 (assoc "n" org-capture-templates)) "* %u %?")
  (setcar (nthcdr 4 (assoc "j" org-capture-templates)) "* %U %?")

  ;; Insert new headings just how I like them
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
  (add-hook! org-insert-heading #'(my/org-insert-heading-spacing
                                   my/org-insert-heading-visibility
                                   my/org-insert-heading-evil-state)))

;; HACK In order to maintian proper spacing of Org footnotes, I had to redefine
;; the `org-footnote-sort' function to *not* insert a leading "\n" before new
;; footnote definitions.
(defun org-footnote-sort ()
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

;;; lang/yaml

;; FIXME The implementation of `lsp-yaml-download-schema-store-db' is buggy
;; Download the schema definitions if we don't have them already
(after! lsp-yaml
  (let ((f lsp-yaml-schema-store-local-db))
    (unless (file-exists-p f)
      (mkdir (file-name-directory f) t)
      (lsp-yaml-download-schema-store-db))))


;;; config/default

;; Use absolute line numbers
(setq display-line-numbers-type t)

;; Do not display line numbers in text mode
(remove-hook 'text-mode-hook #'display-line-numbers-mode)

;; Adjust `which-key' timing
(setq which-key-idle-delay 0.5
      which-key-idle-secondary-delay 0.1)

;; Do not auto-insert pairs of quotes
(sp-pair "\"" nil :actions :rem)
(sp-pair "'"  nil :actions :rem)
(sp-pair "`"  nil :actions :rem)

;; Disable smartparens in the minibuffer, including the `evil-ex' prompt...
;; According to the documentation, each mode listed in `sp-ignore-modes-list'
;; will have smartparens disabled when `smartparens-global-mode' is active.
;; However, this does not appear to be functioning properly, at least for
;; `minibuffer-inactive-mode', so I get around this by overriding all the pair
;; insertion rules for that specific mode.
(after! smartparens
  (let* ((default-pairs (cdr (assoc t sp-pairs)))
         (default-openers (mapcar (lambda (pair) (plist-get pair :open))
                                  default-pairs)))
    (dolist (opener default-openers)
      (sp-local-pair 'minibuffer-inactive-mode opener nil :actions nil))))

;; Function to toggle 1 or 2 spaces at the end of sentences
(defun my/toggle-sentence-end-double-space ()
  (interactive)
  (if (not sentence-end-double-space)
      (progn
        (setq-local sentence-end-double-space t)
        (message "Sentences end with 2 spaces"))
    (setq-local sentence-end-double-space nil)
    (message "Sentences end with 1 space")))

;; REVIEW See if there is a better way to do this (e.g. with `map!')
;; Add additional toggle keybindings
;; TODO Add other toggles:
;; "Line"          hl-line-mode
;; "Column"        column-highlight-mode
;; "Battery"       display-battery-mode
;; "Visual Lines"  visual-line-mode
(define-key! doom-leader-toggle-map
  "a" #'auto-fill-mode
  "h" #'use-hard-newlines
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
    (cl-pushnew `((,(format "\\`%s t h\\'" prefix-re)) nil . "Hard newlines")
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

;; Improve builtin help
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
  ;; replaces `view-emacs-FAQ' b/c I rarely use it
  "C-f" #'find-function
  ;; replaces `describe-language-environment'
  "C-l" #'find-library
  ;; replaces `describe-no-warranty' b/c I never use it
  "C-w" #'my/alternate-keys)


;;; uncategorized

;; Keep icons small by default
(setq all-the-icons-scale-factor 1.0)

;; REVIEW See if there is a cleaner way to flatten the `mapcan' list result
;; Remove `straight' package repos from projectile
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

;; Give buffers unique names based on file path
(setq uniquify-buffer-name-style 'forward)

;; Enable all disabled commands
(setq disabled-command-function nil)

;; Display ^L characters as horizontal lines
(use-package! page-break-lines
  :config (global-page-break-lines-mode))

;; Have C-l send the current line to the top of the window
(setq recenter-positions '(top bottom middle))

;; Perform a line feed after jumping to a page break
(defun my/recenter-top (&rest r) (recenter 0))
(advice-add #'forward-page :after #'my/recenter-top)

;; Don't prompt about killing running processing when quitting
(setq confirm-kill-processes nil)

;; Don't suggest abbreviations for long command names
(setq extended-command-suggest-shorter nil)

;; Allow easy input of accented and special characters via C-\
(setq default-input-method "latin-postfix")

;; Truncate lines by default
(setq-default truncate-lines t)

;; Load custom config if present
(load! "custom" doom-private-dir t)

;; REVIEW Steal config stuff from <https://github.com/tecosaur/emacs-config/blob/master/config.org>
