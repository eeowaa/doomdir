(setq company-idle-delay nil)

(setq counsel-org-goto-all-outline-path-prefix 'buffer-name)

(setq doom-font (font-spec :family "Terminus (TTF)" :size 16)
      doom-theme 'doom-henna)

(remove-hook! '(text-mode-hook prog-mode-hook conf-mode-hook)
              #'+fill-column-enable-h)

(after! hl-todo
  (setq hl-todo-keyword-faces
        (append '(("TESTME" font-lock-constant-face bold)
                  ("PREREQ" font-lock-doc-face bold))
                hl-todo-keyword-faces)))

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

(setq +ligatures-extras-in-modes '(org-mode))

(setq column-number-indicator-zero-based nil)

(setq +popup--display-buffer-alist
      (delq (assoc "^\\*\\(?:Wo\\)?Man " +popup--display-buffer-alist)
            +popup--display-buffer-alist))
(when (bound-and-true-p +popup-mode)
  (setq display-buffer-alist +popup--display-buffer-alist))

(setq Man-notify-method 'pushy)

(pushnew! evil-emacs-state-modes 'noaa-mode)

(global-undo-tree-mode)

(setq eshell-scroll-show-maximum-output nil)

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

(add-hook! vterm-mode #'with-editor-export-editor)

(after! evil-collection-vterm
  (dolist (key '("C-j" "<M-backspace>"))
    (evil-collection-define-key 'insert 'vterm-mode-map
      (kbd key) 'vterm--self-insert)))

(setq ispell-dictionary "english")

(setq magit-repository-directories
      '(("~/Documents/src" . 2)
        ("~/Documents/ref" . 1)
        ("~/stow-dotfiles" . 1)))

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

(add-to-list 'font-lock-extra-managed-props 'display)
(font-lock-add-keywords
 'markdown-mode
 '(("\\(\\\\\\)[[().-]" 1 '(face nil display ""))
   ("&copy;" 0 '(face nil display "©"))
   ("<a name=\".*\"></a>" 0 '(face nil display ""))))

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

(setq org-agenda-prefix-format
      '((agenda  . " %i  %l%-16:c%?-12t% s")
        (todo    . " %i  %l%-16:c")
        (tags    . " %i  %l%-16:c")))

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
  (setq org-refile-use-outline-path 'full-file-path))

(after! org
  (setq org-src-preserve-indentation nil
        org-edit-src-content-indentation 0))

(after! org
  (setq org-hide-leading-stars nil
        org-startup-indented nil
        org-adapt-indentation nil))

(after! org
  (setq org-src-window-setup 'current-window))

(after! org
  (setcar (nthcdr 4 (assoc "t" org-capture-templates)) "* TODO %?") ;; And replace "[ ]"
  (setcar (nthcdr 4 (assoc "n" org-capture-templates)) "* %u %?")
  (setcar (nthcdr 4 (assoc "j" org-capture-templates)) "* %U %?"))

;; REVIEW See if there is a cleaner way to temporarily set `org-log-into-drawer'
(after! org
  (defun my/org-add-note-advice (f &rest r)
    (let ((restore org-log-into-drawer))
      (setq org-log-into-drawer t)
      (apply f r))
      (setq org-log-into-drawer restore))
  (advice-add #'org-add-note :around #'my/org-add-note-advice))

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

(setq org-ditaa-jar-path
      (cond (IS-MAC
             (file-expand-wildcards "/usr/local/Cellar/ditaa/*/libexec/ditaa-*-standalone.jar"))))

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
  (add-hook! org-insert-heading #'(my/org-insert-heading-spacing
                                   my/org-insert-heading-visibility
                                   my/org-insert-heading-evil-state)))

(after! lsp-yaml
  (let ((f lsp-yaml-schema-store-local-db))
    (unless (file-exists-p f)
      (mkdir (file-name-directory f) t)
      (lsp-yaml-download-schema-store-db))))

(setq lsp-yaml-schemas
      '((https://raw\.githubusercontent\.com/awslabs/goformation/v4\.15\.0/schema/cloudformation\.schema\.json
         . ["*.yaml" "*.yml"])))

(setq-hook! 'yaml-mode-hook lsp-diagnostic-package :none)

(after! flycheck
  (flycheck-define-checker cfn-lint
    "A CloudFormation linter using cfn-lint."
    :command ("cfn-lint" "-f" "parseable" source)
    :error-patterns ((warning line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "W" (one-or-more digit)) ":" (message) line-end)
                     (error line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "E" (one-or-more digit)) ":" (message) line-end))
    :modes (yaml-mode))
    (add-to-list 'flycheck-checkers 'cfn-lint))

(add-to-list 'auto-mode-alist '("\\.npmignore\\'" . gitignore-mode))

(sp-pair "\"" nil :actions :rem)
(sp-pair "'"  nil :actions :rem)
(sp-pair "`"  nil :actions :rem)

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
  ;; replaces `view-emacs-FAQ' b/c I rarely use it
  "C-f" #'find-function
  ;; replaces `describe-language-environment'
  "C-l" #'find-library
  ;; replaces `describe-no-warranty' b/c I never use it
  "C-w" #'my/alternate-keys)

;; Use absolute line numbers
(setq display-line-numbers-type t)

;; Do not display line numbers in text mode
(remove-hook 'text-mode-hook #'display-line-numbers-mode)

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

(after! org-babel
  (defun org-babel-tangle (&optional arg target-file lang-re)
    "Write code blocks to source-specific files.
Extract the bodies of all source code blocks from the current
file into their own source-specific files.
With one universal prefix argument, only tangle the block at point.
When two universal prefix arguments, only tangle blocks for the
tangle file of the block at point.
Optional argument TARGET-FILE can be used to specify a default
export file for all source blocks.  Optional argument LANG-RE can
be used to limit the exported source code blocks by languages
matching a regular expression."
    (interactive "P")
    (run-hooks 'org-babel-pre-tangle-hook)
    ;; Possibly Restrict the buffer to the current code block
    (save-restriction
      (save-excursion
        (when (equal arg '(4))
          (let ((head (org-babel-where-is-src-block-head)))
            (if head
                (goto-char head)
              (user-error "Point is not in a source code block"))))
        (let ((block-counter 0)
              (org-babel-default-header-args
               (if target-file
                   (org-babel-merge-params org-babel-default-header-args
                                           (list (cons :tangle target-file)))
                 org-babel-default-header-args))
              (tangle-file
               (when (equal arg '(16))
                 (or (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'light))))
                     (user-error "Point is not in a source code block"))))
              path-collector)
          (mapc ;; map over all languages
           (lambda (by-lang)
             (let* ((lang (car by-lang))
                    (specs (cdr by-lang))
                    (ext (or (cdr (assoc lang org-babel-tangle-lang-exts)) lang))
                    (lang-f (org-src-get-lang-mode lang))
                    she-banged)
               (mapc
                (lambda (spec)
                  (let ((get-spec (lambda (name) (cdr (assoc name (nth 4 spec))))))
                    (let* ((tangle (funcall get-spec :tangle))
                           (she-bang (let ((sheb (funcall get-spec :shebang)))
                                       (when (> (length sheb) 0) sheb)))
                           (tangle-mode (funcall get-spec :tangle-mode))
                           (base-name (cond
                                       ((string= "yes" tangle)
                                        (file-name-sans-extension
                                         (nth 1 spec)))
                                       ((string= "no" tangle) nil)
                                       ((> (length tangle) 0) tangle)))
                           (file-name (when base-name
                                        ;; decide if we want to add ext to base-name
                                        (if (and ext (string= "yes" tangle))
                                            (concat base-name "." ext) base-name))))
                      (when file-name
                        ;; Possibly create the parent directories for file.
                        (let ((m (funcall get-spec :mkdirp))
                              (fnd (file-name-directory file-name)))
                          (and m fnd (not (string= m "no"))
                               (make-directory fnd 'parents)))
                        ;; HACK Clear any old versions of file
                        (and (file-exists-p file-name)
                             (not (member file-name (mapcar #'car path-collector)))
                             (with-temp-file file-name t)) ;; NOTE This is what changed
                        ;; drop source-block to file
                        (with-temp-buffer
                          (when (fboundp lang-f) (ignore-errors (funcall lang-f)))
                          (when (and she-bang (not (member file-name she-banged)))
                            (insert (concat she-bang "\n"))
                            (setq she-banged (cons file-name she-banged)))
                          (org-babel-spec-to-string spec)
                          ;; We avoid append-to-file as it does not work with tramp.
                          (let ((content (buffer-string)))
                            (with-temp-buffer
                              (when (file-exists-p file-name)
                                (insert-file-contents file-name))
                              (goto-char (point-max))
                              ;; Handle :padlines unless first line in file
                              (unless (or (string= "no" (cdr (assq :padline (nth 4 spec))))
                                          (= (point) (point-min)))
                                (insert "\n"))
                              (insert content)
                              (write-region nil nil file-name))))
                        ;; if files contain she-bangs, then make the executable
                        (when she-bang
                          (unless tangle-mode (setq tangle-mode #o755)))
                        ;; update counter
                        (setq block-counter (+ 1 block-counter))
                        (unless (assoc file-name path-collector)
                          (push (cons file-name tangle-mode) path-collector))))))
                specs)))
           (if (equal arg '(4))
               (org-babel-tangle-single-block 1 t)
             (org-babel-tangle-collect-blocks lang-re tangle-file)))
          (message "Tangled %d code block%s from %s" block-counter
                   (if (= block-counter 1) "" "s")
                   (file-name-nondirectory
                    (buffer-file-name
                     (or (buffer-base-buffer) (current-buffer)))))
          ;; run `org-babel-post-tangle-hook' in all tangled files
          (when org-babel-post-tangle-hook
            (mapc
             (lambda (file)
               (org-babel-with-temp-filebuffer file
                 (run-hooks 'org-babel-post-tangle-hook)))
             (mapcar #'car path-collector)))
          ;; set permissions on tangled files
          (mapc (lambda (pair)
                  (when (cdr pair) (set-file-modes (car pair) (cdr pair))))
                path-collector)
          (mapcar #'car path-collector))))))

(setq uniquify-buffer-name-style 'forward)

(setq disabled-command-function nil)

;; Display ^L characters as horizontal lines
(use-package! page-break-lines
  :config (global-page-break-lines-mode))

;; Have C-l send the current line to the top of the window
(setq recenter-positions '(top bottom middle))

;; Perform a line feed after jumping to a ^L character
(defun my/recenter-top (&rest r) (recenter 0))
(advice-add #'forward-page :after #'my/recenter-top)

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

(setq all-the-icons-scale-factor 1.0)

(setq confirm-kill-processes nil)

(setq extended-command-suggest-shorter nil)

(setq default-input-method "latin-postfix")

(setq-default truncate-lines t)

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
