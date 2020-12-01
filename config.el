;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;;; completion/ivy

;; Make it easier to jump to headlines across Org buffers
(setq counsel-org-goto-all-outline-path-prefix 'buffer-name)


;;; ui/doom

;; Set the font and theme
;; PREREQ brew install font-terminus
(setq doom-font (font-spec :family "Terminus (TTF)" :size 16)
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

;;; ui/ligatures

;; Only enable extra ligatures in Org mode (for now)
(setq +ligatures-extras-in-modes '(org-mode))

;;; ui/popup

;; (Wo)Man buffers and windows should stick around until I close them
(set-popup-rule!
  "^\\*\\(?:Wo\\)?Man "
  :vslot -6 :size 0.45 :select t :quit 'current :ttl nil)

;; TODO Modify popup behavior for Customize buffers


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
   ("\\(\\\\\\)[().-]" 1 '(face nil display ""))    ; "\?"              -> "?"
   ("&copy;" 0 '(face nil display "©"))))          ; "&copy;"          -> "©"

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

(after! org
  ;; Do not indent org headlines
  (setq org-hide-leading-stars nil
        org-startup-indented nil
        org-adapt-indentation nil)

  ;; Show edit buffer in the current window
  (setq org-src-window-setup 'current-window)

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


;;; config/default

;; Use absolute line numbers
(setq display-line-numbers-type t)

;; Do not display line numbers in text mode
(remove-hook 'text-mode-hook #'display-line-numbers-mode)

;; Adjust `which-key' timing
(setq which-key-idle-delay 0.5
      which-key-idle-secondary-delay 0.1)

;; REVIEW See if there is a better way to do this (e.g. with `map!')
;; Add a toggle keybinding for `toggle-truncate-lines'
(define-key! doom-leader-toggle-map "t" #'toggle-truncate-lines)
(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`%s t t\\'" prefix-re))
                  nil . "Truncate lines")
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

;; Give buffers unique names based on file path
(setq uniquify-buffer-name-style 'forward)

;; Enable all disabled commands
(setq disabled-command-function nil)

;; Load custom config if present
(load! "custom" doom-emacs-dir t)

;; REVIEW Steal config stuff from <https://github.com/tecosaur/emacs-config/blob/master/config.org>
