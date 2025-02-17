;;; ui/buffer-group/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! buffer-group
  :init
  (setq help-window-select t
        helpful-switch-buffer-function
        (lambda (buffer-or-name)
          (interactive)
          (pop-to-buffer buffer-or-name #'display-buffer-reuse-mode-window)))

  ;;; Modify default buffer groups

  ;; Diagnostic messages
  (when (modulep! :checkers syntax)
    (buffer-group-property-pushnew
     'diagnostics :names "^\\*Flycheck errors\\*")
    (buffer-group-property-pushnew
     'diagnostics :modes 'flycheck-error-list-mode))
  (when (modulep! :tools docker)
    (buffer-group-property-pushnew
     'diagnostics :names "^\\* docker container logs "))

  ;; Emacs internals
  (when (modulep! :emacs ibuffer)
    (buffer-group-property-pushnew
     'internals :modes 'ibuffer-mode)
    (buffer-group-property-pushnew
     'internals :names "^\\*Ibuffer\\*")
    (when (modulep! :ui workspaces)
      (buffer-group-property-pushnew
       'internals :names " buffers$")))


  ;;; Define additional buffer groups

  ;; Calendars
  (buffer-group-side-window-setup
   (buffer-group-define calendar
     `(:names ("^\\*Calendar\\*")
       :modes (calendar-mode))))

  ;; Calendars > Auxiliary
  (buffer-group-side-window-setup
   (buffer-group-define calendar-aux
     `(:names ("^\\*Holidays\\*")))
   '((slot . 1)))

  ;; Secrets
  (when (modulep! :tools bitwarden)
    (buffer-group-side-window-setup
     (buffer-group-define secrets
       `(:names ("^\\*bitwarden-list\\*")
         :modes (bitwarden-list-dialog-mode)))
     '((side . top))))

  (when (modulep! :tools docker)
    ;; Docker > Object listings
    (buffer-group-side-window-setup
     (buffer-group-define docker
       `(:names ("^\\*docker-\\(?:containers\\|images\\|networks\\|volumes\\)")
         :modes (docker-container-mode docker-image-mode docker-volume-mode docker-network-mode)))
     '((side . top)))

    ;; TODO Docker > Transient buffers
    ;;
    ;; Currently, Transient buffers open in the window below the selected window. IIRC,
    ;; Doom sets this option in the Magit configuration. I don't know if I like that.
    ;;
    ;; Unfortunately, transient buffer contents often too large to be comfortably
    ;; displayed in a side window; both vertical and horizontal space can be a problem.
    ;;
    ;; This is one scenario where a child frame centered in the middle of the selected
    ;; frame could be a good solution. (Side note: this could potentially be extended to
    ;; TUI Emacs through Tmux's popup system.)
    ;;
    ;; Take a look at the `transient-posframe' package. It seems a bit limited in that
    ;; it does not accept a display-buffer alist (which is needed for suppressing the
    ;; tab bar) and it does not automatically resize the frame. Resizing may be possible
    ;; by calling `fit-frame-to-buffer' at the appropriate time, perhaps like how the
    ;; `window-height' buffer display action alist parameter can be set to the
    ;; `fit-window-to-buffer' function.
    ;;
    ;; See also:
    ;; - `posframe--fit-frame-to-buffer'
    ;; - `company-box-frame-parameters'
    (buffer-group-define docker-transient '(:names ())))

  (when (modulep! :tools magit)
    ;; Magit > Interactive edits
    (buffer-group-side-window-setup
     (buffer-group-define magit-edit
       `(:names ("^\\(?:\\(?:COMMIT\\|TAG\\)_EDIT\\|MERGE_\\)MSG")))
     '((side . bottom) (slot . 1)))

    ;; Magit > Interactive selections
    (buffer-group-side-window-setup
     (buffer-group-define magit-select
       `(:modes (magit-log-select-mode)))
     '((side . bottom) (slot . 1))))

  (when (modulep! :lang org)
    ;; NOTE For more buffer names, search for `get-buffer-create' in the Org package.

    ;; Org > Interactive prompts
    (buffer-group-side-window-setup
     (buffer-group-define org-prompt
       `(:names ("^\\*Org Note\\*"
                 "^ \\*Agenda Commands\\*")))
     '((slot . 1)))

    ;; Org > Agenda views
    (buffer-group-side-window-setup
     (buffer-group-define org-agenda
       `(:names ("^\\*Org Agenda\\*")))
     '((side . top))))

  ;; Command history
  (buffer-group-side-window-setup
   (buffer-group-define command-history
     `(:names ("^ \\*Input History\\*"
               "^\\*Command Line\\*")
       :modes (completion-list-mode
               evil-command-window-mode)))
   '((slot . 1)))

  ;; Command output
  (buffer-group-side-window-setup
   (buffer-group-define output
     `(:names ("^\\*\\(?:Shell Command\\|Pp Eval\\) Output\\*"))))
  (when (modulep! :tools magit)
    (buffer-group-property-pushnew
     'output :names "^magit-process: ")
    (buffer-group-property-pushnew
     'output :modes 'magit-process-mode))

  ;; Popups > Scratch buffers
  (buffer-group-side-window-setup
   (buffer-group-define popup-scratch
     `(:names ("^\\*doom:scratch\\*"))))

  ;; Popups > Terminals
  (buffer-group-side-window-setup
   (buffer-group-define popup-term
     `(:names ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup"))))

  ;; Help text
  (buffer-group-side-window-setup
   (buffer-group-define help
     `(:names ("^\\*\\(?:[Hh]elp*\\|Apropos\\)"
               "^\\*lsp-help"
               "^\\*Shortdoc ")
       :modes (helpful-mode help-mode apropos-mode
               lsp-help-mode
               shortdoc-mode)))
   '((slot . 1)))

  ;; Reference docs
  (buffer-group-reuse-window-setup
   (buffer-group-define reference
     `(:names ("^\\*info\\*"
               "^\\*\\(?:Wo\\)?Man "
               "^\\*Kubernetes Docs "
               "^\\*ansible-doc ")
       :modes (Info-mode
               Man-mode woman-mode
               kubedoc-mode
               ansible-doc-mode ansible-doc-module-mode))))

  ;; PDFs
  (when (modulep! :tools pdf)
    (buffer-group-reuse-window-setup
     (buffer-group-define pdf
       `(:names ("*\\.pdf$")
         :modes (pdf-view-mode)))))

  ;; REPLs
  (buffer-group-side-window-setup
   (buffer-group-define repl
     `(:names ("^\\*ielm\\*")
       :modes (inferior-emacs-lisp-mode))))

  ;; Diffs
  (buffer-group-define diff '(:names ()))
  (when (modulep! :ui vc-gutter)
    (buffer-group-property-pushnew
     'diff :names "^\\*diff-hl\\*$"))
  (when (modulep! :emacs undo +tree)
    (after! undo-tree
      (buffer-group-property-pushnew
       'diff :names (concat "^" (regexp-quote undo-tree-diff-buffer-name)))))
  (buffer-group-side-window-setup 'diff)

  ;; Tree views > Undo tree
  (when (modulep! :emacs undo +tree)
    (after! undo-tree
      (buffer-group-side-window-setup
       (buffer-group-define undo-tree
         `(:names (,(concat "^" (regexp-quote undo-tree-visualizer-buffer-name)))))
       '((side . left) (slot . 1)))))

  ;; Tree views > LSP Symbols
  (buffer-group-side-window-setup
   (buffer-group-define imenu
     `(:names ("^\\*lsp-ui-imenu\\*"
               "^\\*LSP Symbols List\\*") ;; REVIEW This uses Treemacs
       :modes (lsp-ui-imenu-mode)))
   '((slot . 1)))

  ;; Tree views > Imenu list
  (when (modulep! :ui ilist)
    (after! imenu-list
      (buffer-group-side-window-setup
       (buffer-group-define imenu-list
         `(:names (,(concat "^" (regexp-quote imenu-list-buffer-name) "$"))
           :modes (imenu-list-major-mode)))
       `((side . ,imenu-list-position)
         (window-width . ,imenu-list-size)))))

  ;; Word definitions
  (buffer-group-side-window-setup
   (buffer-group-define dictionary
     `(:names ("*Dictionary*")
       :modes (dictionary-mode))))
  (when (and (modulep! :tools lookup +dictionary)
             (modulep! :tools lookup +offline))
    (after! wordnut
      (buffer-group-property-pushnew
       'dictionary :names (concat "^" (regexp-quote wordnut-bufname) "$"))
      (buffer-group-property-pushnew
       'dictionary :modes 'wordnut-mode))))


;;
;;; Hacks

(load! "+hacks")
