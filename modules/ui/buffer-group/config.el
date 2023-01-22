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

  (buffer-group-side-window-setup
   (buffer-group-define calendar
     `(:names ("^\\*Calendar\\*")
       :modes (calendar-mode))))

  (buffer-group-side-window-setup
   (buffer-group-define calendar-aux
     `(:names ("^\\*Holidays\\*")))
   '((slot . 1)))

  (when (modulep! :tools docker)
    ;; TODO Configure Transient windows
    ;; Currently, Transient buffers open in the window below the selected window. IIRC,
    ;; Doom sets this option in the Magit configuration. I don't know if I like that.
    (buffer-group-side-window-setup
     (buffer-group-define docker
       `(:names ("^\\*docker-\\(?:containers\\|images\\|networks\\|volumes\\)")
         :modes (docker-container-mode docker-image-mode docker-volume-mode docker-network-mode)))
     '((side . top)))
    (buffer-group-property-pushnew
     'diagnostics :names "^\\* docker container logs "))

  (buffer-group-side-window-setup
   (buffer-group-define help
     `(:names ("^\\*\\(?:[Hh]elp*\\|Apropos\\)"
               "^\\*lsp-help"
               "^\\*Shortdoc ")
       :modes (helpful-mode help-mode apropos-mode
               lsp-help-mode
               shortdoc-mode)))
   '((slot . 1)))

  (when (modulep! :ui ilist)
    (after! imenu-list
      (buffer-group-side-window-setup
       (buffer-group-define imenu
         `(:names (,(concat "^" (regexp-quote imenu-list-buffer-name) "$"))
           :modes (imenu-list-major-mode)))
       `((side . ,imenu-list-position)
         (window-width . ,imenu-list-size)))))

  (when (modulep! :checkers syntax)
    (buffer-group-property-pushnew
     'diagnostics :names "^\\*Flycheck errors\\*")
    (buffer-group-property-pushnew
     'diagnostics :modes 'flycheck-error-list-mode))

  (when (modulep! :tools magit)
    (buffer-group-side-window-setup
     (buffer-group-define magit-edit
       `(:names ("^COMMIT_EDITMSG")))
     '((side . bottom) (slot . 1)))
    (buffer-group-side-window-setup
     (buffer-group-define magit-select
       `(:modes (magit-log-select-mode)))))

  (when (modulep! :lang org)
    ;; For more buffer names, search for `get-buffer-create' in the Org package.
    (buffer-group-side-window-setup
     (buffer-group-define org-prompt
       `(:names ("^\\*Org Note\\*")))
     '((slot . 1))))

  (buffer-group-side-window-setup
   (buffer-group-define command-history
     `(:names ("^ \\*Input History\\*"
               "^\\*Command Line\\*")
       :modes (completion-list-mode
               evil-command-window-mode)))
   '((slot . 1)))

  (buffer-group-side-window-setup
   (buffer-group-define output
     `(:names ("^\\*\\(?:Shell Command\\|Pp Eval\\) Output\\*"))))
  (when (modulep! :tools magit)
    (buffer-group-property-pushnew
     'output :names "^magit-process: ")
    (buffer-group-property-pushnew
     'output :modes 'magit-process-mode))

  (when (modulep! :tools pdf)
    (buffer-group-reuse-window-setup
     (buffer-group-define pdf
       `(:names ("*\\.pdf$")
         :modes (pdf-view-mode)))))

  (buffer-group-side-window-setup
   (buffer-group-define popup-scratch
     `(:names ("^\\*doom:scratch\\*"))))

  (buffer-group-side-window-setup
   (buffer-group-define popup-term
     `(:names ("^\\*doom:\\(?:v?term\\|e?shell\\)-popup"))))

  (buffer-group-reuse-window-setup
   (buffer-group-define reference
     `(:names ("^\\*info\\*"
               "^\\*\\(?:Wo\\)?Man "
               "^\\*Kubernetes Docs ")
       :modes (Info-mode
               Man-mode woman-mode
               kubedoc-mode))))

  (buffer-group-side-window-setup
   (buffer-group-define repl
     `(:names ("^\\*ielm\\*")
       :modes (inferior-emacs-lisp-mode))))

  (when (modulep! :emacs undo +tree)
    (after! undo-tree
      (buffer-group-side-window-setup
       (buffer-group-define undo-tree
         `(:names (,(concat "^" (regexp-quote undo-tree-visualizer-buffer-name)))))
       '((side . left) (slot . 1)))
      (buffer-group-side-window-setup
       (buffer-group-define undo-tree-diff
         `(:names (,(concat "^" (regexp-quote undo-tree-diff-buffer-name))))))))

  (when (modulep! :ui vc-gutter)
    (buffer-group-side-window-setup
     (buffer-group-define diff-hl
       `(:names ("^\\*diff-hl\\*$"))))))


;;
;;; Hacks

(load! "+hacks")
