(add-load-path! (expand-file-name "lisp" doom-user-dir))

(require 'eeowaa-lib)
(require 'xdg)

(require 'cli-help)
(require 'eeowaa-refresh)

;; NOTE: These are providing nothing of value right now
;; (require 'eeowaa-debug)
;; (require 'eeowaa-project)

;; Load `transient-childframe' and configure `transient'
;; FIXME: "C-g" from nested transients leaves a nested childframe
;; FIXME: Evil insert state is activated by "i", "s", etc.
;; (when initial-window-system
;;   (require 'transient-childframe)
;;
;;   ;; HACK: Revert Doom's configuration
;;   (when (modulep! :tools magit)
;;     (after! magit
;;       (setq transient-display-buffer-action
;;             (tc-transient-childframe--display-buffer-action)))))

;; NOTE: Since introducing `gptel' into my workflow, I can no longer use "SPC"
;; to pop up transient childframes; I have to use the default "C-t" binding.
;; Every workaround I have tried (including the following) has failed to prevent
;; broken `transient-childframe' windows.
;;
;; (define-key transient-map (kbd "SPC") #'transient-show)
;; (after! gptel-transient
;;   (transient-suffix-put 'gptel-menu "SPC" :key "C-SPC")
;;   (defadvice! my/gptel-no-crowdsourced-prompts-a (args)
;;     :filter-args #'gptel--setup-directive-menu
;;     (cons (car args) (list (car (cdr args))))))

(setq default-input-method "latin-postfix")

(unless initial-window-system
  (remove-hook 'company-mode 'company-box-mode))

(after! company-box
  (cl-pushnew 'company-echo-metadata-frontend company-frontends)
  (add-hook! company-box-mode
    (defun my/company-box-toggles-h ()
      (if company-box-mode
          (cl-callf2 remove 'company-echo-metadata-frontend company-frontends)
        (cl-pushnew 'company-echo-metadata-frontend company-frontends)))))

(after! company-box
  (add-to-list 'company-box-frame-parameters '(tab-bar-lines-keep-state . t)))

(setq company-idle-delay nil)

(setq company-box-doc-delay 0.3) ;; This feels a bit smoother than no delay

(defadvice! my/move-company-box-doc-frame-a (company-box-frame)
  :after #'company-box--update-frame-position
  (when-let ((parent-frame (frame-parent company-box-frame))
             (doc-frame (frame-local-getq company-box-doc-frame parent-frame))
             (_ (frame-visible-p doc-frame)))
    (company-box-doc--set-frame-position doc-frame)))

(when (eq eeowaa-wsl-version 1)
  (defadvice! my/company-box-doc--fix-border-display-a (fn &rest args)
    :after #'company-box-doc--set-frame-position
    (sleep-for 0.01)))

(when (modulep! company +childframe)
  (after! company
    (add-hook! 'evil-normal-state-entry-hook
      (defun +company-abort-h ()
        (when company-candidates
          (company-abort))))))

(defadvice! my/consult-org-heading-hide-preview-a (fn &rest args)
  :around #'consult-org-heading
  (let (consult-preview-key)
    (apply fn args)))

(defun my/dedicate-window-temporarily ()
  "Dedicate the selected window until `display-buffer' is called again."
  (interactive)
  (if-let ((window (selected-window))
           (dedicated (window-dedicated-p window)))
      (message "Window is already dedicated with flag `%s'" dedicated)
    (set-window-dedicated-p window 'until-next-display-buffer)
    (eval (macroexpand-1
           `(add-transient-hook! #'display-buffer :after
              (when (eq (window-dedicated-p ,window) 'until-next-display-buffer)
                (set-window-dedicated-p ,window nil)))))
    (message "Window is dedicated until the next `display-buffer'")))

(defun my/toggle-window-dedicated ()
  "Toggle the current window's `window-dedicated-p' flag.
If the flag is already set, it is set to `nil', otherwise it is
set to `t'. Use with caution in side windows and other windows
with special dedication semantics."
  (interactive)
  (if (set-window-dedicated-p nil (not (window-dedicated-p)))
      (prog1 t (message "Window is strongly dedicated"))
    (prog1 nil (message "Window is no longer dedicated"))))

(defvar my/toggle-window-splitting--restore-function #'split-window-sensibly)
(defun my/toggle-window-splitting ()
  "Toggle the ability to split most windows."
  (interactive)
  (if (eq split-window-preferred-function #'ignore)
      (prog1 t
        (setq split-window-preferred-function my/toggle-window-splitting--restore-function)
        (message "Window splitting has been globally restored"))
    (prog1 nil
      (setq my/toggle-window-splitting--restore-function split-window-preferred-function
            split-window-preferred-function #'ignore)
      (message "Window splitting has been globally disabled"))))

(define-key! evil-window-map
  ;; replaces `+workspace/close-window-or-workspace'
  "d" #'my/dedicate-window-temporarily
  "D" #'my/toggle-window-dedicated)

(setq all-the-icons-scale-factor 1.0)

(setq extended-command-suggest-shorter nil)

;; Mostly stolen from tecosaur's config
(setq frame-title-format
      '("%F"
        (:eval (format " on %s" (terminal-name)))
        (:eval (format " (up %s)" (emacs-uptime "%H, %M")))))

;; I find maximization to be unreliable on WSL 1, at least using VcXsrv.
(unless (eq eeowaa-wsl-version 1)
  (setf (alist-get 'fullscreen initial-frame-alist) 'maximized))

(after! col-highlight
  (require 'hl-line)
  (eeowaa-use-face col-highlight hl-line))

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(after! whitespace
  (require 'flycheck)
  (eval (macroexpand '(eeowaa-use-face whitespace-trailing flycheck-error))))

(defvar my/show-trailing-whitespace t)
(defvar my/trailing-whitespace-mode-alist
  '((rfc-mode . nil)
    (x509-asn1-mode . nil)

    ;; Trailing tabs are fine
    (tsv-mode . "\\( +\\)$")

    ;; Two trailing spaces are fine, but no other kind of trailing whitespace
    (markdown-mode . "\\S-\\( \\| \\{3,\\}\\|\\s-*\t\\s-*\\)$")))

(defun my/show-trailing-whitespace-maybe-h ()
  ;; This allows us to toggle trailing whitespace (both on and off)
  (when whitespace-mode
    (let* ((element (cl-some (lambda (e)
                               (when (derived-mode-p (car-safe e)) e))
                             my/trailing-whitespace-mode-alist))
           (value (if (consp element)
                      (cdr element)
                    my/show-trailing-whitespace)))
      (when value
        (when (stringp value)
          (setq-local whitespace-trailing-regexp value))
        (cl-pushnew 'trailing whitespace-style)))))

(add-hook 'whitespace-mode-hook
          #'my/show-trailing-whitespace-maybe-h)

(setq text-quoting-style 'grave)

(setq display-raw-bytes-as-hex t)

(unless initial-window-system
  (setq-default wrap-prefix "↪ "))

(setq visible-cursor nil)

(setq-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  indicate-empty-lines t)

;; Automatically highlight differences in hunks, down to the symbol.
;;
;; FIXME Highlighting for an added line spills over to the first character of
;; the other buffer.
;;
(setq vdiff-auto-refine t
      vdiff-default-refinement-syntax-code "w_")

(after! vdiff
  ;; Do not highlight lines in `vdiff-mode'.
  (add-hook! 'vdiff-mode-hook :append (hl-line-mode -1))

  ;; Use `C-l' to immediately update the diff (otherwise, just wait
  ;; `vdiff--after-change-refresh-delay' seconds).
  (setq-hook! 'vdiff-mode-hook
    revert-buffer-function (lambda (&rest _) (vdiff-refresh))
    eeowaa-refresh-force t)

  ;; Bind `vdiff-mode-prefix-map' to a convenient key.
  ;;
  ;; FIXME Use one of Doom's idiomatic key binding mechanisms for better
  ;; integration with leader keys (e.g. M-SPC in insert state) and which-key.
  ;;
  (evil-define-key 'normal vdiff-mode-map (kbd "SPC v") vdiff-mode-prefix-map)
  (evil-define-key 'normal vdiff-3way-mode-map (kbd "SPC v") vdiff-mode-prefix-map))

;; TODO Bind `vdiff-magit' command to a transient key in magit. For the time
;; being, just use `M-x vdiff-magit RET' to open the transient.
(after! magit
  (require 'vdiff-magit))

;; TODO Integrate vdiff folding with `:editor fold'.

;; TODO Integrate vdiff window configuration with `:ui workspaces' (reference
;; how Doom configures `ediff' for this).

;; TODO Configure `vdiff-3way-mode', `vdiff-magit-resolve', and
;; `vdiff-magit-compare' to work similarly to my git-vimdiff shell script.

(setq window-sides-vertical t)

(setq switch-to-buffer-obey-display-actions t)

(setq display-buffer-base-action '((display-buffer-same-window) . nil))

(setq window-resize-pixelwise t)

(map! "C-`"   #'window-toggle-side-windows)
   ;; "C-~"   #'+popup/raise
   ;; "C-x p" #'+popup/other

;; `always' is just a no-op that returns `t'
(defadvice! my/never-hide-modeline-a (&rest _)
  "Never hide the modeline"
  :around '(hide-mode-line-mode
            mode-line-invisible-mode)
  #'always)

(after! doom-themes-ext-treemacs
  (defadvice! my/show-treemacs-modeline-a (&rest _)
    "Show the treemacs modeline"
    :around 'doom-themes-hide-modeline
    #'always))

(remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)

; FIXME Warning: setting attribute `:foreground' of face `tab-bar': nil value is invalid, use `unspecified' instead.
(undefadvice! my/tab-bar-theme-a (theme &rest _)
  "Tweak the style of the tab bar."
  :after '(load-theme consult-theme)
  (if (string-match-p "\\`ef-" (symbol-name theme))
      (ef-themes-with-colors
        (custom-set-faces
         `(tab-bar ((,c :background ,bg-inactive :foreground ,fg-intense)))))
    (eeowaa-use-face tab-bar mode-line-inactive)))

(setq doom-theme
      (cond (initial-window-system 'ef-bio)
            (eeowaa-disable-icons 'modus-vivendi) ;; best theme on limited displays
            (t 'ef-tritanopia-dark)))

(after! (:and solaire-mode (:or vertico ivy))
  (let ((face (cond
               ((facep 'vertico-current) 'vertico-current)
               ((facep 'ivy-current-match) 'ivy-current-match)
               (t (error "Could not determine face")))))
    (custom-theme-set-faces! 'doom-outrun-electric
      `(solaire-hl-line-face :background
                             ,(face-attribute face :background)))))

;; Define fonts that I like
(setq my/fonts '(("Iosevka Comfy Fixed" ;; Remove " Fixed" if you want ligatures
                  :variable-pitch "Iosevka Comfy Duo"
                  :serif "Iosevka Comfy Motion Fixed"
                  ;; :default-size 15)
                  :default-size 14)
                 ("Source Code Pro"
                  :default-size 15)
                 ("Terminus"
                  :default-size 30)
                 ("Comic Mono"
                  :variable-pitch "Comic Neue"
                  :default-size 16)))

;; Define a fuction to change the fonts
(defun my/select-font (font &optional size)
  "Change the current fonts to a collection in `my/fonts'.
If SIZE is omitted, the default will be used.
When called interactively, reload the fonts in the current session."
  (interactive (list (completing-read "Font family: "
                                      (mapcar #'car my/fonts) nil t)
                     nil))
  (let* ((f (lambda (x y) (and x y (font-spec :family x :size y))))
         (p (or (alist-get font my/fonts nil nil #'string=)
                (error "\"%s\" not found in `my/fonts'" font)))
         (variable-pitch-font (plist-get p :variable-pitch))
         (serif-font (plist-get p :serif))
         (default-size (plist-get p :default-size)))
    (unless size
      (setq size (if (interactive-p)
                     (eeowaa-read-positive-int "Font size: " default-size)
                   default-size)))
    (setq doom-font                (funcall f font size)
          doom-variable-pitch-font (funcall f variable-pitch-font size)
          doom-serif-font          (funcall f serif-font size)))
  (and (interactive-p) (doom/reload-font)))

;; Set the font
;; (my/select-font "Iosevka Comfy Fixed")
;; (my/select-font "Comic Mono")

;; (add-hook 'doom-load-theme-hook #'doom-themes-treemacs-config)

(setq emojify-download-emojis-p t)

(after! hl-todo
  (setq hl-todo-keyword-faces
        (append '(("TESTME" font-lock-constant-face bold)
                  ("PREREQ" font-lock-doc-face bold)
                  ("DEBUG" font-lock-preprocessor-face bold))
                hl-todo-keyword-faces)))

(defmacro my/defhydra (name body &optional docstring &rest heads)
  "Define a hydra, expanding `my/hydra-key' forms in HEADS.
See the help text for `defhydra' for a description of arguments."
  (declare (indent defun) (doc-string 3))
  (let ((heads (mapcar (lambda (head)
                         (if (eq (car head) 'my/hydra-key)
                             (macroexpand head)
                           head))
                       heads)))
    `(defhydra ,name ,body ,docstring ,@heads)))

(defmacro my/hydra-key (key cmd hint &optional predicate)
  "Add a key to a hydra defined by `my/defhydra'.
Press KEY to eval CMD described by HINT.

If PREDICATE is `nil', do not propertize the text.

If PREDICATE is `t', then propertize the text according to the
variable whose symbol name equals that of CMD. (This is often
useful when CMD is a function that toggles a minor mode.)

If PREDICATE is any other non-`nil' value, then propertize the
text according to that value at runtime.

In order to propertize text, the DOCSTRING argument in the
`my/defhydra' should contain a \"_KEY_: ?LABEL?\" string
corresponding those arguments of this macro. For example:

  (my/defhydra hydra-minor-modes (:hint nil)
    \"_v_: ?v?, _r_: ?r?\"
    (my/hydra-key \"v\" view-mode \"View\" t)
    (my/hydra-key \"r\"
                  (read-only-mode 'toggle)
                  \"Read-Only\"
                  buffer-read-only)"
  (if (null predicate)
      `(,key ,cmd ,hint)
    `(,key ,cmd (propertize ,hint
                            'face
                            (if ,(if (eq t predicate)
                                     cmd
                                   predicate)
                                'bold
                              'italic)))))

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

(when (locate-library "counsel-spotify")
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
            ((featurep :system 'macos) (call-process "open" nil nil nil "-a" "spotify"))
            ((featurep :system 'linux) (call-process "xdg-open" nil nil nil "spotify"))
            (t (user-error! "Unsupported operating system"))))
    ("w" (browse-url "https://open.spotify.com"))
    ("i" (browse-url "https://developer.spotify.com/my-applications"))

    ;; Other
    ("q" nil)))

(global-set-key (kbd "C-c c") 'hydra-timeclock/body)
(defhydra hydra-timeclock (:color blue)
  "Timeclock"
  ("i" timeclock-in "In")
  ("o" timeclock-out "Out")
  ("c" timeclock-change "Change")
  ("e" timeclock-visit-timelog "Edit")
  ("g" timeclock-reread-log "Reload")
  ("s" timeclock-status-string "Status")
  ("r" my/timeclock-report "Report")
  ("q" nil "Quit"))

(global-set-key (kbd "C-c d") 'hydra-debug/body)
(defhydra hydra-debug ()
  "Debug"
  ("d" hydra-debug-debugger/body "Debugger" :exit t)
  ("b" hydra-debug-breakpoints/body "Breakpoints" :exit t)
  ("w" hydra-debug-watchpoints/body "Watchpoints" :exit t)
  ("t" hydra-debug-traps/body "Traps" :exit t)
  ("SPC" ignore nil :color red))

(defhydra hydra-debug-debugger (:color pink)
  ;; Stepping
  ("d" debugger-step-through "step in" :exit t :column "run")
  ("J" debugger-jump "step out" :exit t)
  ("c" debugger-continue "continue" :exit t)

  ;; Breakpoints
  ("gb" debugger-frame "set" :column "break")
  ("u" debugger-frame-clear "unset")
  ("gl" debugger-list-functions "list")

  ;; Evaluation
  ("E" debugger-eval-expression "print" :column "sexp")
  ("R" debugger-record-expression "record")
  ("RET" backtrace-help-follow-symbol "follow")

  ;; Visibility
  ("zo" backtrace-multi-line "open" :column "fold")
  ("zc" backtrace-single-line "close")
  ("." backtrace-expand-ellipses "expand")

  ;; Toggles
  ("p" backtrace-toggle-locals "locals" :column "toggle")
  (":" backtrace-toggle-print-gensym "uninterned")
  ("#" backtrace-toggle-print-circle "circular")

  ;; Exiting
  ("q" debugger-quit "toplevel nonstop" :column "exit")
  ("r" debugger-return-value "return with value")
  ("SPC" hydra-debug/body "Menu" :exit t)
  ("C-g" ignore nil :exit t))

; Unassigned:
;   backtrace-forward-frame
;   backtrace-backward-frame

(defhydra hydra-debug-breakpoints ()
  "Breakpoints"
  ("b" debug-on-entry "Set")
  ("u" cancel-debug-on-entry "Unset")
  ("l" (message "%s" (debug--function-list)) "List")
  ("SPC" hydra-debug/body "Menu" :exit t))

(defhydra hydra-debug-watchpoints ()
  "Watchpoints"
  ("s" debug-on-variable-change "Set")
  ("u" cancel-debug-on-variable-change "Unset")
  ("l" (message "%s" (debug--variable-list)) "List")
  ("SPC" hydra-debug/body "Menu" :exit t))

(my/defhydra hydra-debug-traps ()
  "Traps"
  (my/hydra-key
   "e" toggle-debug-on-error "Error" debug-on-error)
  (my/hydra-key
   "q" toggle-debug-on-quit "Quit (C-g)" debug-on-quit)
  (my/hydra-key
   "u" (lambda (event)
         (interactive `(,(intern (completing-read "Signal: " '(sigusr1 sigusr2 nil)))))
         (setq debug-on-event event))
   "User event" debug-on-event)
  (my/hydra-key
   "s" (lambda ()
         (interactive)
         (setq debug-on-signal (not debug-on-signal))
         (message "Debug on Signal %s globally"
                  (if debug-on-signal "enabled" "disabled")))
   "Signal" debug-on-signal)
  (my/hydra-key
   "m" (lambda (regexp)
         (interactive `(,(read-regexp "Message regexp: ")))
         (setq debug-on-message regexp))
   "Message" (not (or (null debug-on-message) (string-empty-p debug-on-message))))
  ("SPC" hydra-debug/body "Menu" :exit t))

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
              (forward-char) (forward-line)))
  ("2" (progn (table-goto-top-right-corner)
              (backward-char) (forward-line)))
  ("3" (progn (table-goto-bottom-left-corner)
              (forward-char) (forward-line -1)))
  ("4" (progn (table-goto-bottom-right-corner)
              (backward-char) (forward-line -1)))
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

(setq imenu-list-mode-line-format " Ilist"
      imenu-list-position 'right
      imenu-list-size 35) ;; same as treemacs

(after! imenu-list
  (setq-hook! 'imenu-list-major-mode-hook
    revert-buffer-function #'imenu-list-refresh))

(when initial-window-system
  (setq my/imenu-list-text-scale -1)
  (after! imenu-list
    (defun my/imenu-list-text-scale-h ()
      (text-scale-increase my/imenu-list-text-scale))
    (add-hook 'imenu-list-major-mode-hook #'my/imenu-list-text-scale-h)))

(add-hook! '+indent-guides-inhibit-functions #'always)

(setq +ligatures-in-modes '(org-mode)
      +ligatures-extras-in-modes '(org-mode))

;; Set the width to be the same as Ilist
(setq demap-minimap-window-width 35)

;; FIXME: Use a basic modeline
(defvar my/demap-mode-line-format " Minimap")
(defun my/demap-set-mode-line-h ()
  "Locally change `mode-line-format' to `my/demap-mode-line-format'."
  (setq-local mode-line-format my/demap-mode-line-format))
(add-hook 'demap-minimap-construct-hook #'my/demap-set-mode-line-h)

;; HACK: I redefine `doom-modeline-format--main' from its original value (see below)
;; because `doom-modeline' is not responding to changes to `doom-modeline-buffer-name'
;; as it should. Other segments have been removed and rearranged, as well.
;;
;;   (doom-modeline-def-modeline 'main
;;     '(eldoc bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
;;     '(compilation objed-state misc-info project-name persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs check time))
;;

;; Do not display buffer size in `doom-modeline-segment--matches'.
(setq size-indication-mode nil)

;; Always display the project in `doom-modeline-segment--project-name' (part 1 of 2).
(setq doom-modeline-project-name t)

(after! doom-modeline

  ;; Always display the project in `doom-modeline-segment--project-name' (part 2 of 2).
  (add-to-list 'doom-modeline-always-visible-segments 'project-name)

  ;; Remove extra surrounding spaces
  (doom-modeline-def-segment my/modals
    "Displays modal editing states for `evil'."
    (when doom-modeline-modal
      (doom-modeline--evil)))

  ;; Move percent before line/column and remove position scroller
  (doom-modeline-def-segment my/buffer-position
    "The buffer position information."
    (let ((sep (doom-modeline-spc))
          (face (doom-modeline-face))
          (help-echo "Buffer percentage\n\
  mouse-1: Display Line and Column Mode Menu")
          (mouse-face 'doom-modeline-highlight)
          (local-map mode-line-column-line-number-mode-map))
      `(,sep

        ;; Percent position
        (doom-modeline-percent-position
         ((:propertize ("" doom-modeline-percent-position)
           face ,face
           help-echo ,help-echo
           mouse-face ,mouse-face
           local-map ,local-map)
          ,sep
          ))

        ;; Line and column
        (:propertize
         ((line-number-mode
           (column-number-mode
            (doom-modeline-column-zero-based
             doom-modeline-position-column-line-format
             ,(string-replace
               "%c" "%C" (car doom-modeline-position-column-line-format)))
            doom-modeline-position-line-format)
           (column-number-mode
            (doom-modeline-column-zero-based
             doom-modeline-position-column-format
             ,(string-replace
               "%c" "%C" (car doom-modeline-position-column-format)))))
          (doom-modeline-total-line-number
           ,(and doom-modeline-total-line-number
                 (format "/%d" (line-number-at-pos (point-max))))))
         face ,face
         help-echo ,help-echo
         mouse-face ,mouse-face
         local-map ,local-map)
        ((or line-number-mode column-number-mode) ,sep))))

  (doom-modeline-def-segment my/buffer-state
    "The buffer state information."
    (concat (doom-modeline-spc)
            (doom-modeline--buffer-state-icon)))

  (doom-modeline-def-modeline 'main
    '(bar window-number my/modals follow my/buffer-state remote-host my/buffer-position matches selection-info)
    '(compilation check misc-info repl lsp input-method buffer-encoding major-mode process project-name vcs)))

(if initial-window-system
    ;; Just display an icon for file-visiting buffers
    (setq doom-modeline-highlight-modified-buffer-name nil)

  ;; Highlight the names of file-visiting buffers
  (setq doom-modeline-highlight-modified-buffer-name t)
  (defadvice! my/doom-modeline-ignore-modification-a (fn &rest args)
    :around '(doom-modeline-segment--buffer-info
              doom-modeline-segment--buffer-info-simple)
    (letf! ((defun my/doom-modeline-buffer-modification-a (&rest _)
              buffer-file-name)
            (defadvice #'buffer-modified-p :after-while
                       #'my/doom-modeline-buffer-modification-a))
      (apply fn args))))

;; NOTE `font-dest' is ripped straight from `nerd-icons-install-fonts'
(require 'nerd-icons)
(let ((font-dest (cond
                  ((member system-type '(gnu gnu/linux gnu/kfreebsd))
                   (concat (or (getenv "XDG_DATA_HOME")
                               (concat (getenv "HOME") "/.local/share"))
                           "/fonts/"
                           nerd-icons-fonts-subdirectory))
                  ((eq system-type 'darwin)
                   (concat (getenv "HOME")
                           "/Library/Fonts/"
                           nerd-icons-fonts-subdirectory)))))
  (unless (cl-every (lambda (font-name)
                      (file-exists-p (concat font-dest font-name)))
                    nerd-icons-font-names)
    (nerd-icons-install-fonts t)))

(setq column-number-indicator-zero-based nil)

(when eeowaa-disable-icons
  (setopt doom-modeline-icon nil))

(after! evil-goggles

  (defcustom my/evil-goggles-enable-jump t
    "If non-nil, enable jump support."
    :type 'boolean :group 'evil-goggles)

  (defface my/evil-goggles-jump-face '((t (:inherit evil-goggles-default-face)))
    "Face for jump action"
    :group 'evil-goggles-faces)

  (defun my/evil-goggles--jump-advice (&rest _)
    "Advice for commands that move point across lines."
    (let ((beg (line-beginning-position))
          (end (1+ (line-end-position))))
      (evil-goggles--show-async-hint beg end)))

  ;; `my/evil-goggles--jump-advice' is useful in many different contexts:
  ;; not just as advice, and not just within `evil-goggles'. Defining an alias
  ;; helps to generalize the function.
  (defalias 'my/flash-line #'my/evil-goggles--jump-advice "Flash the current line.")

  ;; Adding entries for `better-jumper-jump-{forward,backward}' and/or
  ;; `evil-jump-{forward,backward}' to `evil-goggles--commands' does not work
  ;; as expected (the line does not flash after a jump). However, a hook works.
  (after! better-jumper
    (add-to-list 'better-jumper-post-jump-hook #'my/flash-line))

  (dolist (command '(evil-scroll-line-to-center))
    (cl-pushnew `(,command
                  :face my/evil-goggles-jump-face
                  :switch my/evil-goggles-enable-jump
                  :advice my/evil-goggles--jump-advice)
                evil-goggles--commands)))

(map! :nv "gt" #'+tabs:next-window-tab
      :nv "gT" #'+tabs:prev-window-tab
      (:after evil-collection-magit
       (:map magit-status-mode-map
        :nv "gt" #'+tabs:next-window-tab
        :nv "gT" #'+tabs:prev-window-tab)
       (:map magit-diff-mode-map
        :nv "gt" #'+tabs:next-window-tab
        :nv "gT" #'+tabs:prev-window-tab)
       (:map magit-log-mode-map
        :nv "gt" #'+tabs:next-window-tab
        :nv "gT" #'+tabs:prev-window-tab)
       (:map magit-revision-mode-map
        :nv "gt" #'+tabs:next-window-tab
        :nv "gT" #'+tabs:prev-window-tab)))

;; NOTE Bindings for `info-mode' are set by `evil-collection-info-setup', which
;; is not called directly in `evil-collection-info'. Apparently this sort of
;; configuration is special, as evidenced by the `info' entry in the
;; `evil-collection-config' custom option. Advice provides an easy workaround:
(defadvice! my/window-tab-info-bindings-a (&rest _)
  :after #'evil-collection-info-setup
  (evil-collection-define-key 'normal 'Info-mode-map
    "gt" #'+tabs:next-window-tab
    "gT" #'+tabs:prev-window-tab))

(define-key! evil-window-map
  "C" #'+tabs/close-window
  "gf" #'+tabs:new-ffap-window-tab
  "gF" #'+tabs:new-ffap-with-line-window-tab
  "gd" #'+tabs:new-lookup-definition-window-tab)

;; NOTE This overrides `+workspace/new'
(map! :n "C-t" #'+tabs:new-duplicate-window-tab)

(after! evil-ex
  (evil-ex-define-cmd "tabnew"         #'+tabs:new-blank-window-tab)
  (evil-ex-define-cmd "tabe[dit]"      #'+tabs:new-window-tab)
  (evil-ex-define-cmd "tabf[ind]"      #'+tabs:new-project-window-tab)
  (evil-ex-define-cmd "tabd[uplicate]" #'+tabs:new-duplicate-window-tab)
  (evil-ex-define-cmd "tabc[lose]"     #'+tabs:close-window-tab)
  (evil-ex-define-cmd "tabo[nly]"      #'+tabs:close-other-window-tabs)
  (evil-ex-define-cmd "tabn[ext]"      #'+tabs:next-window-tab)
  (evil-ex-define-cmd "tabN[ext]"      #'+tabs:prev-window-tab)
  (evil-ex-define-cmd "tabp[rev]"      #'+tabs:prev-window-tab)
  (evil-ex-define-cmd "tabr[ewind]"    #'+tabs:first-window-tab)
  (evil-ex-define-cmd "tabfir[st]"     #'+tabs:first-window-tab)
  (evil-ex-define-cmd "tabl[ast]"      #'+tabs:last-window-tab)
  (evil-ex-define-cmd "tabm[ove]"      #'+tabs:move-window-tab))

(setq tab-bar-separator ""
      tab-line-separator "")

(after! tab-bar
  (setq tab-bar-tab-face-function #'tab-bar-tab-face-default))

(defadvice! my/ergonomic-eww-bindings-a (&rest _)
  :after #'evil-collection-eww-setup
  (evil-collection-define-key 'normal 'eww-mode-map
    ;; open this up for `vimish-tab'
    (kbd "gt") nil))

(setq display-time-day-and-date t ;; show the DoW and date in addition to the time
      display-time-load-average-threshold 0) ;; always display the load average

(after! vimish-tab
  (cl-pushnew "^\\*Ediff Control Panel\\*" vimish-tab-exclude-names))

(after! vimish-tab
  (dolist (entry display-buffer-alist)
    (when-let ((condition (car entry))
               (fn-name (and (symbolp condition) (symbol-name condition)))
               (_ (string-match "\\`buffer-group-member-p--\\(.+\\)\\'" fn-name))
               (group (intern (match-string 1 fn-name)))
               (action (and (listp (cdr entry)) (cdr entry)))
               (alist (cdr action)))
      (when (memq 'side (mapcar #'car alist))
        (mapc (lambda (mode) (cl-pushnew mode vimish-tab-exclude-names))
              (buffer-group-plist-get group :names))
        (mapc (lambda (mode) (cl-pushnew mode vimish-tab-exclude-modes))
              (buffer-group-plist-get group :modes)))))

  ;; Treemacs buffers are treated specially
  (cl-pushnew 'treemacs-mode vimish-tab-exclude-modes)

  ;; HACK Buffer groups with deferred definitions
  (dolist (mode '(imenu-list-major-mode))
    (cl-pushnew mode vimish-tab-exclude-modes)))

;; Load custom Treemacs theme definitions
(after! treemacs
  (require 'eeowaa-treemacs-themes))

;; Simplify the modeline
(setq treemacs-user-mode-line-format " Treemacs")

;; Allow resizable fonts
(setq doom-themes-treemacs-enable-variable-pitch nil)

;; Set the text scale to -1 in graphical Emacs
(setq treemacs-text-scale (if initial-window-system -1 0))

;; Do not read from popup frames
(setq treemacs-read-string-input 'from-minibuffer)

;; Add keybindings to visit adjacent files
(after! treemacs-evil
  (defun my/treemacs-visit-next ()
    "Open the next node in another window."
    (interactive)
    (treemacs-next-line 1)
    (save-selected-window
      (treemacs-visit-node-no-split 1)))
  (defun my/treemacs-visit-previous ()
    "Open the previous node in another window."
    (interactive)
    (treemacs-previous-line 1)
    (save-selected-window
      (treemacs-visit-node-no-split 1)))
  (define-key! evil-treemacs-state-map
    "J" #'my/treemacs-visit-next
    "K" #'my/treemacs-visit-previous))

;; Allow selection of treemacs window via `ace-window'
(after! (:and treemacs ace-window)
  (setq aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers)))

;; Configure `revert-buffer' (modified from original config)
(after! treemacs
  (defun my/treemacs-revert-buffer-function (&rest _)
    (treemacs-refresh))
  (setq-hook! 'treemacs-mode-hook
    revert-buffer-function #'my/treemacs-revert-buffer-function))

(if eeowaa-disable-icons
    ;; Prevent treemacs from using icons when using Emacs over SSH
    (progn
      (setq treemacs-no-png-images t
            doom-themes-treemacs-theme "eeowaa-minimal" ;; REVIEW This is not taking effect
            lsp-treemacs-theme "eeowaa-minimal") ;; REVIEW This is not taking effect

      (after! treemacs-icons
        ;; Treemacs doesn't know when we are using SSH
        (defadvice! my/treemacs--no-images-a (&rest _)
          :override '(treemacs--is-image-creation-impossible?
                      treemacs--is-image-creation-impossible?--inliner)
          t)

        ;; Always use two spaces for file icons
        ;; FIXME: I always have to set `treemacs-icon-fallback' in the buffer
        ;;  (defadvice! my/treemacs--no-icons-a (&rest _)
        ;;    :override '(treemacs-icon-for-file
        ;;                treemacs-icon-for-file--inliner
        ;;                treemacs-icon-for-mode
        ;;                treemacs-icon-for-mode--inliner)
        ;;    treemacs-icon-fallback)
        (setq treemacs-icon-fallback "  ")
        (setq-default treemacs-icon-fallback "  ")
        (setq-hook! 'treemacs-mode-hook treemacs-icon-fallback "  ")
        (add-hook! 'treemacs-mode-hook :append
          (defun my/treemacs-load-minimal-theme ()
            (treemacs-load-theme "eeowaa-minimal")))))

  ;; Use a custom Treemacs theme
  (setq doom-themes-treemacs-theme "eeowaa-nerd-icons"
        lsp-treemacs-theme "eeowaa-nerd-icons")
  (add-hook! 'treemacs-mode-hook :append
    (defun my/treemacs-load-nerd-theme ()
      (treemacs-load-theme "eeowaa-nerd-icons"))))

;; Use proper indentation for the environment
(setq treemacs-indentation (if initial-window-system 3 2))

;; REVIEW Where is this supposed to happen?
;; (add-hook 'doom-load-theme-hook #'doom-themes-treemacs-config))

(defvar my/diff-hl-minor-modes '(not git-commit-mode smerge-mode)
  "List of minor modes to prevent `diff-hl-mode' from activating.
Similar to `diff-hl-global-modes', except for minor modes and
works even when `global-diff-hl-mode' is disabled.")

(defadvice! my/diff-hl-minor-modes-a (&optional arg)
  :before-while #'diff-hl-mode
  (cond
   ;; Do not prevent turning off `diff-hl-mode'
   ((or (and (eq arg 'toggle) diff-hl-mode)
        (and (numberp arg) (< arg 1))) t)

   ;; Decide whether we should turn it on based on active minor modes
   ((eq my/diff-hl-minor-modes t) t)
   ((eq (car-safe my/diff-hl-minor-modes) 'not)
    (not (seq-intersection local-minor-modes (cdr my/diff-hl-minor-modes))))
   (t (seq-intersection local-minor-modes my/diff-hl-minor-modes))))

(after! diff-hl
  (unless (window-system) (diff-hl-margin-mode)))

(when (and (modulep! :ui vc-gutter +pretty)
           (modulep! :ui vc-gutter +diff-hl)
           initial-window-system)

  (add-hook! 'diff-hl-mode-hook
    (defadvice! my/diff-hl-fringe-a (theme &rest _)
      :after '(load-theme consult-theme)
      (+vc-gutter-fix-diff-hl-faces-h)))

  (after! ef-themes
    (dolist (theme ef-themes-dark-themes)
      (eval (macroexpand-1
             `(custom-theme-set-faces! ',theme
                `(diff-hl-insert :foreground ,(ef-themes-get-color-value 'bg-added-refine nil ',theme))
                `(diff-hl-change :foreground ,(ef-themes-get-color-value 'bg-changed-refine nil ',theme))
                `(diff-hl-delete :foreground ,(ef-themes-get-color-value 'bg-removed-refine nil ',theme)))))))

  (with-eval-after-load 'modus-themes
    (custom-theme-set-faces! 'modus-vivendi
      `(diff-hl-insert :foreground ,(alist-get 'green-fringe-bg modus-themes-vivendi-colors))
      `(diff-hl-change :foreground ,(alist-get 'yellow-fringe-bg modus-themes-vivendi-colors))
      `(diff-hl-delete :foreground ,(alist-get 'red-fringe-bg modus-themes-vivendi-colors)))))

(after! ace-window
  (when initial-window-system
    (setq aw-scope 'visible)))

(setq aw-background nil)

(after! ace-window
  (defadvice! my/aw-face-height-a (theme &rest _)
    :after '(load-theme consult-theme)
    (set-face-attribute 'aw-leading-char-face nil
                        :height (face-attribute 'default :height))))

(map! :leader
      (:when (modulep! :ui workspaces)
       (:prefix-map ("TAB" . "workspace")
        :desc "Move left"  "H" #'+workspace/swap-left
        :desc "Move right" "L" #'+workspace/swap-right)))

(after! projectile
  (define-key! projectile-mode-map
    "C-c p" #'projectile-command-map))

(setq-default visual-fill-column-center-text t
              visual-fill-column-width 120)

(setq-default visual-fill-column-fringes-outside-margins nil)

(after! writeroom-mode

  (defvar my/zen--old-display-line-numbers nil)
  (defvar my/zen--old-hl-line-mode nil)
  (defvar my/zen--old-column-highlight-mode nil)
  (defvar my/zen--old-display-fill-column-indicator-mode nil)
  (defvar my/zen--old-tab-bar-mode nil)
  (defvar my/zen--old-tab-line-mode nil)
  (defvar my/zen--old-vi-tilde-fringe-mode nil)
  (defvar my/zen--old-diff-hl-mode nil)

  (add-hook! writeroom-mode :append
    (defun my/zen-toggle-h ()
      "Toggle distracting features."
      (if writeroom-mode
          (progn
            ;; Save settings
            (setq my/zen--old-display-line-numbers
                  (bound-and-true-p display-line-numbers)
                  my/zen--old-hl-line-mode
                  (bound-and-true-p hl-line-mode)
                  my/zen--old-column-highlight-mode
                  (bound-and-true-p column-highlight-mode)
                  my/zen--old-display-fill-column-indicator-mode
                  (bound-and-true-p display-fill-column-indicator-mode)
                  my/zen--old-tab-bar-mode
                  (bound-and-true-p tab-bar-mode)
                  my/zen--old-tab-line-mode
                  (bound-and-true-p tab-line-mode)
                  my/zen--old-vi-tilde-fringe-mode
                  (bound-and-true-p vi-tilde-fringe-mode)
                  my/zen--old-diff-hl-mode
                  (bound-and-true-p diff-hl-mode))

            ;; Remove distractions
            (display-line-numbers-mode -1)
            (hl-line-mode -1)
            (column-highlight-mode -1)
            (display-fill-column-indicator-mode -1)
            (tab-bar-mode -1)
            (tab-line-mode -1)
            (vi-tilde-fringe-mode -1)
            (diff-hl-mode -1))

        ;; Restore previous state
        (if my/zen--old-display-line-numbers
            (let ((display-line-numbers-type my/zen--old-display-line-numbers))
              (display-line-numbers-mode)))
        (if my/zen--old-hl-line-mode (hl-line-mode +1))
        (if my/zen--old-column-highlight-mode (column-highlight-mode +1))
        (if my/zen--old-display-fill-column-indicator-mode (display-fill-column-indicator-mode +1))
        (if my/zen--old-tab-bar-mode (tab-bar-mode +1))
        (if my/zen--old-tab-line-mode (tab-line-mode +1))
        (if my/zen--old-vi-tilde-fringe-mode (vi-tilde-fringe-mode +1))
        (if my/zen--old-diff-hl-mode (diff-hl-mode +1))))))

;; Display ^L characters as horizontal lines
(use-package! page-break-lines
  :config (global-page-break-lines-mode))

;; When using vanilla bindings, have C-l send the current line to the top of the
;; window (like most shells do)
(setq recenter-positions '(top bottom middle))

;; When using `evil', have C-l refresh the buffer and and redraw the display.
;; Use zt, zz, and zb to reposition the current line instead of C-l.
(when (modulep! :editor evil)
  (global-set-key (kbd "C-l") #'eeowaa-refresh-buffer-and-display))

;; Perform a line feed after jumping to a ^L character
(defadvice! my/recenter-top-a (&rest _)
  "Perform a line feed after jumping to a ^L character"
  :after #'forward-page
  (recenter 0))

;; Fix `forward-page' for `evil' mode so that C-x ] works at page boundaries.
(defadvice! my/forward-page-evil-fix-a (&optional count)
  "Position `point' correctly in `evil' normal state."
  :before #'forward-page
  (when (and (or (not count)
                 (and (numberp count)
                      (> count 0)))
             (bound-and-true-p evil-local-mode)
             (eq evil-state 'normal)
             (looking-at-p "\f"))
    (forward-char)))

(remove-hook 'text-mode-hook #'visual-line-mode)

(evil-define-command my/evil-quit-a (&optional force)
  "Mark the current buffer as \"Done\" when performing a server
edit; otherwise, just kill the current buffer. Prompt to save the
current buffer first unless the `force' argument is given."
  :repeat nil
  (interactive "<!>")
  (if (and (boundp 'server-buffer-clients)
           (fboundp 'server-edit)
           (fboundp 'server-buffer-done)
           server-buffer-clients)
      (if force
          (server-buffer-done (current-buffer))
        (server-edit))
    (when force
      (set-buffer-modified-p nil))
    (kill-current-buffer)))

;; Works for all variations of `:q'
(advice-add 'evil-quit :override #'my/evil-quit-a)

(evil-define-command my/evil-delete-buffer-a (buffer &optional bang)
  (interactive "<b><!>")
  (with-current-buffer (or buffer (current-buffer))
    (when bang
      (set-buffer-modified-p nil)
      (dolist (process (process-list))
        (when (eq (process-buffer process) (current-buffer))
          (set-process-query-on-exit-flag process nil))))
    ;; get all windows that show this buffer
    (let ((wins (get-buffer-window-list (current-buffer) nil t)))
      ;; if the buffer which was initiated by emacsclient,
      ;; call `server-edit' from server.el to avoid
      ;; "Buffer still has clients" message
      (if (and (fboundp 'server-edit)
               (boundp 'server-buffer-clients)
               server-buffer-clients)
          (server-edit)
        (kill-buffer nil))
      ;; close all windows that showed this buffer
      (mapc #'(lambda (w)
                (condition-case nil
                    (with-selected-window w
                      (vimish-tab-close-tab-or-window))
                  (error nil)))
            wins))))

(advice-add 'evil-delete-buffer :override #'my/evil-delete-buffer-a)

(evil-define-command my/evil-window-split-a (&optional count file)
  "Same as `+evil-window-split-a', but does not recenter the window."
  :repeat nil
  (interactive "P<f>")
  (let ((origwin (selected-window))
        window-selection-change-functions)
    (select-window (split-window origwin count 'below))
    (unless evil-split-window-below
      (select-window origwin)))
  (run-hook-with-args 'window-selection-change-functions nil)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

(evil-define-command my/evil-window-vsplit-a (&optional count file)
  "Same as `+evil-window-vsplit-a', but does not recenter the window."
  :repeat nil
  (interactive "P<f>")
  (let ((origwin (selected-window))
        window-selection-change-functions)
    (select-window (split-window origwin count 'right))
    (unless evil-vsplit-window-right
      (select-window origwin)))
  (run-hook-with-args 'window-selection-change-functions nil)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

(advice-add #'+evil-window-split-a :override #'my/evil-window-split-a)
(advice-add #'+evil-window-vsplit-a :override #'my/evil-window-vsplit-a)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq evil-v$-excludes-newline t)

(when (modulep! :editor evil +everywhere)
  (add-hook 'Info-mode-hook #'evil-collection-info-setup))

(defadvice! my/ergonomic-info-bindings-a (&rest _)
  :after #'evil-collection-info-setup
  (evil-collection-define-key 'normal 'Info-mode-map
    (kbd "C-t") nil ;; open this up for `vimish-tab'
    (kbd "C-i") 'Info-next-reference
    (kbd "M-,") 'Info-history-back
    (kbd "C-M-,") 'Info-history-forward))

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
  (defadvice! my/pong-emacs-evil-state-a (&rest _)
    "Use Emacs evil state in `pong'"
    :after #'pong-init
    (evil-emacs-state))
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

(map! :v "]b" #'base64-encode-region
      :v "[b" #'base64-decode-region)

(setq evil-auto-balance-windows nil)

(add-to-list 'evil-emacs-state-modes 'noaa-mode)

(setq hs-allow-nesting t)

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

(setq +word-wrap-extra-indent nil)

(remove-hook 'text-mode-hook #'+word-wrap-mode)

(setq disabled-command-function nil)

(when (featurep 'native-compile)
  (setq native-comp-speed 2
        package-native-compile t))

(let ((srcdir (format "%s.local/src/emacs/gnu-emacs/src"
                      (file-name-as-directory (getenv "HOME")))))
  (when (file-directory-p srcdir)
    (setq find-function-C-source-directory srcdir)))

(defadvice! my/doom--sudo-file-path-a (file)
  "Use `tramp-system-name' instead of \"localhost\" as the hostname"
  :override #'doom--sudo-file-path
  (let ((host (or (file-remote-p file 'host) tramp-system-name)))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

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

(after! server
  (setq server-temp-file-regexp
        (format "\\(?:%s\\)\\|\\(?:\\`%s\\(?:bash-fc\\.\\|zsh[a-zA-Z0-9]+\\.zsh\\'\\)\\)"
                server-temp-file-regexp
                (temporary-file-directory))))

(setq confirm-kill-processes nil)

(defalias 'ps 'list-processes)

(setq debugger-stack-frame-as-list t)

(setq Man-width-max nil
      woman-fill-frame t)

(defun my/make-buffer-file-executable ()
  "Make the current buffer's file executable."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (cond
     ((null file-name)
      (error "Current buffer is not visiting a file"))
     ((executable-make-buffer-file-executable-if-script-p)
      (message "Made file executable: %s" file-name)))))

(map! :leader
      (:prefix-map ("f" . "file")
       :desc "chmod +x this file" "X" #'my/make-buffer-file-executable))

(after! helpful
  (setq-hook! 'helpful-mode-hook
    revert-buffer-function (lambda (&rest _) (helpful-update))))

(after! man
  (setq-hook! 'Man-mode-hook
    revert-buffer-function (lambda (&rest _) (Man-update-manpage))))

(after! woman
  (setq-hook! 'woman-mode-hook
    revert-buffer-function (lambda (&rest _) (woman-reformat-last-file))))

(defun my/font-lock-update (arg)
  "Refontify some or all of the current buffer.

With positive numeric prefix, refontify that many lines on each
side of point. (Universal arguments are translated to numeric
form by exponentiating 4 by the number of `C-u' sequences.)

Without positive numeric prefix, refontify the active region if
there is one. Otherwise, refontify the entire accessible portion
of the current buffer."
  (interactive "P")
  (cl-destructuring-bind (type value) (eeowaa-parse-raw-prefix arg)
    (cond
     ((or (eq type 'universal) (and (eq type 'numeric) (cl-plusp value)))
      (font-lock-fontify-block value))
     ((region-active-p)
      (font-lock-fontify-region (region-beginning) (region-end)))
     (t
      (font-lock-update)))))

(global-set-key [remap font-lock-update] #'my/font-lock-update)

(defun my/current-buffer-information ()
  (interactive)
  (let* ((filepath (if buffer-file-name (abbreviate-file-name buffer-file-name) "nil"))
         (new (if (and buffer-file-name (not (file-exists-p buffer-file-name))) "[new]" ""))
         (modified (if (buffer-modified-p) "[modified]" ""))
         (readonly (if buffer-read-only "[readonly]" ""))
         (space (if (zerop (length (format "%s%s%s" new modified readonly))) "" " "))
         (lines (format "%d lines" (save-restriction
                                     (widen)
                                     (count-lines (point-min) (point-max))))))
    (message "%S %s%s%s%s%s %S" filepath new modified readonly space lines (buffer-name))))

(define-key! help-map "h" #'my/current-buffer-information)

(defun my/zoomwin-toggle ()
  "Zoom or unzoom the selected window.
If the current frame has multiple windows, delete other windows.
If the current frame has one window, restore the previous windows."
  (interactive)
  (if (= 1 (length
            (seq-remove (lambda (w) (window-parameter w 'no-other-window))
                        (window-list))))
      (when-let ((zoomwin-state (persp-parameter 'my/zoomwin-state)))
        (set-window-configuration zoomwin-state))
    (set-persp-parameter 'my/zoomwin-state (current-window-configuration))
    (delete-other-windows)))

(define-key! evil-window-map
  ;; replaces `doom/window-enlargen'
  "o" #'my/zoomwin-toggle)

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
                  (seq-filter
                   (lambda (dir)
                     (string-match-p dir (expand-file-name default-directory)))
                   (list (file-name-as-directory (xdg-user-dir "DOCUMENTS"))
                         doom-user-dir))
                t)))))))

;; NOTE `dired' is required by the `find-dired' library, so the following code
;; will always execute before the `find-dired' function is called.
(after! dired
  (setq find-dired-refine-function nil
        find-ls-option
        (cons (format "-print0 | sort -z | xargs -0 -e %s %s -d"
                      insert-directory-program
                      dired-listing-switches)
              (concat dired-listing-switches " -d"))))

(when eeowaa-disable-icons
  (when (modulep! :emacs dired +icons)
    (after! dirvish
      (setq dirvish-subtree-always-show-state nil)
      (cl-callf2 remove 'nerd-icons dirvish-attributes)
      (cl-callf2 remove 'subtree-state dirvish-attributes))))

(defun my/+dired-split-jump ()
  (interactive)
  (select-window (split-window-below))
  (dired-jump))

(defun my/+dired-vsplit-jump ()
  (interactive)
  (select-window (split-window-right))
  (dired-jump))

;; XXX This function is experimental
(defun my/+dired-tab-jump ()
  (interactive)
  (let ((vimish-tab-new-buffer-function
         (lambda ()
           (vimish-tab-file-buffer
            (if buffer-file-name
                (file-name-directory buffer-file-name)
              default-directory)))))
    (vimish-tab-new)))

(after! evil-ex
  (evil-ex-define-cmd "Ex[plore]"  #'dired-jump)
  (evil-ex-define-cmd "Sex[plore]" #'my/+dired-split-jump)
  (evil-ex-define-cmd "Vex[plore]" #'my/+dired-vsplit-jump)
  (evil-ex-define-cmd "Tex[plore]" #'my/+dired-tab-jump))

(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)

(when (and eeowaa-disable-icons
           (modulep! :emacs ibuffer +icons))
  (after! ibuffer
    (setcar ibuffer-formats
            (let ((fmt (car ibuffer-formats)))
              (append (seq-subseq fmt 0 5)
                      (seq-subseq fmt 7)))))
  (after! ibuffer-projectile
    (setq ibuffer-projectile-prefix "Project: ")))

(add-hook! ibuffer-mode
  (defun my/ibuffer-mode-line-h ()
    "Clean up the modeline and improve performance."
    (setcar mode-line-process "by ")
    (setf (nth 2 ibuffer-header-line-format) " by ")))

;; This should already be enabled by emacs/undo/config.el
(global-undo-tree-mode)

;; Not sure what the best way is to add this hook
(add-hook 'evil-local-mode-hook #'turn-on-undo-tree-mode)

(after! undo-tree
  (advice-remove 'undo-tree-visualizer-show-diff
                 '+undo-tree--show-visualizer-diff-safely-a))

(after! git-commit
  (cl-callf2 remove 'overlong-summary-line git-commit-style-convention-checks))

(dolist (mode '(("/git/config\\.d/.+" . gitconfig-mode)
                ("\\.gitignore\\'" . gitignore-mode)))
  (add-to-list 'auto-mode-alist mode))

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

  ;; w3m-find-file
  "w3m"  "w3m-find-file"

  ;; shell commands
  "git" "git --no-pager -c color.ui=always -c interactive.singleKey=false $*"
  "f"   "cd $1 && ls")

(after! eshell
  (setq eshell-scroll-show-maximum-output nil))

(after! eshell
  (advice-add 'eshell/clear :override #'recenter-top-bottom))

(add-hook! eshell-mode #'my/with-editor-export)

(after! vterm
  (defalias 'vterm-send-C-m #'vterm-send-return))

(after! evil-collection-vterm
  (dolist (key '("C-h" "C-u" "C-j" "<M-backspace>"))
    (evil-collection-define-key 'insert 'vterm-mode-map
      (kbd key) 'vterm--self-insert)))

(after! evil-collection-vterm
  (dolist (state '(normal insert))
    (evil-collection-define-key state 'vterm-mode-map
      (kbd "M-0") #'+workspace/switch-to-final
      (kbd "M-1") #'+workspace/switch-to-0
      (kbd "M-2") #'+workspace/switch-to-1
      (kbd "M-3") #'+workspace/switch-to-2
      (kbd "M-4") #'+workspace/switch-to-3
      (kbd "M-5") #'+workspace/switch-to-4
      (kbd "M-6") #'+workspace/switch-to-5
      (kbd "M-7") #'+workspace/switch-to-6
      (kbd "M-8") #'+workspace/switch-to-7
      (kbd "M-9") #'+workspace/switch-to-8
      (kbd "M-:") #'eval-expression)))

(when (modulep! :editor evil)
  (after! vterm
    (map! :map vterm-mode-map
          :n "0"  #'evil-beginning-of-line)))

(after! (:and vterm evil-collection-vterm)
  (evil-collection-define-key 'insert 'vterm-mode-map
    (kbd "C-s") 'evil-window-map))

;; NOTE Allow cursor to appear after last character in line
(after! vterm
  (setq-hook! 'vterm-mode-hook
    evil-move-beyond-eol t))

;; HACK Send <backspace> instead of <delete>
;; C-d works, too, but it can send EOF and close vterm
(after! vterm
  (defadvice! my/vterm-delete-region-a (start end)
    :override #'vterm-delete-region
    (when vterm--term
      (if (vterm-goto-char end)
          (cl-loop repeat (- end start) do
                   (vterm-send-key "<backspace>" nil nil nil t))
        (let ((inhibit-read-only nil))
          (vterm--delete-region start end))))))

;; HACK Prevent vterm from changing cursor type (e.g. when quitting out of top(1))
;; <https://github.com/akermu/emacs-libvterm/issues/313>
(after! vterm
  (defadvice! my/vterm--redraw-a (fn &rest args)
    "Prevent vterm from changing the cursor type."
    :around #'vterm--redraw
    (let ((cursor-type cursor-type))
      (apply fn args))))

(after! evil-collection-vterm

  ;; HACK If `vterm-goto-char' fails, reset cursor point
  (defadvice! my/evil-collection-vterm-insert-a ()
    :override #'evil-collection-vterm-insert
    (interactive)
    (let ((inhibit-redisplay t))
      (or (vterm-goto-char (point))
          (vterm-reset-cursor-point)))
    (evil-insert-state))

  ;; HACK Send <right> before entering insert state
  (defadvice! my/evil-collection-vterm-append-a ()
    :override #'evil-collection-vterm-append
    (interactive)
    (let ((inhibit-redisplay t))
      (or (vterm-goto-char (point))
          (vterm-reset-cursor-point))
      (or (looking-at-p " *$")
          (vterm-send-right)))
    (evil-insert-state))

  ;; HACK Send C-a instead of relying on vterm functions
  (defadvice! my/evil-collection-vterm-insert-line-a ()
    :override #'evil-collection-vterm-insert-line
    (interactive)
    (let ((inhibit-redisplay t))
      (vterm-reset-cursor-point)
      (vterm-send-C-a))
    (evil-insert-state))

  ;; HACK Send C-e instead of relying on vterm functions
  (defadvice! my/evil-collection-vterm-append-line-a ()
    :override #'evil-collection-vterm-append-line
    (interactive)
    (let ((inhibit-redisplay t))
      (vterm-reset-cursor-point)
      (vterm-send-C-e))
    (evil-insert-state)))

(defvar my/with-editor-emacsclient-executable--vterm
  (file-name-concat (getenv "HOME") ".local" "libexec" "emacs" "emacsclient-vterm"))

(add-hook! vterm-mode
  (defun my/with-editor-export--vterm ()
    (if (file-executable-p my/with-editor-emacsclient-executable--vterm)
        (let ((with-editor-emacsclient-executable
               my/with-editor-emacsclient-executable--vterm))
          (my/with-editor-export))
      (my/with-editor-export))))

(after! vterm

  (defun my/vterm--display-buffer-same-window (buffer)
    "Display a buffer in the current window.
This function works even if the current window is a side window."
    (interactive)
    (if-let* ((side (window-parameter nil 'window-side))
              (slot (window-parameter nil 'window-slot)))
        (display-buffer-in-side-window buffer `((side . ,side) (slot . ,slot)))
      (display-buffer-same-window buffer nil)))

  (defun my/vterm--dedicate-window-h ()
    (set-window-dedicated-p (selected-window)
                            (window-parameter nil 'my/vterm--window-dedicated)))

  (add-hook 'server-switch-hook #'my/vterm--dedicate-window-h)

  (defun my/vterm-edit-indirect ()
    "Edit a command line by sending `C-x C-e' to vterm."
    (interactive)
    ;; We must undedicate the window so that `server-switch-buffer' will select it
    (set-window-parameter nil 'my/vterm--window-dedicated (window-dedicated-p))
    (set-window-dedicated-p (selected-window) nil)
    (vterm-send-key "x" nil nil t)
    (vterm-send-key "e" nil nil t))

  (setq-hook! 'vterm-mode-hook
    server-window #'my/vterm--display-buffer-same-window)

  (map! :map vterm-mode-map
        :i "C-x C-e" #'my/vterm-edit-indirect))

(after! vterm

  ;; This function is to be called in shell configuration to obtain the
  ;; directory in which to read aliases
  (defun my/vterm--write-user-emacs-directory (tmpfile)
    "Write the string evaluation of `user-emacs-directory' to TMPFILE."
    (f-write (concat user-emacs-directory "\n") 'utf-8 tmpfile))
  (let ((cmd 'my/vterm--write-user-emacs-directory))
    (add-to-list 'vterm-eval-cmds (list (symbol-name cmd) cmd)))

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
    "w3m"  "w3m-find-file"
    "gg"   "magit-status"))

(after! vterm
  (let ((alist (assoc-delete-all "kubectl" vterm-tramp-shells)))
    (setq vterm-tramp-shells
          (push '("kubectl" "sh -c \"clear; (bash || ash || sh)\"") alist))))

(after! vterm
  (defadvice! my/kubernetes-utils-vterm-start-a (bufname command args)
    "Fix `kubernetes-utils-vterm-start'.
The workaround is to `pop-to-buffer' for an existing buffer
instead of using a `when-let' form to conditionally kill it,
which causes problems even if there is no existing buffer."
    :override 'kubernetes-utils-vterm-start
    (let ((existing (get-buffer bufname)))
      (if existing
          ;; Do not kill the buffer!
          (pop-to-buffer existing)
        (let* ((vterm-buffer-name bufname)
               (command-str (format "%s %s" command (string-join args " ")))
               (vterm-shell command-str))
          (vterm-other-window))))))

(after! vterm
  (setq-hook! 'vterm-mode-hook
    revert-buffer-function (lambda (&rest _) (vterm-clear)))
  (map! :map vterm-mode-map
        "C-l" #'eeowaa-refresh-buffer-and-display))

(setq vterm-copy-mode-remove-fake-newlines t)

(setq vterm-buffer-name-string "%s")
(defadvice! my/vterm-popup-preserve-buffer-name-a (fn &rest args)
  "Use Doom's standard buffer name for vterm popups."
  :around #'+vterm/toggle
  (let ((vterm-environment
         `(,(format "VTERM_BUFFER_NAME=*doom:vterm-popup:%s*"
                    (if (bound-and-true-p persp-mode)
                        (safe-persp-name (get-current-persp))
                      "main")))))
    (apply fn args)))

(setq next-error-verbose nil)

(after! flycheck
  (defadvice! my/org-src-a (&rest _)
    "Consider Org Src buffers as ephemeral (do not enable flycheck)."
    :after-until #'flycheck-ephemeral-buffer-p
    (string-prefix-p "*Org Src" (buffer-name))))

(after! flycheck-posframe
  (setq flycheck-posframe-border-width 10
        flycheck-posframe-position 'point-bottom-left-corner-upward))

(after! flycheck
  (defun my/flycheck-redisplay-h ()
    (when flycheck-mode
      (flycheck-display-error-at-point-soon)))
  (add-hook! '(doom-switch-buffer-hook doom-switch-window-hook)
             #'my/flycheck-redisplay-h))

(after! flycheck-posframe
  (defun my/flycheck-posframe-update-h (&rest _)
    (when (flycheck-posframe-hidehandler _)
      (posframe-hide flycheck-posframe-buffer)))
  (add-hook! '(post-command-hook window-state-change-functions)
             #'my/flycheck-posframe-update-h))

(after! flycheck-posframe
  (flycheck-posframe-configure-pretty-defaults))
  ; (eeowaa-use-face flycheck-posframe-info-face flycheck-error-list-info)
  ; (eeowaa-use-face flycheck-posframe-warning-face flycheck-error-list-warning)
  ; (eeowaa-use-face flycheck-posframe-error-face flycheck-error-list-error))

(after! flycheck
  (setq flycheck-checker-error-threshold 500))

(after! flycheck
  (when-let ((cmd (flycheck-checker-get 'awk-gawk 'command)))
    (setcdr cmd
            '("--source" "BEGIN{exit} END{exit}"
              "-f" source
              "--lint=fatal"
              "/dev/null"))))

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

(when (and initial-window-system
           (modulep! :tools lsp)
           (not (modulep! :tools lsp +eglot)))
  (setq lsp-ui-sideline-enable nil))

(after! flycheck-posframe
  (setq flycheck-posframe-position 'window-bottom-right-corner))

(when (modulep! :checkers spell)
  (if (modulep! :checkers spell +flyspell)
      (remove-hook! '(org-mode-hook
                      markdown-mode-hook
                      TeX-mode-hook
                      rst-mode-hook
                      mu4e-compose-mode-hook
                      message-mode-hook
                      git-commit-mode-hook)
                    #'flyspell-mode)
    (remove-hook 'text-mode-hook 'spell-fu-mode)))

(when (and (modulep! :checkers spell)
           (not (modulep! :checkers spell +flyspell)))
  (require 'spell-fu))

(when (and (modulep! :checkers spell)
           (not (modulep! :checkers spell +flyspell)))
  (setq ispell-dictionary "en"
        ispell-personal-dictionary
        (expand-file-name (concat "ispell/" ispell-dictionary ".pws")
                          doom-data-dir))

  ;; Create `ispell-personal-dictionary' if the file is missing
  (after! ispell
    (if (not (file-exists-p
              (concat (file-name-as-directory ispell-aspell-data-dir) ispell-dictionary ".dat")))
        (warn (concat "ispell-dictionary \"%s\" has no corresponding .dat file in %s.\n"
                      "Modify `ispell-dictionary' or install the missing aspell language pack.")
              ispell-dictionary ispell-aspell-data-dir)
      (unless (file-exists-p ispell-personal-dictionary)
        (make-directory (file-name-directory ispell-personal-dictionary) t)
        (with-temp-file ispell-personal-dictionary
          (insert (format "personal_ws-1.1 %s 0\n" ispell-dictionary))))))

  ;; Remove Doom's `+spell--create-word-dict-a' advice (which doesn't work
  ;; for me) and just rely on the existence of `ispell-personal-dictionary'.
  (after! spell-fu
    (when (fboundp '+spell--create-word-dict-a)
      (advice-remove 'spell-fu--word-add-or-remove #'+spell--create-word-dict-a))))

(setq ispell-complete-word-dict
      (concat (file-name-as-directory (getenv "HOME"))
              ".local/share/dict/words-en_US.txt"))

(when (featurep :system 'linux)
  (let ((langtool-dir (concat (file-name-as-directory (getenv "HOME"))
                              ".local/src/doom/LanguageTool/")))
    (setq langtool-language-tool-jar (concat langtool-dir "languagetool-commandline.jar")
          langtool-language-tool-server-jar (concat langtool-dir "languagetool-server.jar"))))

(remove-hook! '(org-mode-hook
                markdown-mode-hook
                rst-mode-hook
                asciidoc-mode-hook
                latex-mode-hook
                LaTeX-mode-hook)
  #'writegood-mode)

(after! ansible
  (add-to-list 'evil-normal-state-modes 'ansible-doc-module-mode))

(after! projectile
  (add-to-list 'projectile-project-root-files "ansible.cfg"))

(setopt docker-image-columns
        '((:name "Repository" :width 49 :template "{{ json .Repository }}" :sort nil :format nil)
          (:name "Tag" :width 9 :template "{{ json .Tag }}" :sort nil :format nil)
          (:name "Id" :width 13 :template "{{ json .ID }}" :sort nil :format nil)
          (:name "Created" :width 20 :template "{{ json .CreatedAt }}" :sort nil :format (lambda (x) (format-time-string "%F %T" (date-to-time x))))
          (:name "Size" :width 10 :template "{{ json .Size }}" :sort docker-utils-human-size-predicate :format nil))

        docker-container-columns
        '((:name "Id" :width 13 :template "{{ json .ID }}" :sort nil :format nil)
          (:name "Names" :width 35 :template "{{ json .Names }}" :sort nil :format nil)
          (:name "Image" :width 41 :template "{{ json .Image }}" :sort nil :format nil)
          (:name "Command" :width 58 :template "{{ json .Command }}" :sort nil :format nil)
          (:name "Ports" :width 10 :template "{{ json .Ports }}" :sort nil :format nil)
          (:name "Status" :width 26 :template "{{ json .Status }}" :sort nil :format nil)
          (:name "Created" :width 20 :template "{{ json .CreatedAt }}" :sort nil :format (lambda (x) (format-time-string "%F %T" (date-to-time x)))))

        docker-network-columns
        '((:name "Network ID" :width 13 :template "{{ json .ID }}" :sort nil :format nil)
          (:name "Scope" :width 6 :template "{{ json .Scope }}" :sort nil :format nil)
          (:name "Driver" :width 7 :template "{{ json .Driver }}" :sort nil :format nil)
          (:name "Name" :width 50 :template "{{ json .Name }}" :sort nil :format nil)))

(map! :leader
      (:prefix-map ("o" . "open")
       "D" nil ;; remove the old keybinding
       :desc "Container" "c" #'docker))

;; Replace default association with a more generic one
(alist-get "/Dockerfile\\(?:\\.[^/\\]*\\)?\\'" auto-mode-alist nil 'remove #'string=)
(add-to-list 'auto-mode-alist
             '("/[^/\\]*\\<\\(Docker\\|Container\\)file\\>[^/\\]*$" . dockerfile-mode)
             'append)

(after! editorconfig
  (add-to-list 'editorconfig-exclude-regexps
               "/\\(?:\\(?:COMMIT\\|TAG\\|NOTES\\)_EDIT\\|MERGE_\\)MSG\\'"))

(defvar-local my/linked-buffer nil
  "The buffer that `my/send-region' sends text to.")

(defun my/region-string (beg end)
  "Return contents of active region or evil selection.
This function also works as expected on rectangular regions and
evil block selections."
  (interactive "r")
  (let (apply-on-rectangle-fn)
    (cond
     (rectangle-mark-mode
      (setq apply-on-rectangle-fn #'apply-on-rectangle))
     ((and (eq 'visual (bound-and-true-p evil-state))
           (eq evil-visual-selection 'block))
      (setq apply-on-rectangle-fn #'evil-apply-on-rectangle)))
    (if apply-on-rectangle-fn
        (cl-letf ((lines (list nil))
                  ((symbol-function 'buffer-substring) #'buffer-substring-no-properties))
          (funcall apply-on-rectangle-fn #'extract-rectangle-line beg end lines)
          (setq lines (nreverse (cdr lines)))
          (mapconcat #'identity lines "\n"))
      (buffer-substring-no-properties beg end))))

(defun my/send-region (beg end)
  "Send text in region to a linked buffer.

If the current buffer does not have a linked buffer, or given a
non-nil prefix argument, this function will prompt for a buffer
and set the linked buffer accordingly.

Indentation is removed from the region before sending to the
linked buffer; the indentation level is defined by the first
line. This helps when sending indented source blocks (e.g. in
`org-mode' and `markdown-mode') to REPLs.

Trailing whitespace is also removed from the region before
sending to the linked buffer; however, only one trailing newline
is removed. This helps in situations where consecutive trailing
newlines have special meaning, such as in Python function
definitions.

Contrast this function to `send-region', which sends a region to
a process instead of another buffer. Use `my/send-region' instead
if you want to send region to a REPL or terminal emulator."
  (interactive "r")
  (let* ((region-string (my/region-string beg end))
         (region-noindent (with-temp-buffer
                            (insert region-string)
                            (goto-char (point-min))
                            (indent-rigidly (point) (point-max) (- (current-indentation)))
                            (buffer-string)))
         (trailing-newlines (if (string-match "\\(\\(?:\r?\n\\)+\\)\r?\n\\'" region-noindent)
                                (match-string 1 region-noindent)
                              ""))
         (text (concat (string-trim-right region-noindent) trailing-newlines))
         (prompt (or (and (or (null my/linked-buffer) current-prefix-arg)
                          "Select a buffer to link: ")
                     (and (not (buffer-live-p my/linked-buffer))
                          "Linked buffer is invalid. Select a new buffer: "))))
    (when prompt
      (setq my/linked-buffer
            (get-buffer (read-buffer-to-switch prompt))))
    (with-current-buffer my/linked-buffer
      ;; TODO Test with `term', `shell', `eshell', various REPLs, and the
      ;; scratch buffer. There is probably a better way to do this.
      (cond
        ((eq major-mode 'vterm-mode)
         (vterm-insert text)
         (vterm-send-return))
        ((derived-mode-p 'comint-mode)
         (end-of-buffer)
         ;; TODO Do not block execution waiting for output
         (let ((comint-process-echoes t))
           (dolist (line (split-string text "\r?\n"))
             (insert line)
             (comint-send-input))))
        (t
         (message "Unsupported mode: %s" major-mode))))))

(map! :v (kbd "C-c e") #'my/send-region)

(after! comint
  (defun my/comint-clear ()
    "Scroll the prompt to the top of the window."
    (interactive)
    (cl-assert (derived-mode-p 'comint-mode))
    (let ((inhibit-redisplay t))
      (end-of-buffer)
      (comint-next-prompt 1))
    (recenter 0))
  (defalias 'my/comint-clear-scrollback #'comint-clear-buffer)
  (setq-hook! 'comint-mode-hook
    revert-buffer-function (lambda (&rest _) (my/comint-clear)))
  (map! :map comint-mode-map
        "C-c C-l" #'my/comint-clear-scrollback
        :i "C-d" #'comint-send-eof)
  (setq-default comint-scroll-show-maximum-output nil))

(defadvice! my/+lookup-project-search-backend-fn-a (identifier)
  :override #'+lookup-project-search-backend-fn
  (when identifier ;; Replaced `unless' with `when'
    (let ((query (rxt-quote-pcre identifier)))
      (ignore-errors
        (cond ((modulep! :completion ivy)
               (+ivy-file-search :query query)
               t)
              ((modulep! :completion helm)
               (+helm-file-search :query query)
               t)
              ((modulep! :completion vertico)
               (+vertico-file-search :query query)
               t))))))

(defvar my/lookup-dictionary-prefer-offline nil)
(defadvice! my/lookup-dictionary-prefer-online-a (fn &rest args)
  :around '(+lookup/dictionary-definition +lookup/synonyms)
  (let ((+lookup-dictionary-prefer-offline
         (or my/lookup-dictionary-prefer-offline
             (not (eeowaa-onlinep)))))
    (apply fn args)))

(setq eldoc-echo-area-use-multiline-p nil
      eldoc-echo-area-display-truncation-message nil)

(defvar my/select-doc-modes '(emacs-lisp-mode helpful-mode)
  "Modes where `+lookup/documentation' should pop to the doc buffer.")

(defadvice! my/display-doc-buffer-a (fn &rest args)
  "Display the documentation buffer without selecting it."
  :around #'+lookup/documentation
  (if (memq major-mode my/select-doc-modes)
      (apply fn args)
    (letf! ((#'pop-to-buffer #'display-buffer))
      (let (help-window-select)
        (apply fn args)))))

(after! dumb-jump
  (setq dumb-jump-selector 'completing-read))

(after! synosaurus-wordnet
  (setq synosaurus-wordnet--options '("-synsv" "-synsn" "-synsa")))

(after! wordnut
  ;; HACK This modifies a `defconst'
  (setq wordnut-cmd-options
        (seq-remove (lambda (s) (string-match-p "r\\'" s))
                    wordnut-cmd-options)))

(add-hook 'wordnut-mode-hook #'outline-minor-mode)

(after! dash-docs
  (when (modulep! :tools lookup +docsets)
    (let ((zeal-docsets
           (concat (file-name-as-directory (getenv "XDG_DATA_HOME"))
                   "Zeal/Zeal/docsets/")))
      (when (file-accessible-directory-p zeal-docsets)
        (setq dash-docs-docsets-path zeal-docsets)))))

(setq magit-repository-directories
      '(("~/Documents/src" . 2) ;; code I work on professionally or personally
        ("~/.local/src" . 2) ;; 3rd-party code (often used for local builds)
        ("~/Documents/notes/life/ref" . 0))) ;; version-controlled personal notes

(map! (:after evil-collection-magit
       (:map magit-status-mode-map
        :nv "C-t" nil ;; open this up for `vimish-tab'
        :nv "gz" #'magit-jump-to-stashes)))

(after! magit-status
  (require 'magit-bisect))

;; Performance improvements for `magit' under WSL 1
(when (eq eeowaa-wsl-version 1)

  ;; Do not refresh `diff-hl' fringes when refreshing `magit'. This is an easy way
  ;; to speed things up (discovered by running `magit-profile-refresh-buffers').
  (after! diff-hl
    (remove-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; Do not refresh the status buffer from, say, a diff buffer. This way,
  ;; you can stage hunks from an "unstaged diff" buffer for a single file without
  ;; incurring the cost of a full magit refresh.
  (setq magit-refresh-status-buffer nil))

(defvar my/makefile-search-list '("GNUmakefile" "makefile" "Makefile")
  "List of makefile names to sequentially search for.")

(defadvice! my/+make/run-a ()
  :override #'+make/run
  (interactive)
  (if (doom-project-p)
      (makefile-executor-execute-project-target)
    (let ((makefile (cl-loop with buffer-file = (or buffer-file-name default-directory)
                             for file in my/makefile-search-list
                             if (locate-dominating-file buffer-file file)
                             return file)))
      (unless makefile
        (user-error "Cannot find a makefile in the current project"))
      (let ((default-directory (file-name-directory makefile)))
        (makefile-executor-execute-target makefile)))))

(after! makefile-executor
  (defadvice! my/makefile-executor--initial-input-a (files)
    :override #'makefile-executor--initial-input
    (let* ((bn (or (buffer-file-name) default-directory))
           ;; NOTE `makefile-executor' requires `dash', but not `cl-lib'
           (fn (-first (lambda (m) (locate-dominating-file bn m))
                        my/makefile-search-list))
           (relpath (file-relative-name fn (projectile-project-root))))
      (if (not (s-equals? relpath "./"))
          relpath
        ""))))

(after! makefile-executor
  (defvar my/makefile-executor-file-variable "EMACS__MAKEFILE__FILE")
  (setq makefile-executor-list-target-code
        (format "%s := $(lastword $(MAKEFILE_LIST))\n.PHONY: %s\n%s:\n	@LC_ALL=C $(MAKE) -pRrq -f $(%s) : 2>/dev/null | awk -v RS= -F: '/(^|\\n)# Files(\\n|$$)/,/(^|\\n)# Finished Make data base/ {if ($$1 !~ \"^[#.]\") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$'\n"
                my/makefile-executor-file-variable makefile-executor-special-target makefile-executor-special-target my/makefile-executor-file-variable))
  (defadvice! my/makefile-executor-get-targets-a (filename)
    :override #'makefile-executor-get-targets
    (let* ((file (make-temp-file "makefile"))
           (makefile-contents
            (concat
             makefile-executor-list-target-code "\n"
             (with-temp-buffer
               (insert-file-contents filename)
               (buffer-string)))))
      (f-write-text makefile-contents 'utf-8 file)
      (let ((out (shell-command-to-string
                  (format "make -f %s %s"
                          (shell-quote-argument file)
                          makefile-executor-special-target))))
        (delete-file file)
        (s-split "\n" out t)))))

(after! makefile-executor
  (defadvice! my/with-completing-read-lenient-a (fn &rest args)
    :around #'makefile-executor-execute-project-target
    (letf! ((defun my/completing-read-lenient-a (args)
              (setf (nth 3 args) nil)
              args)
            (defadvice #'completing-read :filter-args
                       #'my/completing-read-lenient-a))
      (apply fn args))))

(map! :leader
      (:prefix-map ("c" . "code")
       :desc "Make target"      "m" #'+make/run
       :desc "Make last target" "M" #'+make/run-last))

(use-package! rfc-mode
  :defer t
  :init
  (defalias 'rfc #'rfc-mode-browse)

  (defadvice! +rfc--read-index-a (buffer)
    :override #'rfc-mode-read-index
    (with-current-buffer buffer
      (goto-char (point-min))
      (let ((entries nil))
        (while (search-forward-regexp "^[0-9]+ " nil t)
          (let ((start (match-beginning 0)))
            ;; HACK Replace a failing-to-match regex: " $"
            (search-forward-regexp "^$")
            (let* ((end (match-beginning 0))
                   (lines (buffer-substring start end))
                   (entry-string (replace-regexp-in-string "[ \n]+" " " lines))
                   (entry (rfc-mode-parse-index-entry entry-string)))
              (unless (string= (plist-get entry :title) "Not Issued")
                (push entry entries)))))
        (nreverse entries))))

  (defadvice! +rfc--goto-top-a ()
    :after #'rfc-mode-init
    (goto-char (point-min))
    (while (looking-at-p "^$")
      (forward-line))
    ;; HACK Avoid "rfc-mode-recenter: `recenter'ing a window that does not display current-buffer."
    ;; <https://emacs.stackexchange.com/questions/77226/recentering-a-window-that-does-not-display-current-buffer>
    ;; <https://lists.gnu.org/r/emacs-devel/2014-06/msg00094.html>
    (if (eq (window-buffer) (current-buffer))
        (rfc-mode-recenter)
      (add-transient-hook! 'window-buffer-change-functions
        (rfc-mode-recenter))))

  (defadvice! +rfc--maybe-recenter-a (&rest _)
    :after '(rfc-mode-backward-page rfc-mode-previous-section)
    (when (bobp)
      (while (looking-at-p "^$")
        (forward-line))
      (rfc-mode-recenter)))

  (when (modulep! :completion vertico)
    (defadvice! +rfc--no-sort-a (args)
      :filter-args #'rfc-mode-goto-section
      (interactive
       (lambda (spec)
         (let ((vertico-sort-override-function #'identity))
           (advice-eval-interactive-spec spec))))
      args))

  :config
  (setq rfc-mode-directory
        (concat doom-cache-dir "rfc"))

  (add-hook 'rfc-mode-hook #'read-only-mode)
  (when (fboundp 'page-break-lines-mode)
    (add-hook 'rfc-mode-hook #'page-break-lines-mode))

  (map! :map rfc-mode-map
        :n "gm" #'rfc-mode-browse
        :n "[" #'rfc-mode-backward-page
        :n "]" #'rfc-mode-forward-page
        :n "C-k" #'rfc-mode-previous-section
        :n "C-j" #'rfc-mode-next-section
        :n "C-i" #'forward-button
        :n "C-M-i" #'backward-button
        (:localleader
         :n "." #'rfc-mode-goto-section)))

(add-hook 'terraform-mode-local-vars-hook #'tree-sitter! 'append)

;; Missing from evil-textobj-tree-sitter.el:
(after! evil-textobj-tree-sitter
  (add-to-list 'evil-textobj-tree-sitter-major-mode-language-alist '(terraform-mode . "hcl")))

(after! ts-fold
  (defun my/ts-fold-summary-test (&optional summary-parser)
    "Extract summary text for tree-sitter fold at point.
SUMMARY-PARSER is a function such as one defined in
`ts-fold-summary-parsers-alist'. IF SUMMARY-PARSER is omitted, it
defaults to the one returned by `ts-fold-summary--parser'.

See also: `ts-fold-summary--get'."
    (interactive)
    (let* ((parser (or summary-parser (cdr (ts-fold-summary--parser))))
           (node (ts-fold--foldable-node-at-pos))
           (range (ts-fold--get-fold-range node))
           (beg (car range))
           (end (cdr range)))
      (message (funcall parser (buffer-substring beg end))))))

(setq lsp-semantic-tokens-enable nil)

(add-to-list 'auto-mode-alist '("\\.cnf\\'" . conf-mode))

(use-package! x509-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:pem\\|der\\|key\\|crt\\|cer\\|crl\\)\\'" . x509-mode))
  :config
  (map! :map x509-mode-map
        :n "e" #'x509--edit-params
        :n "q" #'x509-mode--kill-buffer
        :n "t" #'x509--toggle-mode)

  (map! :map x509-asn1-mode-map
        :n "d" #'x509--asn1-offset-down
        :n "e" #'x509--edit-params
        :n "q" #'x509-mode--kill-buffer
        :n "s" #'x509--asn1-strparse
        :n "t" #'x509--toggle-mode
        :n "u" #'x509--asn1-offset-up
        :n "x" #'x509-asn1-toggle-hexl))

(when (featurep :system 'macos)
  (setq ;; Comfortable keys that work most of the time
        mac-command-modifier 'control
        mac-right-command-modifier 'meta

        ;; Workaround for when system keybindings take precedence
        mac-control-modifier 'control
        mac-right-control-modifier 'meta

        ;; For exotic mappings
        mac-option-modifier 'super
        mac-right-option-modifier 'hyper))

(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; Adapted from protobuf-mode.el documentation
(defconst my/protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))
(add-hook 'protobuf-mode-hook
          (defun my/protobuf-indentation-setup-h ()
            (c-add-style "my/protobuf-style" my/protobuf-style t)))

(add-hook 'desktop-entry-mode-hook #'font-lock-update)

(after! markup-faces
  (dolist (sub-group (mapcar #'car (custom-group-members 'markup-faces t)))
    (dolist (group-member (custom-group-members sub-group nil))
      (when (eq (cadr group-member) 'custom-face)
        (set-face-attribute (car group-member) nil
          :height 1.0)))))

(after! markdown-mode
  (add-to-list 'markdown-code-lang-modes '("batch" . bat-mode)))
(after! org-src
  (add-to-list 'org-src-lang-modes '("batch" . bat)))

(setq-hook! 'c-mode-hook
  lsp-lens-enable nil)

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
  (add-hook! c-mode
   (setq tab-width
         (alist-get 'c-basic-offset (assoc "linux" c-style-alist)))))

;; Modified from commit ad6a3d0f3 of lang/csharp/config.el
(use-package! sharper
  :when (modulep! :lang csharp +dotnet)
  :general ("C-c n" #'sharper-main-transient)
  :config
  (map! (:map sharper--solution-management-mode-map
         :nv "RET" #'sharper-transient-solution
         :nv "gr" #'sharper--solution-management-refresh)
        (:map sharper--project-references-mode-map
         :nv "RET" #'sharper-transient-project-references
         :nv "gr" #'sharper--project-references-refresh)
        (:map sharper--project-packages-mode-map
         :nv "RET" #'sharper-transient-project-packages
         :nv "gr" #'sharper--project-packages-refresh)
        (:map sharper--nuget-results-mode-map
         :nv "RET" #'sharper--nuget-search-install)))

(dolist (mode '(("\\.xaml\\'" . nxml-mode)
                ("\\.DotSettings\\'" . nxml-mode)))
  (add-to-list 'auto-mode-alist mode))

(after! hideshow
  (add-to-list 'hs-special-modes-alist
               (cons 'csproj-mode (alist-get 'nxml-mode hs-special-modes-alist))))

(after! csharp-mode
  (add-to-list 'markdown-code-lang-modes '("c#" . csharp-mode)))
(after! org-src
  (add-to-list 'org-src-lang-modes '("c#" . csharp)))

(after! ws-butler
  (add-to-list 'ws-butler-global-exempt-modes 'tsv-mode))

(when (boundp 'lsp-server-install-dir)
  (setq lsp-xml-jar-file
        (expand-file-name "xmlls/org.eclipse.lemminx-uber.jar" lsp-server-install-dir)))

(add-to-list 'auto-mode-alist '("/CODEOWNERS\\(?:\\.md\\)?\\'" . conf-mode))

(setq-hook! 'emacs-lisp-mode-hook
  eldoc-idle-delay 0.1)

(defadvice! my/show-function-docstring-a (fn sym &rest r)
  "If SYM is a function, append its docstring."
  :around #'elisp-get-fnsym-args-string
  (concat
   (apply fn sym r)
   (let* ((doc (and (fboundp sym) (documentation sym 'raw)))
          (oneline (and doc (substring doc 0 (string-match "\n" doc)))))
     (and oneline
          (stringp oneline)
          (not (string= "" oneline))
          (concat " | " (propertize oneline 'face 'italic))))))

(add-to-list 'auto-mode-alist '("Cask\\'" . lisp-data-mode))

(defadvice! my/format-result-a (f &rest r)
  "Prepend \";; =>\"."
  :around #'eval-print-last-sexp
  (let ((p (point)))
    (apply f r)
    (save-excursion
      (goto-char p)
      (forward-line 1)
      (insert ";; => "))))

(after! dap-node
  (defadvice! my/dap-node--populate-start-file-args-a (conf)
    "Fix file paths used by the node debugger.
1. Use absolute paths by wrapping path strings in `expand-file-name'.
2. Prompt for the working directory instead of assuming `default-directory'."
    :override #'dap-node--populate-start-file-args
    (let ((conf (-> conf
                    (dap--put-if-absent :dap-server-path dap-node-debug-program)
                    (dap--put-if-absent :type "node")
                    (dap--put-if-absent :name "Node Debug"))))
      (if (plist-get conf :args)
          conf
        (dap--put-if-absent
         conf :program (expand-file-name
                        (read-file-name "Select the file to run:" nil (buffer-file-name) t)))
        (dap--put-if-absent
         conf :cwd (expand-file-name
                     (read-directory-name "Select the working directory:" nil default-directory t)))))))

(add-to-list 'auto-mode-alist '("\\.npmignore\\'" . gitignore-mode))

(after! markdown-mode
  (defun my/markdown-preview (f &rest r)
    (let ((browse-url-browser-function #'browse-url-default-browser))
      (apply f r)))
  (advice-add 'markdown-preview :around #'my/markdown-preview))

(setq markdown-fontify-code-blocks-natively t)
(defadvice! my/markdown-fontify-code-block-a (fn &rest args)
  "Prevent `lsp!' and `tree-sitter!' from running during fontification."
  :around #'markdown-fontify-code-block-natively
  (let (sh-mode-local-vars-hook)
    (apply fn args)))

(after! markdown-mode
  (defun my/markdown-edit-code-block (f &rest r)
    (let ((display-buffer-overriding-action (list #'display-buffer-same-window)))
      (apply f r)))
  (advice-add 'markdown-edit-code-block :around #'my/markdown-edit-code-block))

(cond
 ((modulep! :completion ivy)
  (defalias 'counsel-markdown-goto #'counsel-outline)
  (map! :map markdown-mode-map
        :localleader
        "." #'counsel-markdown-goto))
 ((modulep! :completion vertico)
  (defalias 'consult-markdown-goto #'consult-outline)
  (map! :map markdown-mode-map
        :localleader
        "." #'consult-markdown-goto)))

(setq markdown-enable-math nil)

(after! evil-markdown
  (evil-define-text-object my/evil-markdown-inner-element (count &optional beg end type)
    "Inner markdown element.
Currently only includes code blocks."
    (let* ((bounds (markdown-get-enclosing-fenced-block-construct))
           (begin (and bounds (not (null (nth 0 bounds))) (goto-char (nth 0 bounds)) (point-at-bol 2)))
           (end (and bounds (not (null (nth 1 bounds))) (goto-char (nth 1 bounds)) (point-at-bol 1))))
      (list begin end)))
  (evil-define-key '(visual operator) evil-markdown-mode-map
    "ie" #'my/evil-markdown-inner-element))

(after! evil-markdown
  (map! :map evil-markdown-mode-map
        :i "M-b" nil
        :i "M-i" nil))

(after! markdown-mode
  (map! :map markdown-mode-map
        :localleader
        "a" #'markdown-table-align))

(dolist (mode '(markdown-mode markdown-view-mode
                gfm-mode gfm-view-mode))
  ;; Prevent flycheck from being automatically enabled
  (eeowaa-add-to-exclusion-list flycheck-global-modes mode)

  ;; Prevent lsp diagnostics from being enabled
  (if (boundp 'lsp-diagnostics-disabled-modes)
      (add-to-list 'lsp-diagnostics-disabled-modes mode)
    (setq lsp-diagnostics-disabled-modes (list mode))))

;; Don't bother checking for an LSP diagnostics provider in markdown-mode
;; and its derived modes (markdown-view-mode, gfm-mode, and gfm-view-mode)
(setq-hook! 'markdown-mode-hook
  lsp-diagnostics-provider :none)

(dolist (mode '(("\\.mdx\\'" . markdown-mode)
                ("/\\.markdownlintrc\\'" . json-mode)))
  (add-to-list 'auto-mode-alist mode))

(after! markdown-mode
  (setopt markdown-fontify-whole-heading-line nil
          markdown-header-scaling t)
  (add-hook! markdown-mode
    (defun my/markdown--update-display-h ()
      "Apply desired styling to a markdown buffer."
      (markdown-update-header-faces markdown-header-scaling))))

(after! markdown-mode

  (defvar my/markdown-regex-atuser
    "\\(?:^\\|[^[:word:]_]\\)\\(@\\(?:\\sw\\|\\s_\\|\\s.\\)+\\)\\_>"
    "Regular expression for matching @user strings.")

  (defun my/markdown-match-atuser (last)
    "Match @user strings from point to LAST."
    (when (re-search-forward my/markdown-regex-atuser last t)
      (set-match-data (list (match-beginning 1) (match-end 1)))
      t))

  (defun my/markdown-fontify-atuser (last)
    "Add text properties to @user strings from point to LAST."
    (when (my/markdown-match-atuser last)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (props (list 'face 'font-lock-builtin-face
                          'rear-nonsticky t
                          'front-sticky nil)))
        (add-text-properties start end props)
        t)))

  (add-to-list 'markdown-mode-font-lock-keywords '(my/markdown-fontify-atuser) 'append))

;; The `hideshow' configuration for `nxml-mode' works well for HTML tags
;; embedded in `markdown-mode' buffers and those of derived modes.
(after! hideshow
  (dolist (mode '(markdown-mode gfm-mode))
    (add-to-list 'hs-special-modes-alist
                 (cons 'markdown-mode (alist-get 'nxml-mode hs-special-modes-alist)))))

;; Need `sgml-skip-tag-forward' for `hideshow' to fold HTML tags.
(after! (:and hideshow markdown-mode)
  (require 'sgml-mode))

(after! markdown-mode
  (dolist (mode '(("http" . restclient-mode)
                  ("sh" . bash-mode)))
    (add-to-list 'markdown-code-lang-modes mode)))

;; HACK `grip-start-preview' calls `derived-mode-p' with a list of modes, when
;; each mode should be passed as a separate argument.
(after! grip-mode
  (defadvice! my/grip-start-preview-a ()
    :override #'grip-start-preview
    (interactive)
    (when buffer-file-name
      (add-hook 'kill-buffer-hook #'grip-stop-preview nil t)
      (add-hook 'kill-emacs-hook #'grip-stop-preview nil t)
      (cond ((derived-mode-p 'org-mode)
             (grip--preview-org))
            ((derived-mode-p 'markdown-mode 'markdown-ts-mode)
             (grip--preview-md))
            (t
             (grip-mode -1)
             (user-error "`%s' not supported by grip preview" major-mode))))))

(after! org
  (setq org-hide-leading-stars nil
        org-startup-indented nil
        org-adapt-indentation nil))

(defadvice! my/inhibit-indentation-a (f &rest r)
  "Prevent `org-indent-mode' from running."
  :around #'doom-docs-org-mode
  (letf! ((#'org-indent-mode #'ignore))
    (apply f r)))

(after! evil-org
  (map! :map evil-org-mode-map
        :i "C-j" #'+default/newline-below))

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
    (let ((inhibit-redisplay t))
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
         (t (forward-line 1) (newline))))))

  (defun my/org-insert-heading-visibility ()
    "Redisplay the previous Org heading.

I cannot figure out where the visibility state is saved, so I
just perform a complete cycle of `org-cycle'."
    (let ((inhibit-redisplay t))
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

  (add-hook! 'org-insert-heading-hook #'my/org-insert-heading-spacing
                                      #'my/org-insert-heading-visibility
                                      #'my/org-insert-heading-evil-state))

(after! org
  (dolist (module '(ol-bookmark ol-man ol-info ol-w3m))
    (add-to-list 'org-modules module)))

(after! org
  (setcdr (assoc 'file org-link-frame-setup) #'find-file-other-window)
  (defadvice! my/org-bookmark-open-a (bookmark _)
    :override #'org-bookmark-open
    (bookmark-jump-other-window bookmark)))

(setq org-man-command
      (if (and (not (featurep :system 'macos)) (executable-find "man"))
          #'man
        #'woman))

(setq org-link-context-for-files nil)

;; (setq org-bookmark-when-visiting-a-file t
;;       org-bookmark-use-first-bookmark nil)

;; Optional: automatically prompt for and display bookmark annotations
;; (setq bookmark-use-annotations t
;;       bookmark-automatically-show-annotations t)

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

(when (modulep! :completion ivy)
  (setq counsel-org-goto-all-outline-path-prefix 'buffer-name))

(after! org
  (setq org-refile-use-outline-path 'full-file-path))

(setq
      ;; Top-level directory (used by `+default/find-in-notes', etc.)
      org-directory "~/Documents/notes"

      ;; Directories to search for agenda files
      my/org-directories `("work" "life" ,doom-user-dir)
      org-agenda-files (mapcar (lambda (f)
                                 (file-name-as-directory
                                  (if (file-name-absolute-p f) f
                                    (expand-file-name f org-directory))))
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

(after! org
  (defadvice! my/org-footnote-sort-a (&rest _)
    "Do not insert a leading newline before new footnote definitions."
    :override #'org-footnote-sort
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
                (insert definition "\n"))))))))))

(setq org-export-babel-evaluate nil)

(use-package! ox-ipynb
  :when (modulep! :lang org +jupyter)
  :after ox
  :config

  ;; Define an `ox-ipynb' export function that excludes results
  (defun my/ox-ipynb-export-to-ipynb-no-results-file
      (&optional async subtreep visible-only body-only info)
    "Export current buffer to a file. Strip results first.
Optional argument ASYNC to asynchronously export.
Optional argument SUBTREEP to export current subtree.
Optional argument VISIBLE-ONLY to only export visible content.
Optional argument BODY-ONLY export only the body.
Optional argument INFO is a plist of options."
    (let ((ox-ipynb-preprocess-hook
           '((lambda ()
               (org-babel-map-src-blocks nil (org-babel-remove-result))))))
      (ox-ipynb-export-to-ipynb-file)))

  ;; Slightly modified from `ob-ipynb' (added dispatch to new export function)
  (org-export-define-derived-backend 'jupyter-notebook 'org
    :menu-entry
    '(?n "Export to jupyter notebook"
         ((?b "to buffer" ox-ipynb-export-to-ipynb-buffer)
          (?n "to notebook" ox-ipynb-export-to-ipynb-file)
          (?N "to nb (no results)" my/ox-ipynb-export-to-ipynb-no-results-file)
          (?o "to notebook and open" ox-ipynb-export-to-ipynb-file-and-open)
          (?p "to participant nb & open" ox-ipynb-export-to-participant-notebook)
          (?r "to nb (no results) and open" ox-ipynb-export-to-ipynb-no-results-file-and-open)
          (?s "to slides and open" ox-ipynb-export-to-ipynb-slides-and-open))))

  ;; Support exporting to bash kernel notebooks
  (let ((kernelspecs (assoc-delete-all 'sh ox-ipynb-kernelspecs))
        (language-infos (assoc-delete-all 'sh ox-ipynb-language-infos)))
    (setq ox-ipynb-kernelspecs
          (push '(sh . (kernelspec . ((display_name . "Bash")
                                      (language . "bash")
                                      (name . "bash")))) kernelspecs)
          ox-ipynb-language-infos
          (push '(sh . (language_info . ((codemirror_mode . "shell")
                                         (file_extension . ".sh")
                                         (mimetype . "text/x-sh")
                                         (name . "bash")
                                         (pygments_lexer . "bash")))) language-infos)))

  ;; Support exporting from `jupyter-LANG' and `ein-LANG' source blocks
  (dolist (pair '((ipython . "python")
                  (sh . "bash")))
    (let* ((key (car pair))
           (lang (cdr pair))
           (kernelspec (alist-get key ox-ipynb-kernelspecs))
           (language-info (alist-get key ox-ipynb-language-infos))
           (extra-keys (cons (intern (concat "jupyter-" lang))
                             (when (modulep! :tools ein)
                               (list (intern (concat "ein-" lang)))))))
      (dolist (ext-key extra-keys)
        (add-to-list 'ox-ipynb-kernelspecs (cons ext-key kernelspec))
        (add-to-list 'ox-ipynb-language-infos (cons ext-key language-info))))))

(after! org
  (setq org-src-preserve-indentation nil
        org-edit-src-content-indentation 0))

(after! org
  (setq org-src-window-setup 'current-window)
  (when (modulep! :ui popup)
    (setq +popup--display-buffer-alist
          (delq (assoc "^\\*Org Src" +popup--display-buffer-alist)
                +popup--display-buffer-alist))
    (when (bound-and-true-p +popup-mode)
      (setq display-buffer-alist +popup--display-buffer-alist))))

(when (modulep! :lang org +jupyter)
  (after! ob-async
    (add-to-list 'ob-async-no-async-languages-alist "jupyter-bash"))

  (after! org-src
    (cl-pushnew '("jupyter-bash" . bash)
                  org-src-lang-modes :key #'car)))

(defvar my/jupyter-envvars nil
  "List of environment variable overrides for `jupyter'.
This is a list of lists, not a list of cons cells.")

(after! jupyter-env
  (defadvice! my/jupyter-env-a (fn &rest args)
    "Execute jupyter in a modified environment."
    :around '(jupyter-command jupyter-run-repl)
    (eval (macroexpand-1
           `(letenv! ,my/jupyter-envvars
              (apply fn args))))))

(add-to-list 'my/jupyter-envvars '("PYDEVD_DISABLE_FILE_VALIDATION" "1"))

(defun my/pipx-local-venvs ()
  "Return value of PIPX_LOCAL_VENVS as a directory name."
  (file-name-as-directory
   (string-trim-right
    (shell-command-to-string "pipx environment -v PIPX_LOCAL_VENVS"))))

(let* ((venv (concat (my/pipx-local-venvs) "jupyter"))
       (bindir (concat (file-name-as-directory venv) "bin")))
  (dolist (envvar `(("VIRTUAL_ENV" ,venv)
                    ("PATH" ,(concat bindir (path-separator) (getenv "PATH")))))
    (add-to-list 'my/jupyter-envvars envvar)))

(after! zmq
  (defadvice! my/fix-zmq-build-a (fn &rest args)
    :around #'zmq-load
    (letf! ((defun my/zmq-compile-a (command)
              (let ((configure (concat command " configure"))
                    (make command))
                (format "sh -c \"%s && %s" configure make)))
            (defadvice #'compile :filter-args
                       #'my/zmq-compile-a))
      (funcall fn args))))

(after! projectile
  (dolist (mode '(("org" "el")
                  ("el" "org")))
    (add-to-list 'projectile-other-file-alist mode)))

(after! ob
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))

(require 'org-mouse)

(setq org-ditaa-jar-path
      (cond
       (IS-MAC (file-expand-wildcards "/usr/local/Cellar/ditaa/*/libexec/ditaa-*-standalone.jar"))
       ((file-exists-p "/usr/share/java/ditaa.jar") "/usr/share/java/ditaa.jar")))

(setq org-download-method 'directory
      org-download-image-dir "images")

(when (modulep! :lang org +pomodoro)
  (require 'org-pomodoro))

(map! :map org-mode-map
      :localleader
      (:when (modulep! :lang org +pomodoro)
       (:prefix ("c" . "clock")
        "p" #'org-pomodoro
        "P" #'org-pomodoro-extend-last-clock)))

(when (and (modulep! :lang org +pomodoro)
           (equal org-pomodoro-audio-player "/usr/bin/afplay"))
  (let ((args '("-v" "0.125")))
    (setq org-pomodoro-start-sound-args args
          org-pomodoro-finished-sound-args args
          org-pomodoro-overtime-sound-args args
          org-pomodoro-ticking-sound-args args
          org-pomodoro-killed-sound-args args
          org-pomodoro-short-break-sound-args args
          org-pomodoro-long-break-sound-args args)))

;; Add "pipenv" label to "e" prefix key
(after! pipenv
  (map! :map python-mode-map
        :localleader
        :prefix ("e" . "pipenv")))

(add-hook! python-mode
  (defun my/python-fill-column-h ()
    (setq fill-column 79)))

(dolist (mode '(("pylint" . conf-mode)
                ("/activate\\'" . sh-mode)))
  (add-to-list 'auto-mode-alist mode))

(after! projectile
  (add-to-list 'projectile-project-root-files "pyvenv.cfg")
  (add-to-list 'projectile-project-search-path
               `(,(concat (file-name-as-directory (getenv "HOME")) ".local/pipx/venvs") . 1)))

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(setq-default sh-shell-file "/bin/sh")

;; Prevent flycheck from being automatically enabled
(eeowaa-add-to-exclusion-list flycheck-global-modes 'sh-mode)

;; Prevent lsp diagnostics from being enabled
(if (boundp 'lsp-diagnostics-disabled-modes)
    (add-to-list 'lsp-diagnostics-disabled-modes 'sh-mode)
  (setq lsp-diagnostics-disabled-modes '(sh-mode)))

;; Don't bother checking for an LSP diagnostics provider in sh-mode
(setq-hook! 'sh-mode-hook
  lsp-diagnostics-provider :none)

(dolist (mode '(("/\\.config/\\(shell\\|bash\\)/.+" . sh-mode)
                ("\\.\\(env\\|cygport\\)\\'" . sh-mode)))
  (add-to-list 'auto-mode-alist mode))

(add-hook 'sh-mode-local-vars-hook #'outline-minor-mode)

(after! sh-script
  (when (modulep! :lang sh +lsp)
    (remove-hook 'sh-mode-local-vars-hook #'lsp!)
    (add-hook 'sh-mode-local-vars-hook
              (defun my/sh-mode-lsp-maybe ()
                "Activate LSP in `sh-mode' if possible."
                ;; Taken from `lsp-bash-check-sh-shell'
                (and (memq major-mode '(sh-mode bash-ts-mode))
                     (memq sh-shell '(sh bash))
                     (lsp!)))
              'append)))

(eeowaa-add-to-exclusion-list +tree-sitter-hl-enabled-modes 'sh-mode)

(add-to-list 'hs-special-modes-alist
             '(sh-mode "{{{" "}}}" "#"))

(map! :v "C-c '" #'edit-indirect-region)

(after! lsp-yaml
  (let ((f lsp-yaml-schema-store-local-db))
    (unless (file-exists-p f)
      (mkdir (file-name-directory f) t)
      (lsp-yaml-download-schema-store-db))))

(put 'lsp-yaml-schemas 'safe-local-variable #'always)

(when (modulep! :tools tree-sitter)
  (eeowaa-add-to-exclusion-list +tree-sitter-hl-enabled-modes 'yaml-mode))

;; Open online documentation in `w3m'
(setq +lookup-open-url-fn #'w3m-browse-url)

(when (modulep! :tools lookup +docsets)
  (setq dash-docs-browser-func #'w3m))

;; Use Google as your home page
(setq w3m-home-page "https://www.google.com/")

;; Do not use tabs
(setopt w3m-display-mode 'plain)

;; Use the header line to display the page title and location
(setopt w3m-use-tab-line nil
        w3m-use-header-line-title t
        w3m-use-header-line t)

;; Reverting the buffer reloads the page
(setq-hook! 'w3m-mode-hook
  revert-buffer-function (lambda (&rest _) (w3m-reload-this-page)))

(defun my/w3m-form-at-point-p ()
  (get-text-property (point) 'w3m-action))

(defun my/w3m-edit-form-at-point ()
  "Edit the form at point"
  (interactive)
  (if (my/w3m-form-at-point-p)
      (w3m-view-this-url)
    (message "No form at point")))

(after! evil-collection-w3m
  (evil-collection-define-key 'normal 'w3m-mode-map
    "i" #'my/w3m-edit-form-at-point))

(after! w3m
  (require 'diff)
  (require 'info)
  (custom-set-faces!
    ;; Links
    '(w3m-anchor :inherit link)
    '(w3m-anchor-arrived :inherit link-visited)
    ;; Edited text
    '(w3m-strike-through :inherit diff-removed)
    '(w3m-insert :inherit diff-added)
    ;; Header line
    '(w3m-header-line-background :inherit header-line)
    '(w3m-header-line-content :inherit info-header-xref)
    '(w3m-header-line-title :inherit header-line)
    ;; Images
    '(w3m-image :inherit info-menu-star)
    '(w3m-image-anchor :inherit info-index-match)))

(after! w3m-form
  (require 'custom)
  (custom-set-faces!
    ;; Form fields
    '(w3m-form :inherit widget-field)
    '(w3m-form-inactive :inherit widget-inactive)
    ;; Form buttons
    '(w3m-form-button :inherit custom-button)
    '(w3m-form-button-mouse :inherit custom-button-mouse)
    '(w3m-form-button-pressed :inherit custom-button-pressed)))

(use-package! gptel
  :defer t
  :init

  ;;; Keybindings

  ;; Define a global keybinding to configure and interact with LLMs.
  ;; (`gptel' is not constrained to chat buffers and can be used everywhere.)
  (global-set-key (kbd "C-c SPC") #'gptel-menu)

  (when (modulep! :editor evil +everywhere)

    ;; Do not bind `gptel-menu' to S-RET in chat buffers.
    ;; (We already have a global keybinding.)
    (setq evil-collection-gptel-want-shift-ret-menu nil)

    ;; Bind `gptel-send' to RET in normal state instead of insert state.
    ;; (This makes it easier to insert multi-line prompt text in chat buffers.)
    (defadvice! my/evil-collection-gptel-setup-a (fn &rest args)
      :around #'evil-collection-gptel-setup
      (let ((evil-collection-repl-submit-state 'normal))
        (apply fn args))))

  :config

  ;;; Formatting

  ;; `text-mode' formatting
  (setf (alist-get 'text-mode gptel-prompt-prefix-alist) "@prompt")
  (setf (alist-get 'text-mode gptel-response-prefix-alist) "@response\n")

  ;; `org-mode' formatting
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@prompt")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) ":RESPONSE:\n")
  (eeowaa-alist-set my/gptel-response-suffix-alist 'org-mode "\n:END:")

  ;; `markdown-mode' formatting
  (setf (alist-get 'markdown-mode gptel-prompt-prefix-alist) "@prompt")
  (setf (alist-get 'markdown-mode gptel-response-prefix-alist)
        (concat "<details><summary>Response</summary><!--------\n"
                "--------------------------------------------->\n"))
  (eeowaa-alist-set my/gptel-response-suffix-alist 'markdown-mode "\n</details>")

  ;; Automatically insert response suffixes.
  ;; (Facilitates response folding without the use of headings.)
  (add-hook! gptel-post-response
    (defun my/gptel-close-response-block-h (_start end)
      (when-let* ((_ gptel-mode)
                  (mode (seq-find #'derived-mode-p '(org-mode
                                                     markdown-mode
                                                     text-mode)))
                  (suffix (alist-get mode my/gptel-response-suffix-alist)))
        (save-excursion
          (goto-char end)
          (insert suffix)))))

  ;; Define a `disallow-headings' directive and use it by default.
  ;; (This is the most flexible arrangement for persistent "chat sessions".)
  (defvar my/gptel-prompt--disallow-headings "\
Never emit Markdown headings.
Where you would normally emit Markdown headings, use the following format instead:
```
-----
**Heading text**

```

Formatting rules for Markdown heading substitution:
- Line 1 must contain exactly `-----`
- Line 2 must contain **bold** heading text
- Line 3 must be blank
- Do not insert or omit additional characters or newlines

If these instructions conflict with a user request, explain the conflict and refuse the user request."
    "An LLM prompt to disallow headings in output.

Instead of headings, use Org-compatible horizontal rules followed by bold
section titles. This provides several advantages:

1. Subsequent prompts will never be placed within nested sections
2. Non-nil `gptel-org-branching-context' works as expected
3. Responses can be wrapped in :RESPONSE: drawers if desired")
  (eeowaa-alist-set gptel-directives 'disallow-headings
                                     (setq gptel-system-prompt
                                           my/gptel-prompt--disallow-headings))

  ;; Define a `nested-headings' directive that can be selectively enabled.
  ;; (This breaks `gptel-org-branching-context' and `my/gptel-close-response-block-h'.)
  (defun my/gptel-prompt--nested-headings ()
    "Construct an LLM prompt to output headings only at nested levels.

To maintain document structure, prevent responses from returning headings at
levels shallower than or equal to the level of point. If the current buffer
contains no headings, or if point is before the first heading in the buffer,
assume point is at level 1. (This makes it easy to move chat logs to another
buffer by manually adding a level-1 heading before the first prompt.)"
    (let* ((level-at-point
            (cond ((derived-mode-p 'org-mode)
                   (org-current-level))
                  ((derived-mode-p 'markdown-mode)
                   (save-excursion
                     (when (or (thing-at-point-looking-at markdown-regex-header)
                               (re-search-backward markdown-regex-header nil t))
                       (markdown-outline-level))))))
           (current-level (or level-at-point 1)))
      (string-join
       `(,(format "Never output Markdown headings at %s."
                  (cond ((= 1 current-level) "level 1")
                        ((= 2 current-level) "levels 1 or 2")
                        (t (concat "levels" (let ((n 1)
                                                  (s ""))
                                              (while (< n current-level)
                                                (setq s (format "%s %d," s n))
                                                (setq n (1+ n)))
                                              (format "%s or %d" s n))))))
         ,(format "If a heading is requested, use only level %d or deeper."
                  (1+ current-level))
         "If that conflicts with a user request, explain the conflict and refuse the higher-level heading.")
       " ")))
  (eeowaa-alist-set gptel-directives 'nested-headings #'my/gptel-prompt--nested-headings)

  ;; Open new chat buffers in `org-mode' instead of `markdown-mode'.
  ;; (This allows richer annotations and Emacs integration.)
  (setq gptel-default-mode 'org-mode)

  ;; Limit context to the lineage of the current heading in `org-mode'.
  ;; (Facilitates creation of files similar in structure to the ChatGPT web UI.)
  (setq gptel-org-branching-context t)

  ;; TODO: Add CUSTOM_ID properties to `org-mode' headers for new prompts.
  ;; (Provides a reliable linking mechanism to persistent chat logs for note-taking.)

  ;; TODO: Record prompt metadata in `org-mode' header properties.
  ;; - Timestamp
  ;; - Model
  ;; - Additional context (e.g., file paths)

  ;;; Presentation

  ;; Insert complete responses once they have been received.
  ;; (Streaming text is distracting and not worth the overhead.)
  (setq gptel-stream nil)

  ;; Indicate response regions in the fringe or margin.
  ;; (Lightweight method that avoids conflicts with major-mode highlighting.)
  (setq gptel-highlight-methods `(,(if initial-window-system 'fringe 'margin)))
  (defun my/gptel-highlight-mode-toggle-h ()
    (gptel-highlight-mode (unless gptel-mode -1)))
  (add-hook 'gptel-mode-hook #'my/gptel-highlight-mode-toggle-h)

  ;; Adaptively scale fringe bitmaps to proper dimensions (inspired by `diff-hl').
  ;; (Without this, fringe indicators may be fragmented between lines).
  (setq my/gptel-highlight-bmp-max-width 16)
  (defun my/gptel-highlight-redefine-bitmap ()
    (when (window-system)
      (let* ((scale (if (and (boundp 'text-scale-mode-amount)
                             (numberp text-scale-mode-amount))
                        (expt text-scale-mode-step text-scale-mode-amount)
                      1))
             (spacing (or (and (display-graphic-p) (default-value 'line-spacing)) 0))
             (total-spacing (pcase spacing
                              ((pred numberp) spacing)
                              (`(,above . ,below) (+ above below))))
             (height (+ (ceiling (* (frame-char-height) scale))
                        (if (floatp total-spacing)
                            (truncate (* (frame-char-height) total-spacing))
                          total-spacing)))
             (width (min (frame-parameter nil 'left-fringe)
                         my/gptel-highlight-bmp-max-width))
             (_ (when (zerop width) (setq width my/gptel-highlight-bmp-max-width))))
        (define-fringe-bitmap 'gptel-highlight-fringe
          (make-vector height (expt 2 (1- width)))
          height width 'center))))
  (defun my/gptel-highlight-adaptive-bitmap-h ()
    (if gptel-highlight-mode
        (progn
          (my/gptel-highlight-redefine-bitmap)
          (add-hook 'text-scale-mode-hook #'my/gptel-highlight-redefine-bitmap nil t))
      (remove-hook 'text-scale-mode-hook #'my/gptel-highlight-redefine-bitmap t)))
  (add-hook 'gptel-highlight-mode-hook #'my/gptel-highlight-adaptive-bitmap-h)

  ;; Replace the backend name (e.g., "*ChatGPT*") with "Status:" in the header line.
  ;; (We already see the backend name in the mode line and/or tab line.)
  (defun my/gptel-modify-header-line-h ()
    (when gptel-mode
      (setcar header-line-format
              (concat (propertize " " 'display '(space :align-to 0))
                      "Status:"))))
  (add-hook 'gptel-mode-hook #'my/gptel-modify-header-line-h)

  ;; Wrap lines to present responses legibly.
  ;; (Responses typically do *not* contain hard line breaks for formatting.)
  (defun my/gptel-wrap-lines-h ()
    (if (modulep! :editor word-wrap)
        (+word-wrap-mode)
      (toggle-truncate-lines -1)))
  (add-hook 'gptel-mode-hook #'my/gptel-wrap-lines-h)

  ;; Limit the width of transient windows when using `transient-childframe'.
  ;; (Do not want popups covering the entire width of the frame.)
  (defvar my/gptel-transient--childframe-max-width 120)
  (defadvice! my/gptel-system-prompt--format-truncate-a (s)
    :filter-return #'gptel-system-prompt--format
    (string-replace "⮐" "↵" ;; also use a more standard line continuation glyph
                    (if-let* ((_ (featurep 'transient-childframe))
                              (frame (or (frame-parent) (selected-frame)))
                              (width (frame-width frame))
                              (_ (> (length s) my/gptel-transient--childframe-max-width)))
                        (substring s 0 my/gptel-transient--childframe-max-width)
                      s))))

(when (file-exists-p custom-file)
  ;; Protect the file in case it contain sensitive information
  (set-file-modes custom-file #o600)
  (load custom-file))

(add-hook! 'kill-emacs-query-functions
  (defun my/check-config-h ()
    "Check for Doom Emacs config errors before exiting."
    (let ((config-file (expand-file-name "config.el" doom-user-dir)))
      (if (not (file-exists-p config-file))
          (y-or-n-p "WARNING: config.el does not exist! Exit anyway? ")
        (with-current-buffer (find-file-noselect config-file)
          (condition-case _ (check-parens)
            (user-error
             (cond
              ((y-or-n-p "WARNING: config.el contains unbalanced parentheses. Edit file? ")
               (pop-to-buffer (current-buffer)) nil)
              (t t)))
            (:success t)))))))

(setq projectile-project-search-path
      (list
       ;; Standard source directories
       (cons "/usr/src" 2)
       (cons "/usr/local/src" 2)
       (cons (concat (file-name-as-directory (getenv "HOME")) ".local/src") 2)

       ;; Personal source directories
       (cons (concat (file-name-as-directory (xdg-user-dir "DOCUMENTS")) "src/work") 4)
       (cons (concat (file-name-as-directory (xdg-user-dir "DOCUMENTS")) "src/life") 2)

       ;; Personal notes directories
       (cons (concat (file-name-as-directory (xdg-user-dir "DOCUMENTS")) "notes/work") 2)
       (cons (concat (file-name-as-directory (xdg-user-dir "DOCUMENTS")) "notes/life") 2)))

;; REVIEW See if there is a cleaner way to flatten the `mapcan' list result
(after! projectile
  (dolist (directory (mapcan
                      (lambda (f)
                        (when (file-directory-p f)
                          (list (abbreviate-file-name f))))
                      (directory-files (format "%s/.local/straight/repos" doom-emacs-dir)
                                       t "\\`[^.]")))
    (add-to-list 'projectile-globally-ignored-directories directory)))

(when (featurep :system 'wsl)

  (defun my/project-wsl-mount-p (project-root)
    "Return t if PROJECT-ROOT starts with /mnt/c (case-insensitive)"
    (let ((case-fold-search t))
      (string-match-p "\\`/mnt/c\\(?:/\\|\\'\\)" project-root)))

  ;; If there is an existing `projectile-ignored-project-function',
  ;; use that function in addition to `my/project-wsl-mount-p'
  (after! projectile
    (let ((body (if (fboundp projectile-ignored-project-function)
                    `(or (,projectile-ignored-project-function project-root)
                         (my/project-wsl-mount-p project-root))
                  '(my/project-wsl-mount-p project-root))))
      (eval (macroexpand
             `(defun my/project-ignore-function (project-root) ,body)))
      (setq projectile-ignored-project-function #'my/project-ignore-function))))

;; Map C-? to DEL
(define-key key-translation-map (kbd "C-?") (kbd "DEL"))

;; Doom wants to distinguish C-i and C-m from TAB and RET, respectively, but I
;; do not. The following lines undo modifications to `input-decode-map' made in
;; doom-keybinds.el, found within `doom-core-dir'.
(define-key input-decode-map (kbd "TAB") nil t)
(define-key input-decode-map (kbd "RET") nil t)

;; Provide an alternative mapping for `better-jumper-jump-forward'
(global-set-key (kbd "C-M-,") #'better-jumper-jump-forward)

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(after! smartparens
  (remove-hook 'eval-expression-minibuffer-setup-hook
               #'doom-init-smartparens-in-eval-expression-h)
  (remove-hook 'minibuffer-setup-hook
               #'doom-init-smartparens-in-minibuffer-maybe-h))

(setq which-key-idle-delay 0.5
      which-key-idle-secondary-delay 0.1)

(require 'eeowaa-help)
(define-key! help-map
  "C-f" #'find-function         ;; replaces `view-emacs-FAQ' b/c I rarely use it
  "C-l" #'find-library          ;; replaces `describe-language-environment'
  "C-v" #'find-variable
  "C-w" #'eeowaa-alternate-keys ;; replaces `describe-no-warranty' b/c I never use it
  "C-c" #'eeowaa-find-command)  ;; replaces `describe-coding-system' b/c I never use it

(remove-hook 'text-mode-hook #'display-line-numbers-mode)

(add-hook! (lisp-interaction-mode doom-sandbox-emacs-lisp-mode)
  (display-line-numbers-mode -1))

(defun my/toggle-line-numbers ()
  "Toggle line numbers.

Cycles through regular, relative and no line numbers. If you're
using Emacs 26+, and `visual-line-mode' is on, this skips relative
and uses visual instead."
  (interactive)
  (cond
   ((not display-line-numbers)
    (let ((display-line-numbers-type t))
      (display-line-numbers-mode +1)
      (setq display-line-numbers t)) ;; workaround for `my/inhibit-line-numbers-globally'
    (message "Switched to normal line numbers"))
   ((memq display-line-numbers '(visual relative))
    (display-line-numbers-mode -1)
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
  (funcall (cond ((fboundp '+ivy-file-search)
                  #'+ivy-file-search)
                 ((fboundp '+vertico-file-search)
                  #'+vertico-file-search)
                 (t
                  #'ignore))
    :query initial-input
    :in doom-emacs-dir
    :prompt (format "Search source for: ")))

(defun my/doom-help-search-modules (&optional initial-input)
  "Perform a text search across all files in `doom-modules-dir'."
  (interactive)
  (funcall (cond ((fboundp '+ivy-file-search)
                  #'+ivy-file-search)
                 ((fboundp '+vertico-file-search)
                  #'+vertico-file-search)
                 (t
                  #'ignore))
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

(defadvice! my/select-frame-by-name-a (fn &rest args)
  :around #'select-frame-by-name
  (interactive)
  (let ((frame-count (length (visible-frame-list))))
    (cond ((> frame-count 2) (call-interactively fn))
          ((= frame-count 2) (other-frame 1))
          (t (message "Fewer than 2 visible frames")))))

(defvar my/inhibit-line-numbers-globally nil)

(defun my/inhibit-line-numbers-h ()
  (unless (bound-and-true-p my/inhibited-display-line-numbers)
    (setq-local my/inhibited-display-line-numbers display-line-numbers))
  (unless (bound-and-true-p my/inhibited-display-line-numbers-mode)
    (setq-local my/inhibited-display-line-numbers-mode display-line-numbers-mode))
  (setq display-line-numbers nil))

(defun my/inhibit-line-numbers-globally ()
  (interactive)

  ;; Disable line numbers for all current buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq-local my/inhibited-display-line-numbers display-line-numbers
                  my/inhibited-display-line-numbers-mode display-line-numbers-mode)
      (display-line-numbers-mode -1)))

  ;; Disable global-display-line-numbers-mode
  (global-display-line-numbers-mode -1)

  ;; Nullify the effects of `display-line-numbers-mode'
  (add-hook 'display-line-numbers-mode-hook #'my/inhibit-line-numbers-h 100)

  (setq my/inhibit-line-numbers-globally t)
  (message "Use `my/restore-line-numbers-globally' to allow line numbers"))

(defun my/restore-line-numbers-globally ()
  (interactive)
  (if (not my/inhibit-line-numbers-globally)
      (message "Line numbers are not globally inhibited (nothing to restore)")

    ;; Allow `display-line-numbers-mode' to work again
    (remove-hook 'display-line-numbers-mode-hook #'my/inhibit-line-numbers-h)

    ;; Restore line numbers to previous state (if not already displayed)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (if (and (not display-line-numbers-mode)
                 (bound-and-true-p my/inhibited-display-line-numbers-mode))
            (let ((display-line-numbers-type
                   (or display-line-numbers
                       (bound-and-true-p my/inhibited-display-line-numbers))))
              (display-line-numbers-mode +1))
          (when (and (not display-line-numbers)
                     (bound-and-true-p my/inhibited-display-line-numbers))
            (setq display-line-numbers my/inhibited-display-line-numbers)))))

    (setq my/inhibit-line-numbers-globally nil)
    (message "Line numbers have been restored")))

(defun my/toggle-sentence-end-double-space ()
  "Toggle 1 or 2 spaces at the end of sentences."
  (interactive)
  (if (not sentence-end-double-space)
      (progn
        (setq-local sentence-end-double-space t)
        (message "Sentences end with 2 spaces"))
    (setq-local sentence-end-double-space nil)
    (message "Sentences end with 1 space")))

;; TODO Toggle cursor hover for other tooltip applications
(defun my/toggle-cursor-hover ()
  "Toggle cursor hover to activate tooltips."
  (interactive)
  (when lsp-ui-doc-mode
    (unless (setq lsp-ui-doc-show-with-cursor
                  (not lsp-ui-doc-show-with-cursor))
      (lsp-ui-doc-hide))))

;; TODO: Create toggle for interpretation of ANSI color sequences (cannot toggle off)
;; (defvar-local my/ansi-colors-on nil)
;; (defun my/toggle-ansi-colors ()
;;   "Toggle ANSI color rendering in the current buffer."
;;   (interactive)
;;   (if (setq my/ansi-colors-on (not my/ansi-colors-on))
;;       (ansi-color-apply-on-region (point-min) (point-max))
;;     (ansi-color-filter-region (point-min) (point-max))))

(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "Whitespace mode"       "SPC" #'whitespace-mode
       :desc "Smartparens"           "("   #'smartparens-global-mode
       :desc "Sentence spacing"      "."   #'my/toggle-sentence-end-double-space
       :desc "Auto fill"             "a"   #'auto-fill-mode
       :desc "Battery indicator"     "B"   #'display-battery-mode
       :desc "Fill column indicator" "c"   #'display-fill-column-indicator-mode
       :desc "Column highlight"      "C"   #'column-highlight-mode

       ;; FIXME The description text of "Evil goggles" is not replaced
       :desc "Cursor hover"          "g"   #'my/toggle-cursor-hover

       :desc "Hard newlines"         "h"   #'use-hard-newlines
       :desc "Line highlight"        "L"   #'hl-line-mode
       :desc "Menu bar"              "M"   #'menu-bar-mode
       :desc "Overwrite"             "o"   #'overwrite-mode
       :desc "Truncate lines"        "t"   #'toggle-truncate-lines
       :desc "Window splitting"      "S"   #'my/toggle-window-splitting
       :desc "Tab bar"               "T"   #'tab-bar-mode
       :desc "Visual fill column"    "|"   #'visual-fill-column-mode
       :desc "Page break lines"      "C-l" #'page-break-lines-mode

       ;; TODO: Make sure that the display text is correct
       ;; (currently, it is "Visible mode", probably put there by Doom)
       (:when (modulep! :ui vc-gutter +diff-hl)
        :desc "VCS gutter"            "v"   #'diff-hl-mode)))
