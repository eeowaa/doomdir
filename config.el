(add-load-path! (concat doom-user-dir "lisp"))

(defmacro my/doom-use-face (face other-face)
  "Force FACE to be the same as OTHER-FACE.
Examples:

  (my/doom-use-face tab-bar-tab +workspace-tab-selected-face)
  (my/doom-use-face tab-bar-tab-inactive +workspace-tab-face)"
  `(custom-set-faces!
     '(,face
       ,@(mapcan (lambda (key) (list key nil))
                 (doom-plist-keys (face-attr-construct face)))
       :inherit ,other-face)))

(unless initial-window-system
  (remove-hook 'company-mode 'company-box-mode))

(after! company-box
  (cl-pushnew 'company-echo-metadata-frontend company-frontends)
  (add-hook! company-box-mode
    (defun my/company-box-toggles-h ()
      (if company-box-mode
          (delq! 'company-echo-metadata-frontend company-frontends)
        (cl-pushnew 'company-echo-metadata-frontend company-frontends)))))

(setq company-idle-delay nil)

(when (modulep! company +childframe)
  (after! company
    (add-hook! 'evil-normal-state-entry-hook
      (defun +company-abort-h ()
        (when company-candidates
          (company-abort))))))

(after! company-box
  (add-to-list 'company-box-frame-parameters '(tab-bar-lines-keep-state . nil)))

(setq company-box-doc-delay 0.05) ;; This feels a bit smoother than no delay

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

(setq extended-command-suggest-shorter nil)

;; Author: tecosaur
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (and (buffer-modified-p) (vimish-tab-force-updating-p))
                         " ◉ %s"
                       "  ●  %s") project-name))))))

(setf (alist-get 'fullscreen initial-frame-alist) 'maximized)

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

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
  :around 'hide-mode-line-mode
  #'always)

(after! doom-themes-ext-treemacs
  (defadvice! my/show-treemacs-modeline-a (&rest _)
    "Show the treemacs modeline"
    :around 'doom-themes-hide-modeline
    #'always))

(remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)

(defadvice! my/tab-bar-theme-a (theme &rest _)
  "Tweak the style of the tab bar."
  :after '(load-theme consult-theme)
  (if (string-match-p "\\`ef-" (symbol-name theme))
      (ef-themes-with-colors
        (custom-set-faces
         `(tab-bar ((,c :background ,bg-inactive :foreground ,fg-intense)))))
    (my/doom-use-face tab-bar mode-line-inactive)))

(setq doom-theme
      (if initial-window-system
          'ef-bio
        'ef-tritanopia-dark))

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
                  :default-size 22)
                 ("Source Code Pro"
                  :default-size 22)
                 ("Terminus"
                  :default-size 30)
                 ("Comic Mono"
                  :variable-pitch "Comic Neue"
                  :default-size 24)))

;; Helper function to read a positive integer from the minibuffer
(defun my/read-positive-int (&optional prompt default)
  "Read a positive integer from the minibuffer.
PROMPT defaults to \"Positive integer: \""
  (let ((number (read-number (or prompt "Positive integer: ") default)))
    (if (and (integerp number) (> number 0))
        number
      (message "Please enter a positive integer.")
      (sit-for 1)
      (my/read-positive-int prompt))))

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
                     (my/read-positive-int "Font size: " default-size)
                   default-size)))
    (setq doom-font                (funcall f font size)
          doom-variable-pitch-font (funcall f variable-pitch-font size)
          doom-serif-font          (funcall f serif-font size)))
  (and (interactive-p) (doom/reload-font)))

;; Set the font
(my/select-font "Iosevka Comfy Fixed")

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

(setq imenu-list-mode-line-format "  Ilist"
      imenu-list-position 'right
      imenu-list-size 35) ;; same as treemacs

(setq +ligatures-in-modes '(org-mode)
      +ligatures-extras-in-modes '(org-mode))

(setq column-number-indicator-zero-based nil)

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
       (:map magit-log-mode-map
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
  "gF" #'+tabs:new-ffap-with-line-window-tab)

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

(setq display-time-day-and-date t ;; show the DoW and date in addition to the time
      display-time-load-average-threshold 0) ;; always display the load average

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
  (cl-pushnew 'treemacs-mode vimish-tab-exclude-modes))

(setq doom-themes-treemacs-enable-variable-pitch nil)

(setq +treemacs-git-mode 'extended)

(require 'ace-window)

(after! treemacs
  (setq treemacs-collapse-dirs 0))

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

(after! (:and treemacs ace-window)
  (setq aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers)))

(setq treemacs-show-cursor t)

;; REVIEW Consider detecting troublesome icons and automatically falling back to
;; the default icon for text files.
(defvar my/treemacs-icon-extension-alist
  '(("org" . ("org_archive"))
    ("sh" . ("bat"))
    ("txt" . ("Pipfile"))
    ("json" . ("Pipfile.lock"))) ;; "configuration" icon
  "Alist of file extension mappings for Treemacs icons.

The `car' of each element is a file extension with a desirable
Treemacs icon; the `cdr' is a list of file extensions that should
use that same icon.

If the `car' is a string, it is treated case-insensitively, as
Treemacs defines string extensions as lowercase. See the
`treemacs-icons' package for more info; for a quick reference,
see how `treemacs-create-theme' is used to define the \"Default\"
Treemacs theme.")

(defvar my/treemacs-fallback-icon-alist
  '((dir-closed . " +\t")
    (dir-open . " -\t")
    (root-closed . " +\t")
    (root-open . " -\t"))
  "Alist of file extension mappings for Treemacs TUI icons.

The `car' of each element is a file extension or a symbol
representing a special Treemacs entry (see documentation for the
EXTENSIONS argument of `treemacs-create-icon' for more info).

The `cdr' of each element is a string to use in place of an icon
in TTY Emacs (or whenever Treemacs cannot render icons).")

(defvar my/treemacs-fallback-icon-default "  \t"
  "Default fallback string for TUI icons.

This string is used as the TUI icon for all Treemacs entries not
matched in `my/treemacs-icon-fallback-alist'.")

(defun my/treemacs-fallback-icon (extension)
  "Return the fallback icon corresponding to EXTENSION."
  (when (stringp extension)
    (setq extension (downcase extension)))
  (or (alist-get extension my/treemacs-fallback-icon-alist)
      my/treemacs-fallback-icon-default))

(after! treemacs
  (defun my/treemacs-modify-icons (&optional theme)
    "Modify the icons for the Treemacs THEME (default current).
The following variables are consulted when modifying the theme:

  `my/treemacs-icon-extension-alist'
  `my/treemacs-fallback-icon-alist'
  `my/treemacs-fallback-icon-default'

Call this function after `treemacs-create-theme' is called to
ensure your customizations take hold."
    (unless (treemacs-theme-p theme)
      (setq theme (if (stringp theme)
                      (treemacs--find-theme theme)
                    treemacs--current-theme)))
    (let ((gui-icons (treemacs-theme->gui-icons theme))
          (tui-icons (treemacs-theme->tui-icons theme)))

      ;; Modify GUI icons
      (dolist (entry my/treemacs-icon-extension-alist)
        (let* ((key (car entry))
               (gui-icon (treemacs-get-icon-value (if (stringp key) (downcase key) key)))
               (extension-list (cdr entry)))
          (dolist (ext extension-list)
            (ht-set! gui-icons (if (stringp ext) (downcase ext) ext) gui-icon))))

      ;; Modify TUI icons
      (treemacs--maphash tui-icons (extension _)
        (ht-set! tui-icons extension (my/treemacs-fallback-icon extension)))))

  ;; FIXME: This advice either causes errors or fails to run
  (undefadvice! my/treemacs-modify-icons-a (theme &rest _)
    :after #'treemacs-create-theme
    :before #'treemacs-load-theme
    (my/treemacs-modify-icons theme))

  ;; FIXME: This doesn't work, either (this is easy to see when
  ;; `doom-themes-treemacs-theme' is set to `doom-color', or when running in TTY
  ;; Emacs). The only workaround I've found is to run `my/treemacs-modify-icons'
  ;; manually after opening Treemacs, closing the Treemacs window via
  ;; `treemacs-kill-buffer', and then opening Treemacs again.
  (add-hook! 'doom-load-theme-hook :append #'my/treemacs-modify-icons)

  ;; FIXME This function does not work when the Treemacs window is selected.
  ;; Also, the Treemacs window is always selected after this function runs, and
  ;; I'd rather keep the current window selected.
  (defun my/treemacs-select-theme ()
    "Select and load a new Treemacs theme.
Closes and re-opens Treemacs to apply the new theme."
    (interactive)
    (call-interactively #'treemacs-load-theme)
    (unless (eq (treemacs-current-visibility) 'none)
      (treemacs-select-window)
      (treemacs-kill-buffer)
      (treemacs)))

  (defun my/treemacs-current-theme ()
    "Return the name of the current Treemacs theme."
    (treemacs-theme->name treemacs--current-theme)))

(after! diff-hl
  (unless (window-system) (diff-hl-margin-mode)))

(after! ace-window
  (when initial-window-system
    (setq aw-scope 'visible)))

(setq aw-background nil)

(map! :leader
      (:when (modulep! :ui workspaces)
       (:prefix-map ("TAB" . "workspace")
        :desc "Move left"  "H" #'+workspace/swap-left
        :desc "Move right" "L" #'+workspace/swap-right)))

(after! projectile
  (define-key! projectile-mode-map
    "C-c p" #'projectile-command-map))

(after! writeroom-mode

  (defvar my/zen--old-display-line-numbers nil)
  (defvar my/zen--old-hl-line-mode nil)
  (defvar my/zen--old-column-highlight-mode nil)
  (defvar my/zen--old-display-fill-column-indicator-mode nil)
  (defvar my/zen--old-tab-bar-mode nil)
  (defvar my/zen--old-tab-line-mode nil)
  (defvar my/zen--old-vi-tilde-fringe-mode nil)

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
                  (and (modulep! :ui vi-tilde-fringe) (bound-and-true-p vi-tilde-fringe-mode)))

            ;; Remove distractions
            (setq display-line-numbers nil)
            (hl-line-mode -1)
            (column-highlight-mode -1)
            (display-fill-column-indicator-mode -1)
            (tab-bar-mode -1)
            (tab-line-mode -1)
            (vi-tilde-fringe-mode -1))

        ;; Restore previous state
        (setq display-line-numbers my/zen--old-display-line-numbers)
        (if my/zen--old-hl-line-mode (hl-line-mode +1))
        (if my/zen--old-column-highlight-mode (column-highlight-mode +1))
        (if my/zen--old-display-fill-column-indicator-mode (display-fill-column-indicator-mode +1))
        (if my/zen--old-tab-bar-mode (tab-bar-mode +1))
        (if my/zen--old-tab-line-mode (tab-line-mode +1))
        (if my/zen--old-vi-tilde-fringe-mode (vi-tilde-fringe-mode +1))))))

;; Display ^L characters as horizontal lines
(use-package! page-break-lines
  :config (global-page-break-lines-mode))

;; When using vanilla bindings, have C-l send the current line to the top of the
;; window (like most shells do)
(setq recenter-positions '(top bottom middle))

;; When using `evil', have C-l redraw the display (like Vim does). Use zt, zz,
;; and zb to reposition the current line instead of C-l.
(when (modulep! :editor evil)
  (defun my/redraw ()
    (interactive)
    (redraw-display)
    (set-window-buffer nil (current-buffer))
    (when (fboundp 'vimish-tab-force-tab-line-update)
      (vimish-tab-force-tab-line-update)))
  (global-set-key (kbd "C-l") #'my/redraw))

;; Perform a line feed after jumping to a ^L character
(defadvice! my/recenter-top-a (&rest _)
  "Perform a line feed after jumping to a ^L character"
  :after #'forward-page
  (recenter 0))

(setq default-input-method "latin-postfix")

(setq-default truncate-lines t)

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

(setq evil-auto-balance-windows nil)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

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

(defadvice! my/ergonomic-info-bindings-a (&rest _)
  :after #'evil-collection-info-setup
  (evil-collection-define-key 'normal 'Info-mode-map
    (kbd "C-i") 'Info-next-reference
    (kbd "M-,") 'Info-history-back
    (kbd "C-M-,") 'Info-history-forward))

(pushnew! evil-emacs-state-modes 'noaa-mode 'vterm-mode)

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

(setq disabled-command-function nil)

(when (featurep 'native-compile)
  (setq native-comp-speed 2
        package-native-compile t))

(let ((srcdir (format "%s.local/src/emacs/emacs-%s/src"
                      (file-name-as-directory (getenv "HOME"))
                      emacs-version)))
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

(defun my/zoomwin-toggle ()
  "Zoom or unzoom the selected window.
If the current frame has multiple windows, delete other windows.
If the current frame has one window, restore the previous windows."
  (interactive)
  (if (= 1 (length
            (-remove (lambda (w) (window-parameter w 'no-other-window))
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
                  (-select
                   (lambda (dir)
                     (string-match-p dir (expand-file-name default-directory)))
                   (list (file-name-as-directory (xdg-user-dir "DOCUMENTS"))
                         doom-user-dir))
                t)))))))

(unless (member "IMAGEMAGICK" (split-string system-configuration-features))
  (setq imagemagick-types-inhibit t))

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

(add-hook! ibuffer-mode
  (defun my/ibuffer-mode-line-h ()
    "Clean up the modeline and improve performance."
    (setcar mode-line-process "by ")
    (setf (nth 2 ibuffer-header-line-format) " by ")))
    ;; (setf (alist-get 'header-line-format mode-line-process) "")))

;; This should already be enabled by emacs/undo/config.el
(global-undo-tree-mode)

;; Not sure what the best way is to add this hook
(add-hook 'evil-local-mode-hook #'turn-on-undo-tree-mode)

(after! git-commit
  (delq! 'overlong-summary-line git-commit-style-convention-checks))

(pushnew! auto-mode-alist
          '("/git/config\\.d/.+" . gitconfig-mode)
          '("\\.gitignore\\'" . gitignore-mode))

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

(after! vterm

  ;; This function is to be called in shell configuration to obtain the
  ;; directory in which to read aliases
  (defun my/vterm--write-user-emacs-directory (tmpfile)
    "Write the string evaluation of `user-emacs-directory' to TMPFILE."
    (f-write user-emacs-directory 'utf-8 tmpfile))
  (pushnew! vterm-eval-cmds '("my/vterm--write-user-emacs-directory"
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
          (pushnew! vterm-eval-cmds (list command (intern command)))
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

(after! flycheck
  (defadvice! my/org-src-a (&rest _)
    "Consider Org Src buffers as ephemeral (do not enable flycheck)."
    :after-until #'flycheck-ephemeral-buffer-p
    (string-prefix-p "*Org Src" (buffer-name))))

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

(when (and (modulep! :checkers spell)
           (not (modulep! :checkers spell +flyspell)))
  (remove-hook 'text-mode-hook 'spell-fu-mode))

(require 'spell-fu)

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

(setq eeowaa-project-init-files-alist
      '(;; Git
        (".gitignore")
        (".gitattributes")

        ;; Documentation
        ("README.md")
        ("LICENSE")
        ("todo.org")

        ;; Dev tooling
        (".envrc")
        (".editorconfig")))

(after! dap-mode
  (defun my/refresh-window (&rest arg)
    "Refresh the current window's display margins, fringes, and scroll bars."
    (set-window-buffer nil (current-buffer)))
  (add-hook! '(+dap-running-session-mode-hook
               ;; NOTE Uncomment the following lines to get breakpoints to show
               ;;      most of the time (slows things down a bit):
               ;; dap-breakpoints-changed-hook
               ;; dap-continue-hook
               ;; dap-executed-hook
               ;; dap-loaded-sources-changed-hook
               ;; dap-position-changed-hook
               ;; dap-session-changed-hook
               ;; dap-session-created-hook
               ;; dap-stack-frame-changed-hook
               ;; dap-stopped-hook
               ;; dap-terminated-hook
               )
               :append #'my/refresh-window))

(map! :leader
      ;;; <leader> d --- debug
      (:prefix-map ("d" . "debug")
       :desc "Next"                "n" #'dap-next
       :desc "Step in"             "i" #'dap-step-in
       :desc "Step out"            "o" #'dap-step-out
       :desc "Continue"            "c" #'dap-continue
       :desc "Restart frame"       "r" #'dap-restart-frame
       :desc "Disconnect"          "Q" #'dap-disconnect
       (:prefix ("s" . "switch")
        :desc "Session"             "s" #'dap-switch-session
        :desc "Thread"              "t" #'dap-switch-thread
        :desc "Stack frame"         "f" #'dap-switch-stack-frame
        :desc "Up stack frame"      "u" #'dap-up-stack-frame
        :desc "Down stack frame"    "d" #'dap-down-stack-frame)
       (:prefix ("b" . "breakpoints")
        :desc "Toggle"              "b" #'dap-breakpoint-toggle
        :desc "Delete"              "d" #'dap-breakpoint-add
        :desc "Add"                 "a" #'dap-breakpoint-delete
        :desc "Set condition"       "c" #'dap-breakpoint-condition
        :desc "Set hit count"       "h" #'dap-breakpoint-hit-condition
        :desc "Set log message"     "l" #'dap-breakpoint-log-message)
       (:prefix ("d" . "debug")
        :desc "Debug"               "d" #'dap-debug
        :desc "Debug recent"        "r" #'dap-debug-recent
        :desc "Debug last"          "l" #'dap-debug-last
        :desc "Edit debug template" "e" #'dap-debug-edit-template
        :desc "Debug restart"       "s" #'dap-debug-restart)
       (:prefix ("e" . "eval")
        :desc "Eval"                "e" #'dap-eval
        :desc "Eval region"         "r" #'dap-eval-region
        :desc "Eval thing at point" "s" #'dap-eval-thing-at-point
        :desc "Add expression"      "a" #'dap-ui-expressions-add)
       (:prefix ("w" . "window")
        :desc "Locals"              "l" #'dap-ui-locals
        :desc "Breakpoints"         "b" #'dap-ui-breakpoints
        :desc "Breakpoint List"     "B" #'dap-ui-breakpoints-list
        :desc "Expressions"         "e" #'dap-ui-expressions
        :desc "Sessions"            "s" #'dap-ui-sessions
        :desc "REPL"                "r" #'dap-ui-repl)))

(setq dap-auto-configure-features
      '(sessions       ;; `dap-ui-sessions'
        ;; locals      ;; `dap-ui-locals'
        breakpoints    ;; `dap-ui-breakpoints'
        ;; expressions ;; `dap-ui-expressions'
        repl           ;; `dap-ui-repl'
        ;; controls    ;; FIXME: `dap-ui-controls-mode'
        ;; tooltip     ;; FIXME: `dap-tooltip-mode'
        ))

;; Remove hook installed by Doom
(remove-hook 'dap-ui-mode-hook 'dap-ui-controls-mode)

(setq! docker-image-columns
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

;; Replace default association with a more generic one
(delq! "/Dockerfile\\(?:\\.[^/\\]*\\)?\\'" auto-mode-alist #'assoc-string)
(pushnew! auto-mode-alist
          '("/[^/\\]*\\<\\(Docker\\|Container\\)file\\>[^/\\]*$" . dockerfile-mode))

(setq ein:output-area-inlined-images t)

;; HACK The machinery provided by `ob-ein-languages' and `ob-ein--babelize-lang'
;; is insufficient for bash, so we do it by hand.
(after! ob
  (pushnew! org-babel-load-languages '(ein . t))
  (require 'ob-ein)

  ;; Execute in an anonymous local session by default
  (setq ob-ein-default-header-args:ein
        '((:session . "localhost")))

  ;; This does two things:
  ;; 1. Allows the correct kernel to be selected for execution
  ;; 2. Enables bash-specific features in the source buffer
  (when (not (fboundp 'bash-mode))
    (define-derived-mode bash-mode sh-mode "Bash-script"
      "Major mode for editing bash scripts."
      (sh-set-shell "bash" nil nil)))
  (pushnew! org-src-lang-modes
            '("bash" . bash)
            '("ein-bash" . bash))

  ;; Send output from `ein-bash' source blocks back to org buffer
  (let ((alist (assoc-delete-all :results ob-ein-default-header-args:ein)))
    (setq org-babel-default-header-args:ein-bash
          (push '(:results . "output verbatim") alist)))

  ;; Function to execute `ein-bash' source blocks with
  (defun org-babel-execute:ein-bash (body params)
    (require 'ob-shell nil t)
    ;; hack because ob-ein loads independently of ein
    (custom-set-variables '(python-indent-guess-indent-offset-verbose nil))
    (let ((parser 'org-babel-variable-assignments:bash))
      (ob-ein--execute-body
       (if (fboundp parser)
           (org-babel-expand-body:generic
            body params (funcall (symbol-function parser) params))
         body)
       params))))

(defvar-local my/linked-buffer nil
  "The buffer that `my/send-region' sends text to.")

(defun my/send-region (beg end)
  "Send text in region to a linked buffer.

If the current buffer does not have a linked buffer, or given a
non-nil prefix argument, this function will prompt for a buffer
and set the the linked buffer accordingly. Leading and trailing
whitespace is removed from the region before sending to the
linked buffer.

Contrast this function to `send-region', which sends a region to
a process instead of another buffer. Use `my/send-region' instead
if you want to send region to a REPL or terminal emulator."
  (interactive "r")
  (let* ((region (buffer-substring-no-properties beg end))
         (text (string-trim region))
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
      (pcase major-mode
        ('vterm-mode (vterm-insert text))
        (_ (insert text)))
      (require 'general)
      (funcall (general-simulate-key "RET")))))

(map! :v (kbd "C-c e") #'my/send-region)

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

(when (modulep! :tools lookup +docsets)
  (defun my/ensure-docsets ()
    (dolist (docset dash-docs-docsets)
      (dash-docs-ensure-docset-installed (string-replace " " "_" docset))))
  ;; REVIEW Ensure that `set-docsets!' is only called for modes derived from `prog-mode'
  (add-hook! prog-mode #'my/ensure-docsets))

(after! lsp-ui
  (setq lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-delay 0.5))

(after! lsp-mode
  (when (modulep! :ui popup)
    (setq +popup--display-buffer-alist
          (delq (assoc "^\\*lsp-\\(help\\|install\\)" +popup--display-buffer-alist)
                +popup--display-buffer-alist))
    (set-popup-rule! "^\\*lsp-\\(help\\|install\\)"
      :size #'+popup-shrink-to-fit
      :select nil ;; NOTE I changed this from Doom's default of `t'
      :quit t)))

(setq lsp-modeline-code-actions-segments nil)

(map! :leader
      (:prefix-map ("c" . "code")
       :desc "Glance documentation" "g" #'lsp-ui-doc-glance
       :desc "UI sideline symbols"  "u" #'lsp-ui-sideline-toggle-symbols-info))

(after! treemacs
  (setq lsp-treemacs-symbols-position-params
        `((side . right)
          (slot . 0)
          (window-width . ,treemacs-width))))

;; TODO Convert to `defcustom' with a setter
(defvar my/lsp-ui-delay 0.5
  "Number of seconds before refreshing LSP UI elements.
This variable should be set by `my/lsp-ui-set-delay'.")

(defvar my/lsp-ui-delay-vars
  '(eldoc-idle-delay
    lsp-ui-sideline-delay
    lsp-ui-doc-delay)
  "Variables holding delay durations for LSP UI elements.")

(defun my/lsp-ui-set-delay (delay)
  "Set the delay for LSP UI elements."
  (interactive "nDelay in seconds: ")
  (setq my/lsp-ui-delay delay)
  (dolist (var my/lsp-ui-delay-vars)
    (set var my/lsp-ui-delay)))

(my/lsp-ui-set-delay my/lsp-ui-delay)

;; Removed `doom-themes-hide-fringes-maybe', `doom-themes-hide-modeline', and
;; `doom-themes-define-treemacs-fringe-indicator-bitmap'
(setq treemacs-mode-hook '(doom-themes-setup-line-spacing doom-themes-setup-tab-width))

(define-key! doom-leader-toggle-map
  "i" #'lsp-ui-imenu)

(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`%s t i\\'" prefix-re)) nil . "LSP Imenu")
                which-key-replacement-alist)))

(setq magit-repository-directories
      '(("~/Documents/src" . 2)
        ("~/Documents/ref" . 1)))

(map! (:after evil-collection-magit
       (:map magit-status-mode-map
        :nv "gz" #'magit-jump-to-stashes)))

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
    (letf! (defadvice my/completing-read-lenient-a (args)
             :filter-args #'completing-read
             (setf (nth 3 args) nil)
             args)
      (apply fn args))))

(map! :leader
      (:prefix-map ("c" . "code")
       :desc "Make target"      "m" #'+make/run
       :desc "Make last target" "M" #'+make/run-last))

;; <https://emacs-lsp.github.io/lsp-mode/page/lsp-terraform-ls/>
(when (modulep! :tools terraform +lsp)
  (setq
    ;; Use the official Hashicorp language server from Hashicorp
    lsp-disabled-clients '(tfls)

    ;; Enable reference counts
    lsp-terraform-ls-enable-show-reference t)

  ;; Set keybindings for LSP (reference existing LSP configurations)
  (after! terraform-mode
    (map! :map terraform-mode-map
          :localleader
          (:prefix ("l" . "LSP")
           :desc "terraform init" "i" #'lsp-terraform-ls-init
           :desc "terraform validate" "v" #'lsp-terraform-ls-validate
           :desc "Providers widget" "p" #'lsp-terraform-ls-providers
           :desc "Module calls widget" "m" #'lsp-terraform-ls-module-calls))))

(add-hook 'terraform-mode-local-vars-hook #'tree-sitter! 'append)

;; Missing from evil-textobj-tree-sitter.el:
(after! evil-textobj-tree-sitter
  (pushnew! evil-textobj-tree-sitter-major-mode-language-alist '(terraform-mode . "hcl")))

(defadvice! my/fold--ts-fold-p-a ()
  "Check for tree-sitter-based folding in the current major mode."
  :after-while #'+fold--ts-fold-p
  (assq major-mode ts-fold-range-alist))

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
  :when (modulep! +dotnet)
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

(after! ws-butler
  (pushnew! ws-butler-global-exempt-modes 'tsv-mode))

(require 'eeowaa-project)
;; (require 'eeowaa-debug) NOTE: This is providing nothing of value right now

(defadvice! my/format-result-a (f &rest r)
  "Prepend \";; =>\"."
  :around #'eval-print-last-sexp
  (let ((p (point)))
    (apply f r)
    (save-excursion
      (goto-char p)
      (forward-line 1)
      (insert ";; => "))))

(pushnew! auto-mode-alist '("Cask\\'" . lisp-data-mode))

(after! lsp-haskell
  (setq lsp-haskell-formatting-provider "brittany"))

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

(pushnew! auto-mode-alist '("\\.npmignore\\'" . gitignore-mode))

(after! lua-mode
  (setq lsp-clients-lua-language-server-install-dir
        (concat (file-name-as-directory (getenv "HOME"))
                ".local/opt/lua-language-server")

        lsp-clients-lua-language-server-bin
        (concat (file-name-as-directory lsp-clients-lua-language-server-install-dir)
                "bin/lua-language-server")

        lsp-clients-lua-language-server-main-location
        (concat (file-name-as-directory lsp-clients-lua-language-server-install-dir)
                "main.lua")))

(pushnew! font-lock-extra-managed-props 'display)
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

(setq markdown-fontify-code-blocks-natively t)

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

;; Don't bother checking for an LSP diagnostics provider in markdown-mode
(setq-hook! 'markdown-mode-hook
  lsp-diagnostics-provider :none)

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

(pushnew! auto-mode-alist '("\\.mdx\\'" . markdown-mode))

(add-hook! markdown-mode
  (defun my/markdown-hide-markup-h ()
    "Hide markdown markup."
    (markdown-toggle-markup-hiding 1)))

(after! org
  (setq org-hide-leading-stars nil
        org-startup-indented nil
        org-adapt-indentation nil)

  ;; HACK The following `require' line prevents error messages like this:
  ;; Invalid face reference: org-indent
  (require 'org-indent))

(defadvice! my/inhibit-indentation-a (f &rest r)
  "Prevent `org-indent-mode' from running."
  :around #'doom-docs-org-mode
  (letf! ((#'org-indent-mode #'ignore))
    (apply f r)))

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
  (add-hook! 'org-insert-heading-hook #'my/org-insert-heading-spacing
                                      #'my/org-insert-heading-visibility
                                      #'my/org-insert-heading-evil-state))

(after! org
  (pushnew! org-modules 'ol-man 'ol-info 'ol-w3m))

(after! org
  (setcdr (assoc 'file org-link-frame-setup) #'find-file-other-window))

(setq org-man-command
      (if (and (not IS-MAC) (executable-find "man"))
          #'man
        #'woman))

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
      org-directory "~/org"

      ;; Directories to search for agenda files
      my/org-directories `("work" "life" ,doom-user-dir)
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

  ;; Support exporting from `ein-LANG' source blocks
  (when (modulep! :tools ein)
    (dolist (pair '((ipython . "python")
                    (sh . "bash")))
      (let* ((key (car pair))
             (lang (cdr pair))
             (ein-key (intern (concat "ein-" lang)))
             (kernelspec (alist-get key ox-ipynb-kernelspecs))
             (language-info (alist-get key ox-ipynb-language-infos)))
        (pushnew! ox-ipynb-kernelspecs (cons ein-key kernelspec))
        (pushnew! ox-ipynb-language-infos (cons ein-key language-info))))))

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
    (pushnew! ob-async-no-async-languages-alist "jupyter-bash"))

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
  (pushnew! my/jupyter-envvars
            (list "VIRTUAL_ENV" venv)
            (list "PATH" (concat bindir (path-separator) (getenv "PATH")))))

(after! projectile
  (pushnew! projectile-other-file-alist
            '("org" "el")
            '("el" "org")))

(setq org-ditaa-jar-path
      (cond
       (IS-MAC (file-expand-wildcards "/usr/local/Cellar/ditaa/*/libexec/ditaa-*-standalone.jar"))
       ((file-exists-p "/usr/share/java/ditaa.jar") "/usr/share/java/ditaa.jar")))

(setq org-download-method 'directory
      org-download-image-dir "images")

(require 'org-pomodoro)

(map! :map org-mode-map
      :localleader
      (:when (modulep! :lang org +pomodoro)
       (:prefix ("c" . "clock")
        "p" #'org-pomodoro
        "P" #'org-pomodoro-extend-last-clock)))

(when (equal org-pomodoro-audio-player "/usr/bin/afplay")
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
  (setq fill-column 79)
  (display-fill-column-indicator-mode))

(after! projectile
  (add-to-list 'projectile-project-root-files "pyvenv.cfg")
  (add-to-list 'projectile-project-search-path
               `(,(concat (file-name-as-directory (getenv "HOME")) ".local/pipx/venvs") . 1)))

(pushnew! auto-mode-alist '("pylint" . conf-mode)
                          '("/activate\\'" . shell-script-mode))

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(pushnew! auto-mode-alist '("/Cargo\\.lock\\'" . conf-toml-mode))
(pushnew! auto-minor-mode-alist '("/Cargo\\.lock\\'" . read-only-mode))

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

(pushnew! auto-mode-alist
          '("/\\.config/\\(shell\\|bash\\)/.+" . shell-script-mode)
          '("\\.\\(env\\|cygport\\)\\'" . shell-script-mode))

(after! lsp-yaml
  (let ((f lsp-yaml-schema-store-local-db))
    (unless (file-exists-p f)
      (mkdir (file-name-directory f) t)
      (lsp-yaml-download-schema-store-db))))

(add-hook 'yaml-mode-local-vars-hook #'tree-sitter! 'append)

(put 'lsp-yaml-schemas 'safe-local-variable #'always)

(setq-hook! 'yaml-mode-hook
  display-line-numbers t)

;; REVIEW Compare `kubel' with `kubernetes-el'
;; - kubel is great for working with pods (listing, examining, modifying, logging, and interacting)
;; - kubernetes-el might be closer to Lens in terms of functionality, but I have not tried it yet
(use-package! kubel
  :defer t
  :config
  (defadvice! my/activate-k8s-mode-a (&rest _)
    :after #'kubel-yaml-editing-mode
    (k8s-mode)))

(use-package! kubel-evil
  :after kubel
  :when (modulep! :editor evil +everywhere))

(use-package! kubernetes
  :defer t
  :commands (kubernetes-overview)
  :config
  (setq ;; Disable automatic refresh (call `kubernetes-refresh' manually)
        kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600

        ;; Display pods even if they are done running
        kubernetes-pods-display-completed t))

(use-package! kubernetes-evil
  :after kubernetes-modes
  :when (modulep! :editor evil +everywhere))

(setq +lookup-open-url-fn #'w3m-browse-url)

(after! elfeed
  (add-hook! elfeed-search-mode #'elfeed-update))

(after! elfeed
  ;; Do not truncate RSS entry titles
  (setq elfeed-search-title-max-width 1000)

  ;; Do not truncate RSS entry tags (just need to shift left by 2 characters)
  (defadvice! my/elfeed-format-column-a (str width &optional align)
    "Return STRING truncated or padded to WIDTH - 2 following alignment.
ALIGN should be a keyword :left or :right."
    :override #'elfeed-format-column
    (if (<= width 0)
        ""
      (format (format "%%%s%d.%ds"
                      (if (eq align :left) "-" "")
                      (- width 2)
                      (- width 2))
              str))))

(after! elfeed
  (setq elfeed-search-remain-on-entry t))

(load! "custom" doom-user-dir t)

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

(setq projectile-auto-discover t)

(setq projectile-project-search-path
      (list
       ;; Standard source directories
       (cons "/usr/src" 2)
       (cons "/usr/local/src" 2)
       (cons (concat (file-name-as-directory (getenv "HOME")) ".local/src") 2)

       ;; Personal source directories
       (cons (concat (file-name-as-directory (xdg-user-dir "DOCUMENTS")) "src/work") 2)
       (cons (concat (file-name-as-directory (xdg-user-dir "DOCUMENTS")) "src/life") 2)))

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

;; Map C-? to DEL
(define-key key-translation-map (kbd "C-?") (kbd "DEL"))

;; Map C-i to TAB and provide an alternative mapping for `better-jumper-jump-forward'
(define-key key-translation-map (kbd "C-i") (kbd "TAB"))
(global-set-key (kbd "C-M-,") #'better-jumper-jump-forward)

(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(after! smartparens
  (remove-hook 'eval-expression-minibuffer-setup-hook
               #'doom-init-smartparens-in-eval-expression-h)
  (remove-hook 'minibuffer-setup-hook
               #'doom-init-smartparens-in-minibuffer-maybe-h))

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

(defun my/find-command (key)
  "Find the definition of the command bound to KEY."
  (interactive
   (letf! (defadvice my/find-command-prompt-a (args)
            :filter-args #'read-key-sequence
            (cons "Find function bound to the following key, mouse click, or menu item: "
                  (cdr args)))
     (list (car (help--read-key-sequence 'no-mouse-movement)))))
  (find-function (cadr (help--analyze-key (car key) (cdr key)))))

(define-key! help-map
  "C-f" #'find-function     ;; replaces `view-emacs-FAQ' b/c I rarely use it
  "C-l" #'find-library      ;; replaces `describe-language-environment'
  "C-v" #'find-variable
  "C-w" #'my/alternate-keys ;; replaces `describe-no-warranty' b/c I never use it
  "C-c" #'my/find-command)  ;; replaces `describe-coding-system' b/c I never use it

(remove-hook 'text-mode-hook #'display-line-numbers-mode)

(setq-hook! '(lisp-interaction-mode-hook doom-sandbox-emacs-lisp-mode-hook)
  display-line-numbers nil)

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
  (pushnew! find-sibling-rules '("\\([^/]+\\)\\..*\\'" "\\1\\..*\\'"))
  (define-key! doom-leader-file-map
    "o" #'find-sibling-file)
  (after! which-key
    (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
      (cl-pushnew `((,(format "\\`%s f o\\'" prefix-re)) nil . "Find other file")
                  which-key-replacement-alist))))

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
       :desc "Tab bar"               "T"   #'tab-bar-mode
       :desc "Visual fill column"    "|"   #'visual-fill-column-mode
       :desc "Page break lines"      "C-l" #'page-break-lines-mode

       ;; TODO: Make sure that the display text is correct
       ;; (currently, it is "Visible mode", probably put there by Doom)
       (:when (modulep! :ui vc-gutter +diff-hl)
        :desc "VCS gutter"            "v"   #'diff-hl-mode)))
