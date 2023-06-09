;;; cli-help.el -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO: Configure for doom
;; TODO: Configure for openssl (including with no arguments)
;; TODO: Map command line regexps instead of program names (see top-level openssl, git)
;; TODO: Remove help flag from `cli-help--command-line'
;; TODO: Remove or colorize ANSI escape codes (see docker)
;; TODO: Add an alist for special modes (e.g. `man-mode' for git subcommands)

;;; Code:

(require 'subr-x)

(defgroup cli-help nil
  "Display help text from command line programs."
  :group 'external
  :prefix "cli-help-")


;;; Help flags

(defcustom cli-help-default-flag "--help"
  "The default help text flag for command line tools."
  :group 'cli-help
  :type 'string)

(defcustom cli-help-flag-alist nil
  "Alist of programs to help text flags.
If the cdr of an entry is nil, no flag is passed to the command
when invoking help. Programs without entries in `cli-help-flag-alist'
pass `cli-help-default-flag' to get help text."
  :group 'cli-help
  :type '(alist
          :key-type string
          :value-type string))


;;; Help text buffers

(defvar cli-help--buffers nil
  "List of buffers containing CLI help text.")

(defsubst cli-help--reuse-buffer ()
  (default-value 'cli-help-reuse-buffer))

(defcustom cli-help-reuse-buffer nil
  "Reuse the CLI help text buffer when non-nil.
When nil, create distinct buffers for each command's help text.

Only global (default) values of this variable are considered by
functions in the `cli-help' package."
  :group 'cli-help
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (cli-help--delete-other-buffers)
           (cli-help--uniquify-buffers))))

(defsubst cli-help--buffer-name-function ()
  (default-value 'cli-help-buffer-name-function))

(defcustom cli-help-buffer-name-function #'cli-help-default-buffer-name-function
  "Function to compute the name of the CLI help text buffer.
This function is only used when `cli-help-reuse-buffer' is nil
and is used to construct unique buffer names. The function
receives one argument: the string of the command line for which
help text will be output. It should return a string.

Only global (default) values of this variable are considered by
functions in the `cli-help' package."
  :group 'cli-help
  :type 'function
  :initialize #'custom-initialize-default
  :set-after '(cli-help-reuse-buffer) ;; potentially delete buffers first
  :set (lambda (symbol value)
         (set-default symbol value)
         (cli-help--rename-buffers)))

(defsubst cli-help--buffer-name ()
  (default-value 'cli-help-buffer-name))

(defcustom cli-help-buffer-name "*CLI Help*"
  "The name of the buffer used to display CLI help text.
When `cli-help-reuse-buffer' is nil, the value of `cli-help-buffer-name'
will be used by `cli-help-buffer-name-function' to construct
unique buffer names for each command line's help text.

Only global (default) values of this variable are considered by
functions in the `cli-help' package."
  :group 'cli-help
  :type 'string
  :initialize #'custom-initialize-default
  :set-after '(cli-help-reuse-buffer) ;; potentially delete buffers first
  :set (lambda (symbol value)
         (unless (string= value (cli-help--buffer-name))
           (set-default symbol value)
           (cli-help--rename-buffers))))

(defvar-local cli-help--command-line nil
  "The command line corresponding to the current CLI help buffer.
The help flag is not included in this string.")
(put 'cli-help--command-line 'permanent-local t)

(defun cli-help-default-buffer-name-function (cmdline)
  "Return a unique buffer name for CMDLINE in conventional format."
  (let ((cli-help-buffer-name (cli-help--buffer-name)))
    (if (string-suffix-p "*" cli-help-buffer-name)
        (format "%s: %s*"
                (substring cli-help-buffer-name 0
                           (- (length cli-help-buffer-name) 1))
                cmdline)
      (concat cli-help-buffer-name ": " cmdline))))

(defun cli-help--get-buffer-name (cmdline)
  "Return the name of a buffer in which to display CMDLINE help text."
  (if (cli-help--reuse-buffer)
      (cli-help--buffer-name)
    (funcall (cli-help--buffer-name-function) cmdline)))

(defun cli-help--rename-buffer (buffer)
  "Appropriately rename BUFFER containing CLI help text."
  (with-current-buffer buffer
    (rename-buffer (cli-help--get-buffer-name
                    cli-help--command-line))))

(defun cli-help--rename-buffers ()
  "Appropriately rename all buffers containing CLI help text."
  (mapc #'cli-help--rename-buffer cli-help--buffers))

(defun cli-help--uniquify-buffers ()
  "Uniquify buffer names containing CLI help text.
At most, only one buffer can exist named `cli-help-buffer-name',
so we only need to handle up to one buffer rename."
  (catch 'done
    (dolist (buffer cli-help--buffers)
      (let ((name (buffer-name buffer)))
        (when (string= name (cli-help--buffer-name))
          (cli-help--rename-buffer buffer))
          (throw 'done t)))))

(defun cli-help--delete-other-buffers ()
  "Delete uniquely-named buffers containing CLI help text.
If the current buffer is displaying CLI help text, keep it and
rename it to `cli-help-buffer-name'. If other buffers containing
CLI help text exist and are being displayed, refuse to delete
them."
  (dolist (buffer (seq-uniq cli-help--buffers))
    (let ((name (buffer-name buffer)))
      (cond
       ((eq buffer (current-buffer))
        (rename-buffer (cli-help--buffer-name)))
       ((get-buffer-window buffer t)
        (message "Refusing to kill displayed buffer: %s" name))
       (t
        (message "Killing buffer: %s" name)
        (kill-buffer buffer))))))

(defun cli-help--kill-buffer-function ()
  "Remove the current buffer from `cli-help--buffers'.
This should be added to the buffer-local `kill-buffer-hook' for
all CLI help buffers."
  (setq cli-help--buffers (delq (current-buffer) cli-help--buffers)))

(defun cli-help--get-buffer (cmdline)
  "Return a buffer for displaying CMDLINE help text."
  (get-buffer (cli-help--get-buffer-name cmdline)))

(defun cli-help--get-buffer-create (cmdline &optional enable-mode)
  "Return a buffer for displaying CMDLINE help text, creating a new one if needed.
When ENABLE-MODE is non-nil, enable `cli-help-mode' in new buffers."
  (let ((name (cli-help--get-buffer-name cmdline)))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (when enable-mode (cli-help-mode))
          (add-hook 'kill-buffer-hook #'cli-help--kill-buffer-function nil t)
          (car (push (current-buffer) cli-help--buffers))))))

(defun cli-help-switch-buffer (cmdline)
  "Switch to the buffer containing help text of CMDLINE.
When run interactively with no argument, use `completing-read' to
select an existing CLI help buffer. Note that this command is
only useful when `cli-help-reuse-buffer' is nil."
  (interactive
   `(,(completing-read "Select buffer: "
                       (delq cli-help--command-line
                             (mapcar (lambda (buffer)
                                       (with-current-buffer buffer
                                         cli-help--command-line))
                                     cli-help--buffers)))))
  (if-let* ((buffer (cli-help--get-buffer cmdline)))
      (pop-to-buffer buffer)
    (user-error "No buffer exists for command line: %s" cmdline)))


;;; Help text fontification

(defcustom cli-help-fontifier-alist '(("az" . cli-help-azure-fontifier)
                                      ("docker" . cli-help-docker-fontifier))
  "Alist of programs to fontification functions.
If the cdr of an entry is nil, the function specified by
`cli-help-fontifier-function' is called instead."
  :group 'cli-help
  :type '(alist
          :key-type string
          :value-type function))

(defcustom cli-help-fontifier-function #'cli-help-default-fontifier
  "Default fontification function for CLI help buffers."
  :group 'cli-help
  :type 'function)

(defun cli-help-fontify-buffer ()
  "Fontify the current CLI help text buffer."
  (interactive)
  (let* ((program (car-safe (split-string cli-help--command-line)))
         (fontifier (alist-get program cli-help-fontifier-alist
                               cli-help-fontifier-function nil #'string=)))
      (funcall fontifier)))

;;;; Faces

(defface cli-help-usage
  '((t (:inherit outline-1)))
  "Face to use when fontifying usage strings."
  :group 'cli-help)

(defface cli-help-heading
  '((t (:inherit outline-2)))
  "Face to use when fontifying headings."
  :group 'cli-help)

(defface cli-help-flag
  '((t (:inherit outline-3)))
  "Face to use when fontifying flags."
  :group 'cli-help)

(defface cli-help-punctuation
  '((t (:inherit window-divider)))
  "Face to use when fontifying punctuation."
  :group 'cli-help)

;;;; Buttons

(defun cli-help--subcommand-button-action (button)
  (cli-help-subcommand (button-label button)))

(define-button-type 'cli-help-subcommand
  'action #'cli-help--subcommand-button-action)

(defvar-local cli-help--buttons nil)

(defun cli-help--make-button (beg end)
  (push (make-button beg end 'type 'cli-help-subcommand)
        cli-help--buttons))

(defun cli-help--button-description (button)
  (save-excursion
    (goto-char (button-start button))
    (buffer-substring (point) (line-end-position))))

(defun cli-help--button-candidates ()
  (reverse (mapcar #'cli-help--button-description cli-help--buttons)))

(defun cli-help-select-button (button)
  (interactive
   `(,(let* ((description (completing-read "Select: "
                                           (cli-help--button-candidates)))
             (label (car (split-string description))))
        (seq-find (lambda (button)
                    (string= (button-label button) label))
                  cli-help--buttons))))
  (goto-char (button-start button))
  (push-button))

;;;; Utilities

(defun cli-help--match-group-count ()
  "Helper function to return the number of regexp groups matched."
  (/ (1- (length (match-data))) 2))

(defun cli-help--delete-blank-lines ()
  "Helper function to delete consecutive blank lines starting at point."
  (goto-char (line-beginning-position))
  (while (looking-at "^\\s-*$")
    (delete-region (point) (1+ (line-end-position)))))

(defun cli-help--propertize-match (n property value)
  "Helper function to propertize regexp matches."
  (when-let* ((beg (match-beginning n))
              (end (match-end n)))
    (put-text-property beg end property value)))

;;;; Default fontifier

(defvar cli-help-usage-re "^\\(Usage:\\)\\(.*\\)$")
(defvar cli-help-section-re "^\\s-*\\([a-zA-Z0-9].*:\\)\\s-*$")
(defvar cli-help-flag-re "\\s-+\\(\\(?:+\\|--?\\)[^ ,=[]*\\)\\([,=[]*\\)")

(defun cli-help-default-fontifier ()
  "Fontify buffer according to common help text conventions."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (font-lock-mode)
      (set-text-properties (point-min) (point-max) nil)
      (while (not (eobp))
        (cond
         ((let ((case-fold-search t)) (looking-at cli-help-usage-re))
          (cli-help--propertize-match 1 'font-lock-face 'cli-help-heading)
          (cli-help--propertize-match 2 'font-lock-face 'cli-help-usage))
         ((looking-at cli-help-section-re)
          (cli-help--propertize-match 1 'font-lock-face 'cli-help-heading))
         (t
          (while (looking-at cli-help-flag-re)
            (cli-help--propertize-match 1 'font-lock-face 'cli-help-flag)
            (goto-char (1+ (match-end 1))))))
        (forward-line))))
  (setq-local outline-level (lambda () 1)
              outline-regexp
              (format "\\(?:%s\\)\\|\\(?:%s\\)"
                      cli-help-usage-re
                      cli-help-section-re))
  (outline-minor-mode))

;;;; Docker fontifier

(defun cli-help-docker-fontifier ()
  "Fontify buffer according to common help text conventions."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (font-lock-mode)
      (set-text-properties (point-min) (point-max) nil)
      (while (not (eobp))
        (cond
         ((let ((case-fold-search t)) (looking-at cli-help-usage-re))
          (cli-help--propertize-match 1 'font-lock-face 'cli-help-heading)
          (cli-help--propertize-match 2 'font-lock-face 'cli-help-usage))
         ((looking-at cli-help-section-re)
          (cli-help--propertize-match 1 'font-lock-face 'cli-help-heading))
         ((looking-at "^  \\([^ -][^ ]+\\)")
          (cli-help--propertize-match 1 'font-lock-face 'cli-help-flag)
          (cli-help--make-button (match-beginning 1) (match-end 1)))
         (t
          (while (looking-at cli-help-flag-re)
            (cli-help--propertize-match 1 'font-lock-face 'cli-help-flag)
            (goto-char (1+ (match-end 1))))))
        (forward-line))))
  (setq-local outline-level (lambda () 1)
              outline-regexp
              (format "\\(?:%s\\)\\|\\(?:%s\\)"
                      cli-help-usage-re
                      cli-help-section-re))
  (outline-minor-mode))

;;;; Azure CLI fontifier

(defun cli-help-azure-fontifier ()
  "Fontify buffer displaying Azure CLI help text."
  (font-lock-mode)
  (let ((inhibit-read-only t)
        (inhibit-redisplay t)
        (position (point-marker))
        (group-re "^ *\\([^:]+\\) *\\(:\\)?")
        (subgroup-or-command-re "^ *\\([^ ]+\\) +\\(\\[.+\\]\\)? *\\(:\\)")
        (flag-re "^ *\\(--[^ ]+\\(?: --[^ ]+\\)*\\)? +\\(-[^ ]\\)? *\\(\\[[^]]+\\]\\)? *\\(:\\)")
        (example-comment-re "^ \\{4\\}\\([^ ].*\\)")
        (example-re "^ \\{8\\}\\([^ ].*\\)")
        (group-face 'cli-help-usage)
        (subgroup-face 'outline-4)
        (command-face 'cli-help-flag)
        (long-flag-face 'cli-help-flag)
        (short-flag-face 'font-lock-keyword-face)
        (caution-face 'warning)
        (required-flag-face 'error)
        (example-comment-face 'font-lock-comment-face)
        (example-face 'cli-help-usage))
    (goto-char (point-min))
    (set-text-properties (point-min) (point-max) nil)
    (setq cli-help--buttons nil)
    (cli-help--delete-blank-lines)
    (while (not (eobp))
      (cond
       ((looking-at "^\\(?:Group\\|Command\\)$")
        (put-text-property (line-beginning-position) (line-end-position)
                           'font-lock-face 'cli-help-heading)
        (forward-line)
        (when (looking-at group-re)
          (cli-help--propertize-match 1 'font-lock-face group-face)
          (cli-help--propertize-match 2 'font-lock-face 'cli-help-punctuation)))
       ((looking-at "^\\(Subgroups\\|Commands\\):$")
        (let ((header-face (if (string= (match-string 1) "Subgroups")
                               subgroup-face command-face)))
          (put-text-property (line-beginning-position) (line-end-position)
                             'font-lock-face 'cli-help-heading)
          (while (not (looking-at "^ *$"))
            (forward-line)
            (when (looking-at subgroup-or-command-re)
              (cli-help--propertize-match 1 'font-lock-face header-face)
              (cli-help--make-button (match-beginning 1) (match-end 1))
              (cli-help--propertize-match 2 'font-lock-face caution-face)
              (cli-help--propertize-match 3 'font-lock-face 'cli-help-punctuation)))))
       ((looking-at "^\\(?:[^ ].* \\)?Arguments$")
        (put-text-property (line-beginning-position) (line-end-position)
                           'font-lock-face 'cli-help-heading)
        (while (not (looking-at "^ *$"))
          (forward-line)
          (when (looking-at flag-re)
            (cli-help--propertize-match 1 'font-lock-face long-flag-face)
            (cli-help--propertize-match 2 'font-lock-face short-flag-face)
            (cli-help--propertize-match
             3 'font-lock-face (if (string= (match-string 3) "[Required]")
                                   required-flag-face caution-face))
            (cli-help--propertize-match 4 'font-lock-face 'cli-help-punctuation))))
       ((looking-at "^Examples$")
        (put-text-property (line-beginning-position) (line-end-position)
                           'font-lock-face 'cli-help-heading)
        (forward-line)
        ;; FIXME If there are multiple examples, sometimes only the first one gets fontified
        (while (not (or (looking-at "^[^ ]+") (eobp)))
          (cond
           ((looking-at example-comment-re)
            (cli-help--propertize-match 1 'font-lock-face example-comment-face)
            (forward-line))
           ((while (looking-at example-re)
              (cli-help--propertize-match 1 'font-lock-face example-face)
              (when-let ((beg (and (looking-at ".*-$") (match-end 0)))
                         (end (save-excursion
                                (goto-char (1+ beg))
                                (and (looking-at example-re) (match-beginning 1)))))
                (delete-region beg end)
                (put-text-property beg (line-end-position) 'font-lock-face example-face))
              (forward-line)))
           (t (forward-line))))
        (forward-line -1))
       ((looking-at "^\\(To search .* use:\\) \\(az .*\\)$")
        (cli-help--propertize-match 1 'font-lock-face 'italic)
        (cli-help--propertize-match 2 'font-lock-face 'bold)))
      (forward-line))
    (goto-char (marker-position position))
    (setq-local outline-level (lambda () 1)
                outline-regexp
                (format "\\(?:%s\\)"
                        (string-join '("Group" "Subgroups:" "Commands:"
                                       "Command" "\\(?:[^ ].* \\)?Arguments" "Examples")
                                     "\\|")))
    (outline-minor-mode))
  (redisplay))


;;; Commands

(defun cli-help (cmdline)
  "Display the help text of CMDLINE."
  (interactive
   `(,(read-string "Command line: "
                   (when-let* ((cmd cli-help--command-line))
                     (concat cmd " ")))))
  (unless (string-match-p "\\S-" cmdline)
    (user-error "Command line must not be empty"))
  (let* ((fields (split-string cmdline))
         (program (car fields))
         (args (cdr fields))
         (help-flag (alist-get program cli-help-flag-alist
                               cli-help-default-flag nil #'string=))
         (sanitized-cmdline (string-join fields " "))
         (existing-buffer (cli-help--get-buffer sanitized-cmdline))
         (buffer (or existing-buffer
                     (cli-help--get-buffer-create sanitized-cmdline))))
    (when (or (cli-help--reuse-buffer) (not existing-buffer))
      (with-current-buffer buffer
        (setq cli-help--command-line cmdline
              buffer-read-only t)
        (let ((inhibit-redisplay t)
              (inhibit-read-only t)
              (infile nil)
              (destination '(t t)) ;; interleave STDOUT and STDERR
              (display nil)
              (args (append args (list help-flag))))
          (erase-buffer)
          (apply #'call-process program infile destination display args)
          (goto-char (point-min)))
        (cli-help-mode)))
    (pop-to-buffer buffer)))

(defun cli-help-subcommand (subcommand &optional cmdline)
  "Display the help text of a command line SUBCOMMAND.
CMDLINE defaults to the buffer-local value of `cli-help--command-line'."
  (interactive "sSubcommand: ")
  (unless (string-match-p "\\S-" subcommand)
    (user-error "Subcommand must not be empty"))
  (cli-help (concat (or cmdline cli-help--command-line)
                    " " subcommand)))

(defalias 'cli-help-parent-command 'cli-help-supercommand)
(defun cli-help-supercommand (&optional cmdline)
  "Display the help text of a command line's parent command.
CMDLINE defaults to the buffer-local value of `cli-help--command-line'."
  (interactive)
  (let* ((cmdline (or cmdline cli-help--command-line))
         (fields (butlast (split-string cmdline))))
    (unless fields
      (user-error "Command line has no supercommand: %s" cmdline))
    (cli-help (string-join fields " "))))

(defun cli-help-kill-ring-save-cmdline ()
  "Save the command line for this CLI help buffer to the kill ring."
  (interactive)
  (kill-new cli-help--command-line)
  (message "Saved to kill ring: %s" cli-help--command-line))

(defvar cli-help-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") #'cli-help-supercommand)
    (define-key map (kbd ".") #'cli-help-select-button)
    (define-key map (kbd ",") #'cli-help-switch-buffer)
    (define-key map (kbd "m") #'cli-help)
    (define-key map (kbd "C-c C-w") #'cli-help-kill-ring-save-cmdline)
    map))

(define-derived-mode cli-help-mode fundamental-mode "CLI-Help"
  "Major mode for displaying CLI Help text.
This mode is usually activated by the `cli-help' command."
  :group 'cli-help
  (setq buffer-read-only t)
  (cli-help-fontify-buffer)
  ;; TODO Add `cli-help-outline-buffer'
  (button-mode))

;; Rather than enable `hl-line-mode' in the major mode command, set it in the
;; mode hook so that users can easily remove it if desired.
(add-hook 'cli-help-mode-hook #'hl-line-mode)

;;;; Evil bindings

(declare-function evil-define-key "evil-core")
(with-eval-after-load 'evil
  (evil-define-key 'normal cli-help-mode-map (kbd "u") #'cli-help-supercommand)
  (evil-define-key 'normal cli-help-mode-map (kbd ".") #'cli-help-select-button)
  (evil-define-key 'normal cli-help-mode-map (kbd ",") #'cli-help-switch-buffer)
  (evil-define-key 'normal cli-help-mode-map (kbd "gm") #'cli-help)
  (evil-define-key 'normal cli-help-mode-map (kbd "C-c C-w") #'cli-help-kill-ring-save-cmdline))

;;; Vterm integration

(defcustom cli-help-install-vterm-bindings t
  "When non-nil, install default keybindings to `vterm-mode-map'.
In `vterm', `M-h' will send the current command line to `cli-help'.
This also works for `evil' users in normal and insert state."
  :group 'cli-help
  :type 'boolean)

(declare-function vterm--get-prompt-point "vterm")
(with-eval-after-load 'vterm
  (defun cli-help-vterm-prompt ()
    "Display help text for the current command line in `vterm'.
The command must exist on a single line. The help text buffer is
displayed in another window."
    (interactive)
    (let ((display-buffer-overriding-action
           '((display-buffer-reuse-window
              display-buffer-reuse-mode-window
              display-buffer-use-some-window
              display-buffer-pop-up-window)
             . ((inhibit-same-window . t)))))
      (cli-help (buffer-substring
                 (vterm--get-prompt-point)
                 (line-end-position)))))
  (when cli-help-install-vterm-bindings
    (with-eval-after-load 'evil
      (defvar vterm-mode-map)
      (evil-define-key 'normal vterm-mode-map (kbd "M-h") 'cli-help-vterm-prompt)
      (evil-define-key 'insert vterm-mode-map (kbd "M-h") 'cli-help-vterm-prompt))))

(provide 'cli-help)
;;; cli-help.el ends here
