;;; eeowaa-project.el -*- lexical-binding: t; -*-

;;
;;; Project configuration

(defgroup eeowaa-project nil
  "Project management facilities."
  :group 'tools)

(defcustom eeowaa-project-default-directory "~/Documents/src"
  "The directory beneath which new projects are created by default."
  :group 'eeowaa-project
  :type 'string)

(defcustom eeowaa-project-vcs 'git
  "The VCS to be used in projects."
  :group 'eeowaa-project
  :type '(choice (const git)
                 (const nil)))

(defcustom eeowaa-project-framework 'projectile
  "The Emacs project management system to use."
  :group 'eeowaa-project
  :type '(choice (const projectile)
                 (const nil)))

(defcustom eeowaa-project-browser 'treemacs
  "The Emacs project browser to use."
  :group 'eeowaa-project
  :type '(choice (const treemacs)
                 (const nil)))

;;
;;; Project initialization

(defgroup eeowaa-project-init nil
  "Initialize a project."
  :group 'eeowaa-project)

(defcustom eeowaa-project-init-files-alist '((".gitignore" . nil))
  "Alist of files to create during project initialization.

The `car' of each cell is a string containing a file path relative to the
project root.

The `cdr' is a string containing a path to a file whose contents should be
copied. The file path should be absolute. You may specify `nil' instead of a
file path to create an empty file."
  :group 'eeowaa-project-init)

(defun eeowaa-project-init (directory)
  "Create and open a new project.
With prefix arg, use `default-directory' instead of
`eeowaa-project-default-directory' as the starting point when interactively
selecting DIRECTORY.

This function performs the following steps:

1. Create the directory if it does not exist.
2. Populate the directory according to `eeowaa-project-init-files-alist'.
3. Register and open the project using the project management framework
   specified in `eeowaa-project-framework'.
4. Initialize the VCS with respect to `eeowaa-project-vcs'.
"
  (interactive (list (read-directory-name
                      "Project directory: "
                      (if current-prefix-arg
                          default-directory
                        eeowaa-project-default-directory))))

  ;; Create the directory if it can function as a project
  (when (eq eeowaa-project-framework 'projectile)
    (require 'projectile)
    (when (projectile-ignored-project-p directory)
      (error (format "ignored by projectile: %s" directory))))
  (make-directory directory t)
  (let ((default-directory directory))

    ;; Populate the directory with files
    (dolist (cell eeowaa-project-init-files-alist)
      (let ((target (car cell))
            (source (cdr cell)))
        (cond
         ((file-exists-p target) t)
         ((null source) (write-region "" nil target))
         ((file-readable-p source) (copy-file source target))
         (t (error (format "Not a readable file: %s" source))))))

    ;; Initialize VCS
    (cond
     ((eq eeowaa-project-vcs 'git)
      (require 'magit-status)
      (save-window-excursion
        (magit-init directory)))
     ((null eeowaa-project-vcs) t)
     (t (error (format "Invalid `eeowaa-project-vcs': %s"
                       eeowaa-project-vcs))))

    ;; Register and open the project directory
    (cond
     ((eq eeowaa-project-framework 'projectile)
      (projectile-add-known-project directory)
      (let ((+workspaces-switch-project-function
             (if (eq eeowaa-project-vcs 'git) #'magit-status #'always))
            (+workspaces-on-switch-project-behavior t)) ;; XXX: Doom Emacs
        (projectile-switch-project-by-name directory)))
     ((null eeowaa-project-framework) t)
     (t (error (format "Invalid `eeowaa-project-framework': %s"
                        eeowaa-project-framework))))

    ;; TODO: Display the project browser
    ;; (cond
    ;;  ((eq eeowaa-project-browser 'treemacs)
    ;;   (require 'treemacs)
    ;;   (if (fboundp '+treemacs/toggle) ;; XXX Doom Emacs
    ;;       (+treemacs/toggle)
    ;;     (treemacs-add-and-display-current-project)))
    ;;  ((null eeowaa-project-browser) t)
    ;;  (t (error (format "Invalid `eeowaa-project-browser': %s"
    ;;                    eeowaa-project-browser))))
    ))

;;
;;; Project diagnostics

;; TODO: Implement this section

;; (defvar eeowaa-project-doctor-functions
;;   '(eeowaa-project-check-gitignore
;;     eeowaa-project-check-gitattributes
;;     eeowaa-project-check-envrc
;;     eeowaa-project-check-editorconfig)
;;   "List of functions to run by `eeowaa-project-doctor'.
;; Functions are executed in list order at the project root, should
;; take no arguments, and should return one of the following
;; symbols:
;;
;;   `passed'   The check succeeded.
;;   `warning'  The check succeeded with warnings.
;;   `error'    The check failed.")
;;
;; (defvar eeowaa-project-doctor-disabled-functions nil
;;   "List of functions that should not be run by `eeowaa-project-doctor'.
;; This overrides `eeowaa-project-doctor-functions'.")
;;
;; (defun eeowaa-project-doctor ()
;;   "Run a list of checks against a project."
;;   (interactive)
;;   (let ((result '((passed . 0) (warning . 0) (error . 0))))
;;     (dolist (check eeowaa-project-checks result)
;;       (unless (member step eeowaa-project-doctor-disabled-functions)
;;         ;; TODO: Store results in a compilation buffer
;;         (projectile-run-command-in-root check)))))

(provide 'eeowaa-project)
;;; eeowaa-project.el ends here