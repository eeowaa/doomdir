;;; ol-project.el --- Links to project files -*- lexical-binding: t; -*-

;;; Commentary:

;; This library introduces the "project" link type in Org. The idea is that
;; "project" links should be the same as "file" links, except paths are relative
;; to project root directories. Project IDs are statically mapped to root
;; directories via `ol-project-roots'. Dynamic project ID/root mapping is
;; currently unimplemented.

;;; Todo:

;; - Store links as "project" type if `ol-project-root-lookup-function' returns non-nil.
;;   If the project exists within `ol-project-roots', use that project identifier (default ".").
;; - Convert `defvar' forms to `defcustom' forms.
;; - Implement project link export. (Think about how teams would utilize this feature.)

;;; Code:

(require 'ol)
(require 'project)
(eval-when-compile (require 'cl-lib))

(defun ol-project--default-root-lookup ()
  (when-let ((project (project-current nil)))
    (project-root (project-current))))

(defvar ol-project-root-lookup-function #'ol-project--default-root-lookup
  "Function to find the root of the current project.
The function is called without arguments.")

(defvar ol-project-roots nil
  "Mapping from project names to project roots.")

(defvar ol-project-case-fold nil
  "When non-nil, project names and roots are case-insensitive.")

(defun ol-project-link-parse (path)
  "Return (\"PROJECT\" . \"FILE\") for PATH of \"PROJECT/FILE\".
If \"/FILE\" or \"FILE\" is omitted from PATH, it defaults to \".\",
signifying the root directory of PROJECT."
  (if (string-match "\\`\\([^/]+\\)\\(?:/\\(.*\\)\\)?\\'" path)
      (cons (match-string 1 path)
            (or (match-string 2 path) "."))
    (user-error "Malformed project link: %s" path)))

(defun ol-project-link-follow (path _)
  (cl-destructuring-bind (project . file) (ol-project-link-parse path)
    (if-let* ((root (if (equal "." project)
                        (funcall ol-project-root-lookup-function)
                      (cdr (assoc-string project ol-project-roots ol-project-case-fold))))
              (default-directory (file-name-as-directory
                                  (expand-file-name root))))
        (org-link-open-as-file file nil)
      (user-error "Unknown project: %s" project))))

(org-link-set-parameters "project"
                         :follow #'ol-project-link-follow)

(provide 'ol-project)
;;; ol-project.el ends here
