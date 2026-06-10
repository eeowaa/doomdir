;;; forge-extras.el --- Additional functionality for Forge -*- lexical-binding: t; -*-

;;; Commentary:

;; Additional functionality for Forge.

;;; Todo:

;; - Allow filtering of topics by negation (e.g., who is *not* a reviewer)
;; - Allow filtering of topics by description
;; - Allow customizable columns in `forge-topics-mode'

;;; Code:

(require 'forge)

(defvar forge-known-directories nil)

(defun forge-directory (dir)
  "Return a forge-repository corresponding to DIR from the DB."
  (let ((default-directory dir)
        (forge-buffer-repository nil)
        (forge-buffer-topic nil))
    (forge-get-repository :known? nil 'notatpt)))

(defun forge-directory-add (dir)
  "Add a forge-repository corresponding to DIR to the DB."
  (let ((default-directory dir)
        (forge-buffer-repository nil)
        (forge-buffer-topic nil))
    (when-let* ((repo (forge-get-repository :stub? nil 'notatpt)))
      (forge-add-repository repo))))

(defun forge-directory-remove (dir)
  "Remove a forge-repository corresponding to DIR from the DB."
  (when-let* ((repo (forge-directory dir)))
    (forge--msg repo t nil "Removing REPO")
    (forge-remove-repository repo)
    (forge--msg repo t t "Removing REPO")))

(defun forge-directory-sync (dir &optional clean)
  "Update or remove DIR."
  (cond ((and (file-directory-p dir) (member dir forge-known-directories))
         (let ((default-directory dir)
               (forge-buffer-repository nil)
               (forge-buffer-topic nil))
           (when-let* ((repo (forge-get-repository :insert! nil 'notatpt)))
             (forge--pull repo))))
        (clean (forge-directory-remove dir))))

(defun forge-directory-sync-all (&optional clean)
  "Update `forge-known-directories' from upstream."
  (interactive)
  (dolist (dir forge-known-directories)
    (forge-directory-sync dir clean)))


(provide 'forge-extras)
;;; forge-extras.el ends here
