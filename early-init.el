;; HACK With Doom Emacs moving away from `use-package', the `use-package-hook!'
;; macro is now defined in $EMACSDIR/modules/config/use-package/init.el.
;; Unfortunately, this isn't early enough in the startup sequence for the
;; `use-package-hook!' to be used as intended (in $DOOMDIR/init.el), so I
;; redefine it at the start of $DOOMDIR/early-init.el.
(defmacro use-package-hook! (package when &rest body)
  "Reconfigures a package's `use-package!' block.

This macro must be used *before* PACKAGE's `use-package!' block. Often, this
means using it from your DOOMDIR/init.el.

Under the hood, this uses use-package's `use-package-inject-hooks'.

PACKAGE is a symbol; the package's name.
WHEN should be one of the following:
  :pre-init :post-init :pre-config :post-config

WARNINGS:
- The use of this macro is more often than not a code smell. Use it as last
  resort. There is almost always a better alternative.
- If you are using this solely for :post-config, stop! `after!' is much better.
- If :pre-init or :pre-config hooks return nil, the original `use-package!''s
  :init/:config block (respectively) is overwritten, so remember to have them
  return non-nil (or exploit that to overwrite Doom's config)."
  (declare (indent defun))
  (unless (memq when '(:pre-init :post-init :pre-config :post-config))
    (error "'%s' isn't a valid hook for use-package-hook!" when))
  `(progn
     (setq use-package-inject-hooks t)
     (add-hook ',(intern (format "use-package--%s--%s-hook"
                                 package
                                 (substring (symbol-name when) 1)))
               (lambda () ,@body)
               'append)))

(unless initial-window-system
  (use-package-hook! company-box :pre-config nil))

(use-package-hook! persp-mode
  :pre-init (setq persp-keymap-prefix (kbd "C-c w")))
