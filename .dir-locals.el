((nil . (;; Edit local configuration (if there is a server)
         (projectile-project-configure-cmd
          . "if [ -S /tmp/emacs/server ]; then emacsclient -n custom.el; else echo >&2 'Emacs server is not running'; exit 1; fi")

         ;; Tangle literate config into elisp and shell scripts
         ;; FIXME This seemingly tangles everything, including archived subtrees!
         (projectile-project-compilation-cmd . "org-tangle config.org")

         ;; Ensure that all of our tangled files are symlinked via GNU Stow
         (projectile-project-install-cmd . "cd ~/Documents/src/life/stow-dotfiles && make doom")

         ;; Install package dependencies (elisp and system); do this interactively in case there are any installation prompts
         (projectile-project-package-cmd . "doom sync -u && install/run")
         (projectile-package-use-comint-mode . t)

         ;; Se if there is anything wrong with our setup
         (projectile-project-test-cmd . "doom doctor")

         ;; Run a new instance of Doom Emacs
         (projectile-project-run-cmd . "doom run"))))
