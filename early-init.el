(use-package-hook! persp-mode
  :pre-init (setq persp-keymap-prefix (kbd "C-c w")))

(when noninteractive
  (pushnew! doom-env-blacklist
            "^AWS_ACCESS_KEY_ID$"
            "^AWS_SECRET_ACCESS_KEY$"
            "^AWS_SESSION_TOKEN$"))
