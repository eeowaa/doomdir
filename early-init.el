(when noninteractive
  (pushnew! doom-env-blacklist
            "^AWS_ACCESS_KEY_ID$"
            "^AWS_SECRET_ACCESS_KEY$"
            "^AWS_SESSION_TOKEN$"))
