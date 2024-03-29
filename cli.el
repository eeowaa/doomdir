;;; DOOMDIR/cli.el -*- lexical-binding: t; -*-

(defcli-group! "Demos"
  :docs "Demonstrations of basic Doom CLI functionality"

  (defcli! echo-hello ()
    "Say \"hello world\" by calling out to echo."
    ;; FIXME This now produces an error trace
    (throw 'exit (list "echo" "hello world")))

  (defcli! print-hello ()
    "Say \"hello world\" by evaluating print!."
    (print! "hello world")))

(defcli-group! "Personal"
  :docs "My own custom Doom CLI commands"

  (defcli! checkout-main ()
    "Checkout main branch for each repo managed by straight."
    ;; FIXME This (probably) now produces an error trace
    (throw 'exit `(,(concat
                     (file-name-as-directory
                      (expand-file-name "bin" doom-user-dir))
                     "doom-checkout-main")))))

(after! doom-cli-env
  (pushnew! doom-env-deny
            "^AWS_ACCESS_KEY_ID$"
            "^AWS_SECRET_ACCESS_KEY$"
            "^AWS_SESSION_TOKEN$"))

(after! doom-cli-env
  (add-to-list 'doom-env-deny "VTERM_BUFFER_NAME"))
