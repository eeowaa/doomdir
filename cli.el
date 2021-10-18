;;; DOOMDIR/cli.el -*- lexical-binding: t; -*-

(defcligroup! "Demos"
  "Demonstrations of basic Doom CLI functionality"
  (defcli! echo-hello ()
    "Say \"hello world\" by calling out to echo."
    (throw 'exit (list "echo" "hello world")))

  (defcli! print-hello ()
    "Say \"hello world\" by evaluating print!."
    (print! "hello world")))

(defcligroup! "Personal"
  "My own custom Doom CLI commands"

  (defcli! checkout-main ()
    "Checkout main branch for each repo managed by straight."
    (throw 'exit (list "doom-checkout-main"))))
