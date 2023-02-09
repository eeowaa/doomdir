;;; tools/bitwarden/doctor.el -*- lexical-binding: t; -*-

(assert! (executable-find "bw")
         "This module requires the bw executable!")
