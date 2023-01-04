;;; ui/buffer-group/doctor.el -*- lexical-binding: t; -*-

(assert! (not (modulep! :ui popup))
         "This module is incompatible with (:ui popup)")

(when (modulep! +defaults)
  (warn! "The +defaults flag is currently unimplemented"))
