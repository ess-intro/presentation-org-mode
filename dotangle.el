#!/usr/bin/emacs --script
(let
    ((org-confirm-babel-evaluate nil))   ; don't ask if it's okay to run fixups
  (package-initialize)                  ; in case 'org is a package
  (require 'org)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)))
  ;; command-line-args-left: https://www.emacswiki.org/emacs/EmacsScripts
  (mapcar 'org-babel-tangle-file command-line-args-left))
