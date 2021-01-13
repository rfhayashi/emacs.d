(straight-use-package 'org)

(require 'org)
(org-babel-tangle-file (expand-file-name "config.org" user-emacs-directory))

(load-file (expand-file-name "config.el" user-emacs-directory))

; make scratch buffer be the first
(setq initial-buffer-choice t)
