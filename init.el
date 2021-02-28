(straight-use-package 'org)

; we need to set this before tangling config.org since it evals code
(setq org-confirm-babel-evaluate nil)

(require 'org)
(org-babel-tangle-file (expand-file-name "config.org" user-emacs-directory))
(load-file (expand-file-name "config.el" user-emacs-directory))
(when (file-exists-p "~/.config/emacs/local.org")
  (org-babel-tangle-file "~/.config/emacs/local.org")
  (load-file "~/.config/emacs/local.el"))

; make scratch buffer be the first
(setq initial-buffer-choice t)
