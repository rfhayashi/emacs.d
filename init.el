(straight-use-package 'org)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

; make scratch buffer be the first
(setq initial-buffer-choice t)
