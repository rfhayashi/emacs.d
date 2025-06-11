;; -*- lexical-binding: t; -*-

(use-package json
  :straight nil
  :custom
  (js-indent-level 2))

(defun my/js-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
