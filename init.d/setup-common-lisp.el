;;; -*- lexical-binding: t; -*-

(use-package slime
  :general
  (leader-def :keymaps 'slime-mode-map
    "ee" 'slime-eval-last-expression
    "ef" 'slime-eval-defun
    "eb" 'slime-eval-buffer
    "ep" 'slime-eval-print-last-expression)
  :hook
  (common-lisp-mode . enable-paredit-mode))
