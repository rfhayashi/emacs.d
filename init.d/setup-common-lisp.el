;;; -*- lexical-binding: t; -*-

(use-package sly
  :custom
  (inferior-lisp-program "sbcl")
  :general
  (leader-def :keymaps 'sly-mode-map
    "ee" 'sly-eval-last-expression
    "ef" 'sly-eval-defun
    "eb" 'sly-eval-buffer
    "ep" 'sly-eval-print-last-expression)
  :hook
  (lisp-mode . enable-paredit-mode))
