;;; -*- lexical-binding: t; -*-

(use-package emacs-lisp-mode
  :straight nil
  :general
  (leader-def :keymaps 'emacs-lisp-mode-map
    "ee" 'eval-last-sexp
    "ef" 'eval-defun
    "eb" 'eval-buffer
    "ep" 'eval-print-last-sexp
    "gg" 'evil-goto-definition
    "gr" 'xref-find-references)
  :hook
  (emacs-lisp-mode . enable-paredit-mode))
