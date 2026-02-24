;;; -*- lexical-binding: t; -*-

(use-package lua-mode)
(use-package fennel-mode
  :hook
  (fennel-mode . enable-paredit-mode)
  (fennel-mode . lsp-deferred)
  :general
  (leader-def :keymaps 'fennel-mode-map
    "ee" 'fennel-eval-last-sexp
    "ef" 'fennel-eval-toplevel-form))
