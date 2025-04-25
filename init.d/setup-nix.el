;;; -*- lexical-binding: t; -*-

(use-package nix-mode
  :hook
  (nix-mode . lsp-deferred))

(use-package lsp-nix
  :straight nil
  :after (lsp-mode)
  :custom
  (lsp-nix-nil-formatter ["nixfmt"]))

(defun my-nixos-switch ()
  (interactive)
  (let ((s (shell)))
    (with-current-buffer s
      (insert (concat "sudo nixos-rebuild switch --flake " (getenv "HOME") "/dev/nixos-config"))
      (comint-send-input))))
