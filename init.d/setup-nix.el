;;; -*- lexical-binding: t; -*-

(use-package nix-mode
  :hook
  (nix-mode . eglot-ensure))

(defun my-nixos-switch ()
  (interactive)
  (let ((s (shell)))
    (with-current-buffer s
      (insert (concat "sudo nixos-rebuild switch --flake " (getenv "HOME") "/dev/nixos-config"))
      (comint-send-input))))
