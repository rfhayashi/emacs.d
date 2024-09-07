;;; -*- lexical-binding: t; -*-

(use-package nix-mode)

(defun my-switch-nix-os ()
  (interactive)
  (let ((s (shell)))
    (with-current-buffer s
      (insert (concat "sudo nixos-rebuild switch --flake " (getenv "HOME") "/dev/nixos-config"))
      (comint-send-input))))
