;;; -*- lexical-binding: t; -*-

(use-package nix-mode)

(defun my-switch-home-manager ()
  (interactive)
  (shell-command (concat "home-manager switch --flake " (getenv "HOME") "/dev/home")))
