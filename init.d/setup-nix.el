;;; -*- lexical-binding: t; -*-

(use-package nix-mode)

(use-package nix-ts-mode
  :hook
  (nix-ts-mode . eglot-ensure)
  :init
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode)))

(defun my-nixos-switch ()
  (interactive)
  (let ((s (shell)))
    (with-current-buffer s
      (insert (concat "sudo nixos-rebuild switch --flake " (getenv "HOME") "/dev/nixos-config"))
      (comint-send-input))))
