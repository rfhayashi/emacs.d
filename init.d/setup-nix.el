;;; -*- lexical-binding: t; -*-

(use-package nix-mode)

;; until we setup nix tree sitter in macos
(when (eq system-type 'gnu/linux)
  (use-package nix-ts-mode
    :hook
    (nix-ts-mode . lsp-deferred)
    :init
    (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))))

(defun my-nixos-switch ()
  (interactive)
  (let ((s (shell)))
    (with-current-buffer s
      (insert (concat "sudo nixos-rebuild switch --flake " (getenv "HOME") "/dev/nixos-config"))
      (comint-send-input))))
