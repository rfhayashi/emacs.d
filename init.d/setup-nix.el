;;; -*- lexical-binding: t; -*-

(use-package nix-mode)

;; TODO associate
(use-package nix-ts-mode
  :hook
  (nix-mode . eglot-ensure)
  :init
  (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode)))

(defun my-nixos-switch ()
  (interactive)
  (let ((s (shell)))
    (with-current-buffer s
      (insert (concat "sudo nixos-rebuild switch --flake " (getenv "HOME") "/dev/nixos-config"))
      (comint-send-input))))
