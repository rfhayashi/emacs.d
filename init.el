;;; -*- lexical-binding: t; -*-

(set-frame-font "Source Code Pro 11" nil t)

(setq make-backup-files nil)

(setq straight-use-package-by-default t)

(setq visible-bell t)

(straight-use-package 'use-package)

(use-package a)
(use-package s)

(use-package diminish)

(defvar space-key-map (make-sparse-keymap))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :init
  (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer t))

(use-package general
  :config
  (general-define-key
   :states '(normal visual)
   :prefix-map 'space-key-map
   :global-prefix "C-SPC"
   :prefix "SPC"))

(general-create-definer leader-def
  :prefix ","
  :states '(normal visual)
  "e" '(:ignore t :which-key "eval")
  "g" '(:ignore t :which-key "jump")
  "r" '(:ignore t :which-key "refactor")
  "t" '(:ignore t :which-key "test"))

(use-package emacs
  :straight nil
  :diminish auto-revert-mode
  :general
  (space-key-map
   "SPC" 'execute-extended-command
   "'" 'shell
   "b" '("buffer" . (keymap))
   "bb" 'ido-switch-buffer
   "bd" 'kill-current-buffer
   "bs" 'scratch
   "f" '("file" . (keymap))
   "ff" 'find-file
   "w" '("window" . (keymap))
   "w/" 'split-window-right
   "w-" 'split-window-below
   "wd" 'delete-window
   "1" 'winum-select-window-1
   "2" 'winum-select-window-2
   "3" 'winum-select-window-3
   "4" 'winum-select-window-4
   "5" 'winum-select-window-5
   "6" 'winum-select-window-6
   "7" 'winum-select-window-7
   "8" 'winum-select-window-8
   "9" 'winum-select-window-9
   "0" 'winum-select-window-0-or-10))

(use-package winum
  :config
  (require 'winum)
  (winum-mode))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(show-paren-mode)

(use-package magit
  :general
  (space-key-map
   "g" '("git" . (keymap))
   "gs" 'magit-status))

(use-package projectile
  :general
  (space-key-map
   "p" '("project". (keymap))
   "pp" 'projectile-switch-project
   "pf" 'projectile-find-file
   "p'" 'projectile-shell))

(use-package scratch
  :straight (:host nil :repo "https://codeberg.org/emacs-weirdware/scratch.git"))

(use-package flycheck)

(use-package atomic-chrome
  :custom
  (atomic-chrome-enable-auto-update t)
  (atomic-chrome-default-major-mode 'python-mode))

(use-package vertico
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous))
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :general
  (:states '(normal visual)
   "/" 'consult-line)
  (space-key-map
   "/" 'consult-git-grep
   "bb" 'consult-buffer))

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
	("C-j" . 'corfu-next)
	("C-k" . 'corfu-previous)))

(use-package lsp-mode
  :general
  (leader-def :keymaps 'lsp-mode-map
    "gg" 'xref-find-definitions
    "gr" 'xref-find-references
    "rr" 'lsp-rename))

(use-package paredit
  :diminish paredit-mode
  :general
  (space-key-map
   "k" '("paredit" . (keymap))
   "kb" 'paredit-forward-barf-sexp
   "kd" 'kill-sexp
   "kr" 'paredit-raise-sexp
   "ks" 'paredit-forward-slurp-sexp))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package buttercup)

(use-package yaml-mode)

(use-package cue-mode)

(use-package just-mode)

(use-package expreg
  :general
  (space-key-map
   "ke" 'expreg-expand)
  :config
  (repeat-mode 1)
  (defvar-keymap expreg-repeat-map
    :repeat t
    "e" 'expreg-expand
    "-" 'expreg-contract)
  (put 'expreg-expand 'repeat-map 'expreg-repeat-map))

;; setup
(let* ((initd-dir (expand-file-name "init.d" user-emacs-directory))
       (files (thread-last
		(directory-files initd-dir)
		(seq-filter (apply-partially 's-ends-with-p ".el")))))
  (dolist (file files)
    (load-file (expand-file-name file initd-dir))))
