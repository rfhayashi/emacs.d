;;; -*- lexical-binding: t; -*-

(setq make-backup-files nil)

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(require 'org)
(org-babel-tangle-file (expand-file-name "dotfiles.org" user-emacs-directory))

(use-package a)
(use-package s)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :init
  (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer t))

(use-package general)

(use-package which-key
  :config
  (which-key-mode))

(use-package winum
  :config
  (require 'winum)
  (winum-mode))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(show-paren-mode)

(use-package paredit)

(use-package magit)

(use-package ag)

(use-package projectile
  :config
  (projectile-mode +1)
  :init
  (when (file-directory-p "~/dev")
    (setq projectile-project-search-path '("~/dev"))))

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
  :general (:states '(normal visual)
	    "/" 'consult-line))

;; keybindings

(defun my-gen-keymap (desc-map)
  (seq-let (desc map) desc-map
    (let ((keymap (make-sparse-keymap)))
      (dolist (mapping map)
	(my-gen-key keymap mapping))
      (cons desc keymap))))

(defun my-gen-key (parent-map key-data)
  (let ((key (kbd (car key-data)))
	(val (cdr key-data)))
    (define-key parent-map key
      (if (symbolp val)
	  val
	(my-gen-keymap val)))))

(let ((space-key-map
       (my-gen-keymap
	'("space"
	  (("SPC" . execute-extended-command)
	   ("'" . shell)
	   ("b" . ("buffer"
		   (("b" . ido-switch-buffer)
		    ("d" . kill-this-buffer)
		    ("s" . scratch))))
	   ("f" . ("file"
		   (("f" . find-file))))
	   ("g" . ("git"
		   (("s" . magit-status))))
	   ("k" . ("paredit"
		   (("b" . paredit-forward-barf-sexp)
		    ("d" . kill-sexp)
		    ("r" . paredit-raise-sexp)
		    ("s" . paredit-forward-slurp-sexp))))
	   ("p" . ("project"
		   (("f" . projectile-find-file)
		    ("p" . projectile-switch-project)
		    ("'" . projectile-run-shell))))
	   ("w" . ("window"
		   (("/" . split-window-right)
		    ("-" . split-window-below)
		    ("d" . delete-window))))
	   ("/" . consult-git-grep)
	   ("1" . winum-select-window-1)
	   ("2" . winum-select-window-2)
	   ("3" . winum-select-window-3)
	   ("4" . winum-select-window-4)
	   ("5" . winum-select-window-5)
	   ("6" . winum-select-window-6)
	   ("7" . winum-select-window-7)
	   ("8" . winum-select-window-8)
	   ("9" . winum-select-window-9)
	   ("0" . winum-select-window-0-or-10))))))
  (evil-global-set-key 'normal (kbd "SPC") space-key-map)
  (evil-global-set-key 'normal (kbd "C-SPC") space-key-map))

;; setup
(let* ((initd-dir (expand-file-name "init.d" user-emacs-directory))
       (files (thread-last
		(directory-files initd-dir)
		(seq-filter (apply-partially 's-ends-with-p ".el")))))
  (dolist (file files)
    (load-file (expand-file-name file initd-dir))))
