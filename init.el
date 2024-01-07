(setq make-backup-files nil)

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(use-package
  evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1))

(use-package
  evil-collection
  :init
  (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer t))

(use-package
  which-key
  :config
  (which-key-mode))

(use-package
  zenburn-theme
  :config
  (load-theme 'zenburn t))

(show-paren-mode)

(use-package
  paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

(use-package counsel
  :custom
  (ivy-initial-inputs-alist nil)
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  :bind (:map ivy-minibuffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)))
