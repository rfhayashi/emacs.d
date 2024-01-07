;;; -*- lexical-binding: t; -*-

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
  company
  :config
  (global-company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package
  which-key
  :config
  (which-key-mode))

(use-package winum
  :config
  (require 'winum)
  (winum-mode))

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

(use-package magit)

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

(evil-global-set-key
 'normal
 (kbd "SPC")
 (my-gen-keymap
  '("space"
    (("SPC" . execute-extended-command)
     ("'" . shell)
     ("b" . ("buffer"
	     (("b" . counsel-switch-buffer)
	      ("d" . kill-this-buffer))))
     ("g" . ("git"
	     (("s" . magit-status))))
     ("k" . ("paredit"
	     (("d" . paredit-kill)
	      ("r" . paredit-raise-sexp)
	      ("s" . paredit-forward-slurp-sexp))))
     ("w" . ("window"
	     (("/" . split-window-right)
	      ("-" . split-window-below)
	      ("d" . delete-window))))
     ("1" . winum-select-window-1)
     ("2" . winum-select-window-2)
     ("3" . winum-select-window-3)
     ("4" . winum-select-window-4)
     ("5" . winum-select-window-5)
     ("6" . winum-select-window-6)
     ("7" . winum-select-window-7)
     ("8" . winum-select-window-8)
     ("9" . winum-select-window-9)
     ("0" . winum-select-window-0-or-10)))))

(defun my-set-emacs-lisp-keys ()
  (evil-local-set-key
    'normal
    (kbd ",")
    (my-gen-keymap
     '("local"
       (("e" . ("eval"
		(("e" . eval-last-sexp)
		 ("f" . eval-defun)
		 ("b" . eval-buffer)
		 ("p" . eval-print-last-sexp)))))))))

(add-hook 'emacs-lisp-mode-hook 'my-set-emacs-lisp-keys)
