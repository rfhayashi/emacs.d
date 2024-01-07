;;; -*- lexical-binding: t; -*-

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
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
