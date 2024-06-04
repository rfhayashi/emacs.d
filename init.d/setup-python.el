;;; -*- lexical-binding: t; -*-

(use-package python
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt")
  (python-shell-prompt-detect-failure-warning nil)
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (add-hook 'inferior-python-mode-hook (lambda () (setq comint-move-point-for-output t))))

;; (use-package conda
;;   :custom
;;   (conda-anaconda-home (expand-file-name "~/anaconda3"))
;;   :config
;;   (conda-env-initialize-interactive-shells)
;;   (conda-env-initialize-eshell)
;;   (conda-env-autoactivate-mode t)
;;   (conda-env-activate "base"))

(use-package elpy
  :init
  (elpy-enable))
