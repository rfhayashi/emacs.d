;;; -*- lexical-binding: t; -*-

(use-package clojure-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure"))
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'lsp))

(defun my-cider-test-run-focused-test ()
  "Run test around point"
  (interactive)
  (cider-load-buffer)
  (cider-test-run-test))

(defun my-cider-test-run-ns-tests ()
  "Run namespace test"
  (interactive)
  (cider-load-buffer)
  (cider-test-run-ns-tests nil))

(defun my-set-clojure-keys ()
  (evil-local-set-key
   'normal
   (kbd ",")
   (my-gen-keymap
    '("local"
      (("e" . ("eval"
	       (("b" . cider-eval-buffer)
		("e" . cider-eval-last-sexp)
		("f" . cider-eval-defun-at-point)
		("p" . cider-eval-print-last-sexp))))
       ("g". ("jump"
	      (("d" . cider-doc)
	       ("g" . lsp-find-definition)
	       ("r" . lsp-find-references))))
       ("r" . ("refactor"
	       (("r" . lsp-rename))))
       ("s" . ("repl"
	       (("q" . cider-quit)
		("x" . cider-ns-refresh))))
       ("t" . ("test"
	       (("n" . my-cider-test-run-ns-tests)
		("t" . my-cider-test-run-focused-test))))
       ("'" . cider-jack-in))))))

(use-package cider
  :custom
  (cider-save-file-on-load t)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-test-defining-forms '("deftest" "defspec"))
  (clojure-toplevel-inside-comment-form t)
  (cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow")
  :config
  (add-hook 'clojure-mode-hook 'my-set-clojure-keys))
