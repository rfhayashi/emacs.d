;;; -*- lexical-binding: t; -*-

(use-package clojure-mode
  :hook
  (clojure-mode . enable-paredit-mode)
  (clojure-mode . eglot-ensure))

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

(use-package cider
  :custom
  (cider-save-file-on-load t)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-test-defining-forms '("deftest" "defspec"))
  (clojure-toplevel-inside-comment-form t)
  (cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow")
  :general
  (leader-def :keymaps 'clojure-mode-map
    "eb" 'cider-eval-buffer
    "ee" 'cider-eval-last-sexp
    "ef" 'cider-eval-defun-at-point
    "ep" 'cider-eval-print-last-sexp
    "gd" 'cider-doc
    "gg" 'xref-find-definitions
    "gr" 'xref-find-references
    "rr" 'eglot-rename
    "s" '(:ignore t :which-key "repl")
    "sq" 'cider-quit
    "tn" 'my-cider-test-run-ns-tests
    "tt" 'my-cider-test-run-focused-test
    "'" 'cider-jack-in))
