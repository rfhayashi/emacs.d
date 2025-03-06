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

(defun my-cider-portal-open ()
  (interactive)
  (let ((repl (cider-current-repl)))
    (nrepl-send-sync-request `("op" "eval" "code" "(portal/open)") repl)))

(defun my-cider-system-reset ()
  (interactive)
  (let ((repl (cider-current-repl)))
    (nrepl-send-sync-request `("op" "eval" "code" "(repl/reset)") repl)))

(defun my-cider-system-stop ()
  (interactive)
  (let ((repl (cider-current-repl)))
    (nrepl-send-sync-request `("op" "eval" "code" "(repl/stop)") repl)))

(use-package cider
  :custom
  (cider-save-file-on-load t)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-test-defining-forms '("deftest" "defspec"))
  (clojure-toplevel-inside-comment-form t)
  (cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow")
  (cider-clojure-cli-global-aliases ":user:dev")
  :general
  (leader-def :keymaps 'clojure-mode-map
    "eb" 'cider-eval-buffer
    "ee" 'cider-eval-last-sexp
    "ef" 'cider-eval-defun-at-point
    "ep" 'cider-eval-print-last-sexp
    "gd" 'cider-doc
    "s" '(:ignore t :which-key "repl")
    "sr" 'my-cider-system-reset
    "ss" 'my-cider-system-stop
    "sq" 'cider-quit
    "tn" 'my-cider-test-run-ns-tests
    "tt" 'my-cider-test-run-focused-test
    "'" 'cider-jack-in)
  :config
  (put-clojure-indent 'match 1)
  (define-clojure-indent
   (def-source '(1 nil nil (1)))
   (def-sink '(1 nil nil (1)))))
