;;; -*- lexical-binding: t; -*-

(use-package clojure-mode
  :hook
  (clojure-mode . enable-paredit-mode)
  (clojure-mode . lsp-deferred))

(defun my-cider-babashka-repl-p ()
  (let ((repl-buf (cider-current-repl)))
    (with-current-buffer repl-buf
      (save-excursion
	(goto-char (point-min))
	(search-forward "Babashka" nil t)))))

(let* ((template "
(let [out (java.io.StringWriter.)
      summary (binding [clojure.test/*test-out* out]
                (clojure.test/run-test-var (var %s/%s)))]
  {:summary summary
   :out (str out)})
")
       (code (lambda (ns var)
	       (format template ns var))))
  (defun my-babashka-run-focused-test ()
    (let* ((found (cider--extract-test-var-at-point))
	   (found-ns (car found))
	   (found-var (cadr found)))
      (if (not found-var)
	  (message "No test found at point")
	(cider-nrepl-send-request
	 `("op" "eval" "code" ,(funcall code found-ns found-var))
	 (lambda (response)
	   (when (nrepl-dict-contains response "value")
	     (with-current-buffer (get-buffer-create "bb-test-report")
	       (goto-char (point-max))
	       (insert (gethash :out (parseedn-read-str (nrepl-dict-get response "value"))))
	       (set-window-point
		(get-buffer-window (current-buffer) 'visible)
		(point-max))))))))))

(defun my-cider-test-run-focused-test ()
  "Run test around point"
  (interactive)
  (cider-eval-defun-at-point) 
  (if (my-cider-babashka-repl-p)
      (my-babashka-run-focused-test)
    (cider-test-run-test)))

;; TODO support babashka
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
  (cider-clojure-cli-global-options "-Sforce -J-XX:-OmitStackTraceInFastThrow")
  (cider-clojure-cli-global-aliases ":user:dev")
  (cider-babashka-global-options (format
				  "--init %s -Sdeps '{:deps {djblue/portal {:mvn/version \"0.58.5\"} global/user {:local/root \"%s\"}}}'"
				  (expand-file-name "~/.clojure/injections/src/user.clj")
				  (expand-file-name "~/.clojure/injections")))
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
