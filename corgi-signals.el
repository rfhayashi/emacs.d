((corkey-local-mode ( :sexp/slurp-forward paredit-forward-slurp-sexp
	    :sexp/barf-forward paredit-forward-barf-sexp
	    :sexp/kill kill-sexp
	    :sexp/raise paredit-raise-sexp
	    :sexp/splice paredit-splice-sexp))

  (emacs-lisp-mode ( :eval/last-sexp eval-last-sexp
                     :eval/top-level-form eval-defun
                     :eval/buffer eval-buffer
                     :eval/last-sexp-pprint pprint-to-buffer-last-sexp
                     :eval/region eval-region
                     :repl/toggle ielm
                     :eval/last-sexp-pprint-comment pprint-to-buffer-last-sexp-to-current-buffer
                     :jump/definition xref-find-definitions
                     :jump/back xref-pop-marker-stack))

  (clojure-mode (:sexp/forward clojure-forward-logical-sexp
                 :sexp/backward clojure-backward-logical-sexp

                 :eval/last-sexp cider-eval-last-sexp
                 :eval/last-sexp-pprint cider-pprint-eval-last-sexp
                 :eval/last-sexp-pprint-comment cider-pprint-eval-last-sexp-to-comment
		 :eval/top-level-form cider-eval-defun-at-point
                 :eval/ns-form cider-eval-ns-form
                 :eval/last-sexp-replace cider-eval-last-sexp-and-replace
                 :eval/buffer cider-eval-buffer
                 :eval/region cider-eval-region
                 :eval/interrupt cider-interrupt
                 :eval/up-to-point cider-eval-sexp-up-to-point

		 :run-test/focused my/cider-test-run-focused-test
		 :run-test/ns my/cider-test-run-ns-tests

                 :repl/toggle cider-switch-to-repl-buffer
                 :repl/quit cider-quit
                 :repl/other cider-repl-switch-to-other
                 :repl/connect cider-connect
                 :repl/refresh cider-refresh 

                 :jump/definition lsp-find-definition
                 :jump/back cider-pop-back
                 :jump/ns cider-find-ns
                 :jump/references lsp-find-references

                 :refactor/rename lsp-rename
                 :refactor/thread-first clojure-thread-first-all
                 :refactor/thread-last clojure-thread-last-all
                 :refactor/unwind-thread clojure-unwind-all

                 :refactor/sort-namespace-declaration clojure-sort-ns
                 :refactor/add-missing cljr-add-missing-libspec
                 :refactor/extract-function cljr-extract-function

                 :repl/jack-in ("Jack-in Clojure" cider-jack-in-clj)
                 :repl/jack-in-alt ("Jack in ClojureScript" cider-jack-in-clj)
                 :repl/jack-in-combined ("Jack in Clj+Cljs" cider-jack-in-clj&cljs)))

  (org-mode (:eval/top-level-form org-ctrl-c-ctrl-c
	     :eval/buffer org-babel-execute-buffer
             :eval/to-portal my/org-babel-execute-src-block-to-clojure-portal
	     :jump/definition org-open-at-point
             :repl/jack-in my/ob-clojure-cider-jack-in-clj))

  (cider-repl-mode ( :repl/toggle cider-switch-to-last-clojure-buffer
                     :repl/quit cider-quit
                     :repl/other cider-repl-switch-to-other

                     :jump/definition cider-find-var
                     :jump/back cider-pop-back
                     :jump/ns cider-find-ns)))
