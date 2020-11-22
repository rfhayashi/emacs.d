(:bindings
 (("SPC" "Global leader key"
   ("b" "Buffer commands"
    ("b" "Switch buffer" helm-buffers-list)
    ("d" "Kill buffer" kill-this-buffer)
    ("l" "List buffers" list-buffers)
    ("r" "Rename buffer" rename-buffer)
    ("w" "Toggle read-only" read-only-mode))

   ("f" "File commands"
    ("f" "Find file" helm-find-files)
    ("s" "Save file" save-buffer)
    ("S" "Save all" evil-write-all)
    ("A" "Find alternate file" find-alternate-file))

   ("s" "Search commands"
    ("s" "Search in buffer" swiper)
    ("p" "Grep in project" counsel-git-grep))

   ("p" "Project"
    ("f" "Find file" helm-projectile-find-file)
    ("p" "Switch project" helm-projectile-switch-project)
    ("b" "Switch to buffer" helm-projectile-switch-to-buffer))

   ("g" "Git"
    ("s" "Magit Status" magit-status))

   ("h" "Help"
    ("d" "Describe"
     ("k" "Describe key" describe-key)
     ("v" "Describe variable" describe-variable)
     ("f" "Describe function" describe-function)
     ("m" "Describe mode" describe-mode)
     ("b" "Describe bindings" describe-bindings)))

   ("w" "Windows"
    ("TAB" "Alternate window" alternate-window)
    ("1" "Delete other windows" delete-other-windows)
    ("/" "Split window right" split-window-right)
    ("-" "Split window below" split-window-below)
    ("o" "Go to other window" other-window)
    ("d" "Delete window" delete-window))

   ;; TODO: this is temporary, using the keybinding from Spacemac's lisp editing
   ;; mode, but we don't want to copy lisp editing mode, so we might look for
   ;; more suitable bindings for this
   ("k" "Structural editing"
    ("s" :sexp/slurp-forward)
    ("b" :sexp/barf-forward)
    ("d" :sexp/kill)
    ("r" :sexp/raise)
    ("p" :sexp/splice))

   ("r" "Org Roam"
    ("f" "Find file" org-roam-find-file)
    ("r" "Today's file" org-roam-dailies-today))

   ("t" "Toggle modes"
    ("l" "Toggle line numbers" linum-mode))

   ("SPC" "Execute command (M-x)" helm-M-x)
   ("u" "Universal prefix" universal-argument)
   ("1" "Select window 1" winum-select-window-1)
   ("2" "Select window 2" winum-select-window-2)
   ("3" "Select window 3" winum-select-window-3)
   ("4" "Select window 4" winum-select-window-4)
   ("5" "Select window 5" winum-select-window-5)
   ("6" "Select window 6" winum-select-window-6)
   ("7" "Select window 7" winum-select-window-7)
   ("8" "Select window 8" winum-select-window-8)
   ("9" "Select window 9" winum-select-window-9)
   ("0" "Select window 10" winum-select-window-10))

  ("," "Project specific leader key"

   ("e" "Evaluate expressions"
    ("b" "Eval buffer" :eval/buffer)
    ("e" "Eval form before cursor" :eval/last-sexp)
    ("f" "Eval top level form" :eval/top-level-form)
    ("p" "Eval and pretty print" :eval/last-sexp-pprint)
    ;; TODO: make this consistent, in clojure buffers it prints to a comment, in elisp it inserts directly
    ("P" "Eval to comment" :eval/last-sexp-pprint-comment)
    ("n" "Eval ns form" :eval/ns-form)
    ("r" "Eval region" :eval/region)
    ("i" "Interrupt eval" :eval/interrupt)
    ("-" "Eval up to point" :eval/up-to-point))

   ("t" "Run tests"
    ("t" "Focused Test" :run-test/focused)
    ("n" "Namespace" :run-test/ns))

   ("s" "REPL"
    ("s" "Toggle REPL" :repl/toggle)
    ("q" "Quit current REPL" :repl/quit)
    ("o" "Switch to Other REPL" :repl/other)
    ("s" "Connect to REPL" :repl/connect))

   ("g" "Go places"
    ("g" "Go to definition" :jump/definition)
    ("b" "Go back" :jump/back)
    ("n" "Go to namespace" :jump/ns)
    ("t" "Go to test/implemenentation" projectile-toggle-between-implementation-and-test))

   ("l" "Link to REPL"
    ("p" "Link with project" sesman-link-with-project)
    ("b" "Link with buffer" sesman-link-with-buffer)
    ("d" "Link with directory" sesman-link-with-directory)
    ("l" "Link least specific" sesman-link-with-least-specific)
    ("u" "Unlink" sesman-unlink))

   ("r" "Refactor"
    ("t" "Threading"
     ("f" "Thread first" :refactor/thread-first)
     ("l" "Thread last"  :refactor/thread-last)
     ("u" "Unwind thread" :refactor/unwind-thread))
    ("s" "Sort ..."
     ("n" "Sort namespace declaration" :refactor/sort-namespace-declaration))
    ("a" "Add ..."
     ("m" "Add missing" :refactor/add-missing))
    ("e" "Extract..."
     ("f" "Extract function" :refactor/extract-function)))

   ("'" "Jack in" :repl/jack-in)
   ("\"" "Jack in Alternate" :repl/jack-in-alt)
   ("&" "Jack in Combined" :repl/jack-in-combined)))

 :modes
 ((global ( :sexp/slurp-forward paredit-forward-slurp-sexp
	    :sexp/barf-forward paredit-forward-barf-sexp
	    :sexp/kill paredit-kill
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
		 :eval/top-level-form cider-eval-defun
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

                 :jump/definition cider-find-var
                 :jump/back cider-pop-back
                 :jump/ns cider-find-ns

                 :refactor/thread-first clojure-thread-first-all
                 :refactor/thread-last clojure-thread-last-all
                 :refactor/unwind-thread clojure-unwind-all

                 :refactor/sort-namespace-declaration clojure-sort-ns
                 :refactor/add-missing cljr-add-missing-libspec
                 :refactor/extract-function cljr-extract-function

                 :repl/jack-in ("Jack-in Clojure" cider-jack-in-clj)
                 :repl/jack-in-alt ("Jack in ClojureScript" cider-jack-in-clj)
                 :repl/jack-in-combined ("Jack in Clj+Cljs" cider-jack-in-clj&cljs)))

  (org-mode (:eval/top-level-form org-babel-execute-src-block
	     :jump/definition org-open-at-point))

  (cider-repl-mode ( :repl/toggle cider-switch-to-last-clojure-buffer
                     :repl/quit cider-quit
                     :repl/other cider-repl-switch-to-other

                     :jump/definition cider-find-var
                     :jump/back cider-pop-back
                     :jump/ns cider-find-ns))))
