(normal|visual
 ("SPC" "Global leader key"
   ("b" "Buffer commands"
    ("b" "Switch buffer" counsel-switch-buffer)
    ("d" "Kill buffer" kill-this-buffer)
    ("l" "List buffers" list-buffers)
    ("r" "Rename buffer" rename-buffer)
    ("s" "Scratch buffer" scratch)
    ("w" "Toggle read-only" read-only-mode))

   ("f" "File commands"
    ("f" "Find file" find-file)
    ("s" "Save file" save-buffer)
    ("S" "Save all" evil-write-all)
    ("A" "Find alternate file" find-alternate-file))

   ("s" "Search commands"
    ("s" "Search in buffer" swiper))

   ("p" "Project"
    ("f" "Find file" projectile-find-file)
    ("p" "Switch project" projectile-switch-project)
    ("b" "Switch to buffer" projectile-switch-to-buffer)
    ("'" "Start a shell" projectile-run-shell))

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
    ("d" "Delete window" delete-window)
    ("u" "Undo window config" winner-undo))

   ("k" "Structural editing"
    ("s" "Slurp Forward" :sexp/slurp-forward)
    ("b" "Barf Forward" :sexp/barf-forward)
    ("d" "Kill" :sexp/kill)
    ("r" "Raise" :sexp/raise)
    ("p" "Splice" :sexp/splice))

   ("r" "Org Roam"
    ("f" "Find file" org-roam-find-file)
    ("r" "Today's file" org-roam-dailies-find-today))

   ("t" "Toggle modes"
    ("l" "Toggle line numbers" linum-mode))

   ("SPC" "Execute command (M-x)" execute-extended-command)
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
   ("0" "Select window 10" winum-select-window-10)
   ("'" "Shell" shell)
   ("/" "Find in Project" projectile-ag))

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
    ("-" "Eval up to point" :eval/up-to-point)
    ("t" "To portal" :eval/to-portal))

   ("t" "Run tests"
    ("t" "Focused Test" :run-test/focused)
    ("n" "Namespace" :run-test/ns))

   ("s" "REPL"
    ("s" "Toggle REPL" :repl/toggle)
    ("q" "Quit current REPL" :repl/quit)
    ("o" "Switch to Other REPL" :repl/other)
    ("s" "Connect to REPL" :repl/connect)
    ("x" "Refresh namespaces" :repl/refresh))

   ("g" "Go places"
    ("g" "Go to definition" :jump/definition)
    ("b" "Go back" :jump/back)
    ("n" "Go to namespace" :jump/ns)
    ("r" "Find references" :jump/references)
    ("t" "Go to test/implemenentation" projectile-toggle-between-implementation-and-test))

   ("l" "Link to REPL"
    ("p" "Link with project" sesman-link-with-project)
    ("b" "Link with buffer" sesman-link-with-buffer)
    ("d" "Link with directory" sesman-link-with-directory)
    ("l" "Link least specific" sesman-link-with-least-specific)
    ("u" "Unlink" sesman-unlink))

   ("r" "Refactor"
    ("r" "Rename" :refactor/rename)
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
