;;; -*- lexical-binding: t; -*-

(defun obsidian-backward-delete ()
  (interactive)
  (if (member (char-before (point)) '(?\[ ?\]))
      (paredit-backward-delete)
    (backward-delete-char 1)))

(defun obsidian-open-square ()
  (interactive)
  (paredit-open-square)
  (company-complete))

(defconst obsidian-regex-internal-link (rx "[[" (* (not "]")) "]]"))

(defun obsidian-link-p ()
  (thing-at-point-looking-at obsidian-regex-internal-link))

(defun obsidian-follow-link-at-point ()
  (interactive)
  (cond
   ((markdown-link-p) (markdown-follow-link-at-point))
   ((obsidian-link-p) (push-button))))

(define-derived-mode obsidian-mode markdown-mode "obsidian"
  (define-key obsidian-mode-map (kbd "[") 'obsidian-open-square)
  (define-key obsidian-mode-map (kbd "DEL") 'obsidian-backward-delete))

(add-to-list 'lsp-language-id-configuration '(obsidian-mode . "obsidian"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection "obsidian-lsp")
  :activation-fn (lsp-activate-on "obsidian")
  :server-id 'obsidian))

(add-hook 'obsidian-mode-hook 'lsp)

(defun my-set-obsidian-keys ()
  (evil-local-set-key
   'normal
   (kbd ",")
   (my-gen-keymap
    '("local"
      (("e" . lsp-execute-code-action)
       ("g" . ("jump"
	       (("g" . obsidian-follow-link-at-point)))))))))

(add-hook 'obsidian-mode-hook 'my-set-obsidian-keys)
