;;; -*- lexical-binding: t; -*-

(defun my-set-markdown-keys ()
  (evil-local-set-key
   'normal
   (kbd ",")
   (my-gen-keymap
    '("local"
      (("g" . ("jump"
	       (("g" . markdown-follow-link-at-point)))))))))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'my-set-markdown-keys))

;; ability to edit markdown code blocks in another buffer
(use-package edit-indirect)




