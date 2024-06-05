;;; -*- lexical-binding: t; -*-

(use-package markdown-mode
  :general
  (leader-def :keymaps 'markdown-mode-map
    "gg" 'markdown-follow-link-at-point))

;; ability to edit markdown code blocks in another buffer
(use-package edit-indirect)




