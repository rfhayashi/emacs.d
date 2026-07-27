;;; -*- lexical-binding: t; -*-

(use-package eca
  :hook (eca-chat-mode . company-mode)
  :general
  (space-key-map
   "e" '(:ignore t :which-key "eca")
   "ee" 'eca
   "em" 'eca-chat-select-model
   "ea" 'eca-chat-select-agent))
