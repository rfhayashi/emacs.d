;;; -*- lexical-binding: t; -*-

(use-package eca
  :general
  (space-key-map
   "e" '(:ignore t :which-key "eca")
   "em" 'eca-chat-select-model
   "ea" 'eca-chat-select-agent))
