;;; -*- lexical-binding: t; -*-

(defvar my/eca-chat-bottom-width-threshold 120
  "Show new ECA chat windows below when the frame is narrower than this many columns.")

(defun my/eca-chat-display-by-frame-width (display-buffer-fn buffer)
  "Display BUFFER below on narrow frames, otherwise use the configured side."
  (let ((eca-chat-window-side
         (if (< (frame-width) my/eca-chat-bottom-width-threshold)
             'bottom
           eca-chat-window-side)))
    (funcall display-buffer-fn buffer)))

(use-package eca
  :hook (eca-chat-mode . company-mode)
  :custom (eca-chat-window-height 0.40)
  :config
  (advice-add 'eca-chat--display-buffer :around
              #'my/eca-chat-display-by-frame-width)
  :general
  (space-key-map
   "e" '(:ignore t :which-key "eca")
   "ee" 'eca
   "em" 'eca-chat-select-model
   "ea" 'eca-chat-select-agent))
