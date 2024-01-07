;;; -*- lexical-binding: t; -*-

(require 's)

(defconst clerk-extension ".clerk")
(defvar clerk-directory (expand-file-name ".clerk" (getenv "HOME")))

(defun clerk--ensure-directory-exists (dir)
  (make-directory dir t))

(defun clerk--notebooks-dir ()
  (expand-file-name "notebooks" clerk-directory))

(defun clerk--notebooks ()
  (let ((notebooks-dir (clerk--notebooks-dir)))
    (clerk--ensure-directory-exists notebooks-dir)
    (thread-last
      (directory-files notebooks-dir)
      (seq-filter (apply-partially 's-ends-with-p clerk-extension))
      (seq-map (apply-partially 's-chop-suffix clerk-extension)))))

(defun clerk--notebook-file (notebook)
  (when (not (s-blank-p notebook))
    (expand-file-name (concat notebook clerk-extension) (clerk--notebooks-dir))))

(defun clerk-find-notebook ()
  (interactive)
  (when-let ((notebook-file (thread-first
			      (completing-read "Clerk notebook: " (clerk--notebooks))
			      (clerk--notebook-file))))
    (find-file notebook-file)
    (clojure-mode)))

(provide 'clerk)
