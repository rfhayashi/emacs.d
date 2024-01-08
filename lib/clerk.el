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

(defun clerk--deps-file ()
  (expand-file-name "deps.edn" clerk-directory))

(defun clerk--ensure-deps-file ()
  (let ((deps-file (clerk--deps-file)))
    (when (not (file-exists-p deps-file))
      (with-temp-buffer
	(insert "{:deps {io.github.nextjournal/clerk {:mvn/version \"0.15.957\"}}}")
	(write-file deps-file)))))

(defun clerk-find-notebook ()
  (interactive)
  (when-let ((notebook-file (thread-first
			      (completing-read "Clerk notebook: " (clerk--notebooks))
			      (clerk--notebook-file))))
    (find-file notebook-file)
    (clojure-mode)
    (clerk-mode)
    (clerk--show)))

(defvar clerk-process nil)

(defun clerk--start-process ()
  (let ((default-directory clerk-directory))
    (setq clerk-process
	  (start-file-process-shell-command
	   "clerk"
	   (get-buffer-create "clerk-buffer")
	   "clojure"))))

(defun clerk--eval (input)
  (process-send-string clerk-process (concat input "\n")))

(defun clerk-serve ()
  (interactive)
  (unless clerk-process
    (clerk--start-process)
    (clerk--eval "(require '[nextjournal.clerk :as clerk])")
    (clerk--eval "(clerk/serve! {:browse? true})")))

(defun clerk-show ()
  (interactive)
  (when clerk-process
    (let ((file-name (buffer-file-name)))
      (clerk--eval (concat "(clerk/show! \"" file-name "\")")))))

(define-minor-mode clerk-mode
  "Updates clerk notebook when file is saved."
  :lighter "Clerk"
  (if clerk-mode
      (add-hook 'after-save-hook 'clerk-show nil t)
    (remove-hook 'after-save-hook 'clerk-show nil t)))

(provide 'clerk)
