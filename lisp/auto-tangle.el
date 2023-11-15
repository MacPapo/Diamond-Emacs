;; -*- lexical-binding: t; -*-

(require 'ob-tangle)

(defun tangle-config ()
  "Automatically tangle `config.org`, delete old .elc files, and byte-compile."
  (when (string= (buffer-file-name)
                 (expand-file-name (concat user-emacs-directory
                                           "config.org")))
    (org-babel-tangle)))

(provide 'auto-tangle)
