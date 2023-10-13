;; -*- lexical-binding: t; -*-

(require 'ob-tangle)

(defun tangle-config ()
  "Automatically tangle `config.org`, delete old .elc files, and byte-compile."
  (when (string= (buffer-file-name)
      	   (expand-file-name (concat user-emacs-directory
      				     "config.org")))
    (org-babel-tangle)
    ;; (let ((tangled-files (org-babel-tangle)))
    ;;   (dolist (file tangled-files)
    ;;     (let ((elc-file (concat file "c")))
    ;;       (when (file-exists-p elc-file)
    ;;         (delete-file elc-file))
    ;;       ;; Do not byte-compile init.el and early-init.el
    ;;       (unless (or (string= (file-name-nondirectory file) "init.el")
    ;;                   (string= (file-name-nondirectory file) "early-init.el"))

    ;;         (byte-compile-file file)))))
    ))

(add-hook 'org-mode-hook (lambda ()
      		     (add-hook 'after-save-hook #'tangle-config)))

(provide 'auto-tangle)
