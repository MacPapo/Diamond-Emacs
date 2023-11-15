;; -*- lexical-binding: t; -*-

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package dired
  :straight nil
  :config
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t))

(provide 'init-dired)
