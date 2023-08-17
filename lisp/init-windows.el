;; -*- lexical-binding: t; -*-

(use-package winner-mode
  :straight nil
  :hook after-init)

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(provide 'init-windows)
