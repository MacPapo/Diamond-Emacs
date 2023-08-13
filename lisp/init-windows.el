;; -*- lexical-binding: t; -*-

(add-hook 'after-init-hook 'winner-mode)

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  ;; Visualizza il numero della finestra nel centro
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(provide 'init-windows)
