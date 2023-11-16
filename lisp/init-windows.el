;; -*- lexical-binding: t; -*-

(use-package winner-mode
  :straight nil
  :hook after-init
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1)
  (remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(provide 'init-windows)
