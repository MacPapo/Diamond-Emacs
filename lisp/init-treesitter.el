;; -*- lexical-binding: t; -*-

(setq treesit-load-name-override-list nil
      major-mode-remap-alist nil
      treesit-font-lock-level 4)

(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

(provide 'init-treesitter)
