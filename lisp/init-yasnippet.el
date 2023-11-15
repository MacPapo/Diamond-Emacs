;; -*- lexical-binding: t; -*-

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :bind (:map  yas-minor-mode-map
               ("<backtab>" . yas-expand))
  :config
  ;;(setq yas-snippet-dirs '("~/.emacs.d/personal-snippets"))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-yasnippet)
