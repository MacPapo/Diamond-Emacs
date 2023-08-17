(use-package yasnippet
  :diminish yasnippet
  :bind (:map  yas-minor-mode-map
               ("<backtab>" . yas-expand))
  :config
  ;;(setq yas-snippet-dirs '("~/.emacs.d/personal-snippets"))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package helm-c-yasnippet
  :after (yasnippet helm)
  :bind (("C-c Y" . helm-yas-complete)
         ("<f6>"  . helm-yas-visit-snippet-file))
  :config
  (setq helm-yas-space-match-any-greedy t))


(provide 'init-yasnippet)
