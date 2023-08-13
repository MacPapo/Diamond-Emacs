(use-package yasnippet
  :ensure t
  :defer 5
  :diminish yasnippet
  :init
  (yas-global-mode 1)
  :config
  ;; Directory per i tuoi snippet personali, se desideri
  ;; (setq yas-snippet-dirs '("~/.emacs.d/mysnippets"))
  (define-key yas-minor-mode-map (kbd "<backtab>") #'yas-expand))

(use-package yasnippet-snippets
  :ensure t
  :defer 5)

(use-package helm-c-yasnippet
  :ensure t
  :defer 5
  :bind (("C-c Y" . helm-yas-complete)
         ("<f6>" . helm-yas-visit-snippet-file))
  :config
  (setq helm-yas-space-match-any-greedy t))


(provide 'init-yasnippet)
