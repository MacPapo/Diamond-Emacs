(use-package ssh-deploy
  :after helm-tramp
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :config
  (ssh-deploy-line-mode)
  (ssh-deploy-add-menu))

(setq remote-file-name-inhibit-cache nil)
(setq tramp-verbose 6)
(setq tramp-inline-compress-start-size 1000000)
(setq tramp-default-method "ssh")

(provide 'init-tramp)
