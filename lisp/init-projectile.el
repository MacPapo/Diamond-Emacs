;; -*- lexical-binding: t; -*-

(use-package projectile
  :defer 5
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-indexing-method                'alien
        projectile-sort-order                     'modification-time
        projectile-enable-caching                 t
        projectile-per-project-compilation-buffer t
        projectile-mode-line-function             '(lambda ()
                                                     (format " Proj[%s]"
                                                             (projectile-project-name))))
   (projectile-mode +1))

(use-package projectile-git-autofetch
  :diminish projectile-git-autofetch-mode
  :config
  (projectile-git-autofetch-mode 1))

(provide 'init-projectile)
