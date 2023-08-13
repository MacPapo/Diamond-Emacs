;; -*- lexical-binding: t; -*-

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-indexing-method                'alien
        projectile-sort-order                     'modification-time
        projectile-enable-caching                 t
        projectile-completion-system              'ivy
        projectile-per-project-compilation-buffer t
        projectile-mode-line-function             '(lambda ()
                                                     (format " Proj[%s]"
                                                             (projectile-project-name)))))

(use-package counsel-projectile
  :after (projectile ivy)
  :ensure t
  :config
  (counsel-projectile-mode 1))

(use-package projectile-git-autofetch
  :diminish projectile-git-autofetch-mode
  :ensure t
  :init
  (projectile-git-autofetch-mode 1))

(provide 'init-projectile)
