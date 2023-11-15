;; -*- lexical-binding: t; -*-

(use-package projectile
  :defer t
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

;; (use-package projectile-git-autofetch
;;   :diminish projectile-git-autofetch-mode
;;   :config
;;   (projectile-git-autofetch-mode 1))

(use-package ibuffer
  :straight nil
  :bind (("C-x C-b" . ibuffer)))

(use-package ibuffer-projectile)

(provide 'init-projectile)
