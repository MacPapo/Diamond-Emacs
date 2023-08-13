;; -*- lexical-binding: t; -*-

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (setq helm-M-x-fuzzy-match                  t
        helm-buffers-fuzzy-matching           t
        helm-recentf-fuzzy-match              t
        helm-locate-fuzzy-match               t
        helm-candidate-number-limit           200
        helm-split-window-inside-p            t
        helm-always-two-windows               nil
        helm-display-buffer-default-height    15
        helm-move-to-line-cycle-in-source     t
        helm-autoresize-max-height            40
        helm-autoresize-min-height            20
        helm-M-x-show-short-doc               t
        helm-default-display-buffer-functions '(display-buffer-in-side-window))
  :bind (("C-x b"   . helm-mini)
         ("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("M-y"     . helm-show-kill-ring)
         ("<f1> f"  . helm-apropos)
         ("C-c o"   . helm-imenu)
         ("C-c b"   . helm-bookmarks)
         ("C-c t"   . helm-themes)
         ("<f1> l"  . helm-locate-library))
  :bind (:map helm-find-files-map
              ("C-c C-i" . helm-ff-properties-persistent))
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1))

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (helm-projectile-on)
  :bind (("C-c p h" . helm-projectile)
         ("C-c p p" . helm-projectile-switch-project)
         ("C-c p f" . helm-projectile-find-file)
         ("C-c p g" . helm-projectile-grep)))

(use-package helm-git-grep
  :ensure t
  :after helm
  :bind (("C-c g" . helm-git-grep)
         ("C-c G" . helm-git-grep-at-point))
  :config
  (setq helm-git-grep-include-submodules t)
  (setq helm-git-grep-use-iomenu-last-pattern t))

(use-package helm-ls-git
  :ensure t
  :bind (("M-g G" . helm-browse-project)
         ("M-g H" . helm-projects-history)))

(use-package helm-gitignore
  :ensure t
  :after helm)

(use-package helm-themes
  :ensure t)

(use-package helm-swoop
  :ensure t
  :after helm
  :bind (("M-i"     . helm-swoop)
         ("M-I"     . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-split-with-multiple-windows nil)
  (setq helm-swoop-split-direction 'split-window-vertically)
  (setq helm-swoop-speed-or-color nil)
  (setq helm-swoop-move-to-line-cycle t)
  (setq helm-swoop-use-line-number-face t)
  (setq helm-swoop-use-fuzzy-match t))

(use-package helm-rg
  :ensure t
  :after helm
  :bind ("C-c k" . helm-rg))

(use-package helm-mt
  :ensure t
  :after helm
  :bind (("C-x T" . helm-mt))
  :config
  (helm-mt/reroute-terminal-functions t))

(use-package helm-tramp
  :ensure t
  :after helm
  :bind (("C-c s" . helm-tramp))
  :config
  (setq tramp-default-method "ssh")
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
  (add-hook 'helm-tramp-pre-command-hook '(lambda () (global-aggressive-indent-mode 0)
                                            (projectile-mode 0)
                                            (editorconfig-mode 0)))
  (add-hook 'helm-tramp-quit-hook '(lambda () (global-aggressive-indent-mode 1)
                                     (projectile-mode 1)
                                     (editorconfig-mode 1))))

(use-package helm-make
  :ensure t
  :after helm
  :bind ("C-c c" . helm-make-projectile))

(use-package helm-descbinds
  :ensure t
  :after helm
  :bind ("<f2> D" . helm-descbinds))

(provide 'init-helm)
