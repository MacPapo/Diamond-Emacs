;; -*- lexical-binding: t; -*-

(use-package git-modes)

(use-package git-timemachine
  :defer t
  :bind ("C-x v t" . git-timemachine-toggle))

(use-package magit
  :defer t
  :bind
  (([(meta f12)] . magit-status)
   ("C-x g"      . magit-status)
   ("C-x M-g"    . magit-dispatch))
  :hook (magit-mode . (lambda () (local-unset-key [(meta h)])))
  :config
  (setq-default magit-diff-refine-hunk 'all))

(use-package forge
  :after magit)

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init         . global-diff-hl-mode))
  :bind (:map diff-hl-mode-map
              ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk)))

(provide 'init-git)
