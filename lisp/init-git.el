;; -*- lexical-binding: t; -*-

(use-package git-blamed
  :ensure t)

(use-package git-modes
  :ensure t)

(use-package git-timemachine
  :ensure t
  :bind ("C-x v t" . git-timemachine-toggle))

(use-package magit
  :ensure t
  :bind
  (([(meta f12)] . magit-status)
   ("C-x g"      . magit-status)
   ("C-x M-g"    . magit-dispatch))
  :config
  (setq-default magit-diff-refine-hunk 'all)
  :init
  (when *is-a-mac*
    (with-eval-after-load 'magit
      (add-hook 'magit-mode-hook
                (lambda () (local-unset-key [(meta h)]))))))

(use-package forge
  :after magit
  :ensure t)

(use-package diff-hl
  :ensure t
  :init
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode))
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'after-init-hook 'global-diff-hl-mode)
  (with-eval-after-load 'diff-hl
    (define-key diff-hl-mode-map
                (kbd "<left-fringe> <mouse-1>")
                'diff-hl-diff-goto-hunk)))

(provide 'init-git)
