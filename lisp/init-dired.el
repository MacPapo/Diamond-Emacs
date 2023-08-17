;; -*- lexical-binding: t; -*-

(require 'dired)
(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package diredfl
  :config
  (diredfl-global-mode 1))

(use-package dired-recent
  :init
  (dired-recent-mode 1))

(use-package dired-hacks-utils
  :bind (:map dired-mode-map
              ("M-n" . dired-hacks-next-file)
              ("M-p" . dired-hacks-previous-file)))

(use-package dired-filter
  :bind (:map dired-mode-map
              ("/" . dired-filter-mode)))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("s" . dired-narrow))
  :config
  (setq dired-narrow-exit-when-1-left nil))

(provide 'init-dired)
