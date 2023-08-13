;; -*- lexical-binding: t; -*-

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(use-package wgrep
  :ensure t
  :config
  (define-key grep-mode-map (kbd "C-c C-q") 'wgrep-change-to-wgrep-mode)
  (define-key grep-mode-map (kbd "w")       'wgrep-change-to-wgrep-mode))

(use-package deadgrep
  :ensure t
  :bind ("<f5>" . deadgrep))

(provide 'init-grep)
