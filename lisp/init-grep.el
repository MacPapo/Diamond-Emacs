;; -*- lexical-binding: t; -*-

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(use-package wgrep
  :bind (:map grep-mode-map
         ("C-c C-q" . wgrep-change-to-wgrep-mode)))

(use-package deadgrep
  :bind ("<f5>" . deadgrep))

(provide 'init-grep)
