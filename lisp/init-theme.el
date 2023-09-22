;; -*- lexical-binding: t; -*-

(use-package zenburn-theme
  :init
  (setq custom-safe-themes t)
  (load-theme 'zenburn t))

(use-package pulsar
  :config
  (setq pulsar-pulse t
        pulsar-face 'pulsar-yellow
        pulsar-highlight-face 'pulsar-yellow
        pulsar-delay 0.055)
  (add-to-list 'pulsar-pulse-functions 'ace-window)
  (pulsar-global-mode 1))

(provide 'init-theme)
