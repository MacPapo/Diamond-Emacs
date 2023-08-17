;; -*- lexical-binding: t; -*-

(setq custom-safe-themes t)

(use-package diminish)

(use-package zenburn-theme
  :init
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
