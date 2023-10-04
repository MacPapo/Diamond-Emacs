;; -*- lexical-binding: t; -*-

(setq custom-safe-themes t)

(use-package autothemer)

(use-package zenburn-theme)

(use-package kanagawa-theme
  :straight (:host github :repo "Meritamen/kanagawa-theme" :branch "master"))

(use-package timu-macos-theme)

(load-theme 'timu-macos t)

(use-package pulsar
  :config
  (setq pulsar-pulse t
        pulsar-face 'pulsar-yellow
        pulsar-highlight-face 'pulsar-yellow
        pulsar-delay 0.055)
  (add-to-list 'pulsar-pulse-functions 'ace-window)
  (pulsar-global-mode 1))

(provide 'init-theme)
