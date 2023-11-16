;; -*- lexical-binding: t; -*-

(setq custom-safe-themes t)

(use-package plan9-theme)
(use-package solarized-theme)

;; (load-theme 'plan9 t)
(load-theme 'solarized-dark t)

(use-package pulsar
  :config
  (setq pulsar-pulse t
        ;; pulsar-face 'pulsar-yellow
        ;; pulsar-highlight-face 'pulsar-purple
        pulsar-delay 0.055)
  (add-to-list 'pulsar-pulse-functions 'ace-window)
  (pulsar-global-mode 1))

(use-package display-time
  :straight nil
  :hook after-init
  :init
  (setq display-time-day-and-date t
        display-time-24hr-format  t
        display-time-default-load-average nil))

(provide 'init-theme)
