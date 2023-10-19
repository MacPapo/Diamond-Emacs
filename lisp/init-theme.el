;; -*- lexical-binding: t; -*-

(setq custom-safe-themes t)

(use-package autothemer)

(use-package zenburn-theme)

(use-package sublime-themes)
(use-package subatomic-theme)
(use-package plan9-theme)
(use-package solarized-theme)
(use-package modus-themes
  :straight (:host github :repo "protesilaos/modus-themes" :branch "main")
  :config
  ;; (setq modus-themes-italic-constructs t
  ;;       modus-themes-bold-constructs nil
  ;;       ;; modus-themes-mixed-fonts t
  ;;       modus-themes-variable-pitch-ui t
  ;;       modus-themes-custom-auto-reload t
  ;;       modus-themes-disable-other-themes t)
  )

;; (load-theme 'spolsky t)
;; (load-theme 'modus-vivendi t)
;; (load-theme 'plan9 t)
;; (load-theme 'subatomic t)
(load-theme 'solarized-selenized-dark t)

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
