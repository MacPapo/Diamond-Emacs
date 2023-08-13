;; -*- lexical-binding: t; -*-

(setq custom-safe-themes t)

(use-package diminish
  :ensure t)

(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t))

(use-package pulsar
  :ensure t
  :config
  (setq pulsar-pulse t
        pulsar-face 'pulsar-blue
        pulsar-highlight-face 'pulsar-yellow
        pulsar-delay 0.055)
  (add-to-list 'pulsar-pulse-functions 'ace-window)
  :init
  (pulsar-global-mode 1))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(provide 'init-theme)
