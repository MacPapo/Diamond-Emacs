;; -*- lexical-binding: t; -*-

;;(global-set-key (kbd "C-c e") 'eshell)

(use-package eshell-prompt-extras
  :init
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

(use-package eshell-syntax-highlighting
  :config
  (eshell-syntax-highlighting-global-mode +1))

(provide 'init-shell)
