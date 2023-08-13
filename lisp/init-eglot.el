;; -*- lexical-binding: t; -*-

(require 'eglot)

(setq read-process-output-max (* 1024 1024))

(use-package consult-eglot
  :ensure t)

(provide 'init-eglot)
