;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :commands lsp
  :hook ((c-ts-mode . lsp))
  :config
  (require 'lsp-clients)
  (setq lsp-log-io nil)
  (setq lsp-idle-delay 0.500)
  (setq lsp-auto-guess-root t))

(provide 'init-lsp)
