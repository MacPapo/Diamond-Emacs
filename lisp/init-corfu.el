;; -*- lexical-binding: t; -*-

(use-package corfu
  :hook
  ((shell-mode  . (lambda () (setq-local corfu-auto nil)))
   (eshell-mode . (lambda () (setq-local corfu-auto nil)))
   (term-mode   . (lambda () (setq-local corfu-auto nil)))
   (after-init  . global-corfu-mode))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-commit-predicate nil)
  (corfu-quit-no-match t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  :bind (:map corfu-map
              ("TAB" . corfu-insert)
              ([tab] . corfu-insert))

  (provide 'init-corfu)
