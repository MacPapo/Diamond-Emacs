;; -*- lexical-binding: t; -*-

(let ((normal-gc-cons-threshold (* 40 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(setq package-enable-at-startup nil)
(setq debug-on-error t)
(add-hook 'emacs-startup-hook (lambda () (setq debug-on-error nil)))

(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

(when (not (window-system nil))
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (menu-bar-mode 0))

(fringe-mode '(8 . 0))
(load-theme 'modus-vivendi t)
(global-auto-revert-mode 1)  ;; auto revert/refresh file when change detected

(setq user-full-name "Jacopo Costantini")
(setq user-mail-address "891938@stud.unive.it")

(setq read-process-output-max (* 4 1024 1024))
(setq global-prettify-symbols-mode 1)
