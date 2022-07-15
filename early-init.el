;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

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

(setq user-full-name "Jacopo Costantini")
(setq user-mail-address "891938@stud.unive.it")

(setq auth-sources '("~/.authinfo.gpg"))
