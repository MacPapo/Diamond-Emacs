;; init.el --- Load the full configuration -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(menu-bar-mode -1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; TODO: sposta da qui
(setq-default history-length 1000)
(add-hook 'after-init-hook 'savehist-mode)

(require 'init-pkgmanager)

(require 'init-local nil t)

(require 'init-theme)

;; (require 'init-macos)

(require 'init-uniquify)

(require 'init-recentf)

(require 'init-hippie-expand)

(require 'init-editing-utils)

(require 'init-treesitter)

(require 'init-flymake)

(require 'init-misc)

(require 'init-tramp)

(require 'init-shell)

(require 'init-folding)

(require 'init-dired)

(require 'init-isearch)

;; (require 'init-helm)

;;(require 'init-ivy)

(require 'init-ido)

(require 'init-which-key)

(require 'init-projectile)

(require 'init-grep)

(require 'init-corfu)

(require 'init-embark)

(require 'init-eglot)

(require 'init-windows)

(require 'init-git)

(require 'init-org)

(require 'init-yasnippet)

(require 'init-dart)

(require 'init-cxx)
