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

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq initial-major-mode 'text-mode)

(require 'init-pkgmanager)

(when *is-a-mac*
  (require 'init-macos))

(require 'init-local nil t)

(require 'init-theme)

(require 'init-editing-utils)

;; (require 'init-treesitter)

(require 'init-flymake)

(require 'init-tramp)

(require 'init-shell)

(require 'init-dired)

;;(require 'init-ido)

(require 'init-projectile)

(require 'init-grep)

(require 'init-copilot)

;;(require 'init-corfu)

(require 'init-company)

;;(require 'init-eglot)

(require 'init-lsp)

(require 'init-windows)

(require 'init-git)

(require 'init-org)

(require 'init-docker)

(require 'init-yasnippet)

(require 'init-dart)

(require 'init-cxx)

(require 'init-java)

(require 'init-go)

(require 'init-ruby)

(require 'init-clisp)

(require 'auto-tangle)
