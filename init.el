;; init.el --- Load the full configuration -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defconst diamond-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p diamond-savefile-dir)
  (make-directory diamond-savefile-dir))

(require 'init-pkgmanager)

(when *is-a-mac*
  (require 'init-macos))

(require 'init-local nil t)

(require 'init-theme)

(require 'init-editing-utils)

(require 'init-flymake)

(require 'init-tramp)

(require 'init-shell)

(require 'init-dired)

(require 'init-vertico)

(require 'init-projectile)

(require 'init-grep)

(require 'init-copilot)

(require 'init-company)

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

(require 'init-web)

(require 'init-clisp)
