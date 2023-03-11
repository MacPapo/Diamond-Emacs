;;   -*- lexical-binding: t; -*-

(defconst *is-a-mac* (eq system-type 'darwin))

(require 'package)
(setq package-archives '(("elpa"  . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"	  . "https://orgmode.org/elpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (eq window-system nil)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0))

;; Probably Linux
(unless *is-a-mac*
  (setq x-super-keysym 'meta)
  (menu-bar-mode 0))

;; Is a Mac
(when *is-a-mac*
  (unless (package-installed-p 'exec-path-from-shell)
    (package-install 'exec-path-from-shell))
  (exec-path-from-shell-initialize)
  (display-battery-mode 1)
  (setq mac-command-modifier 'meta
	mac-option-modifier  'none)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;; install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(eval-and-compile
  (require 'use-package-ensure)
  (setq use-package-always-ensure t
        use-package-expand-minimally t
	package-check-signature nil))

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(fringe-mode '(8 . 0))
(load-theme 'modus-vivendi t)
(global-auto-revert-mode 1)  ;; auto revert/refresh file when change detected
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;;; TODO - Bindings

(setq user-full-name "Jacopo Costantini")
(setq user-mail-address "891938@stud.unive.it")

(setq inhibit-compacting-font-caches t)
(setq global-prettify-symbols-mode 1)

(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(setq create-lockfiles nil)
(setq select-enable-clipboard t)                   ; merge system's and emacs' clipboard

(electric-pair-mode 1)

;;; GUARDAMI
(setq c-basic-offset 2)
(setq js-indent-level 2)

(setq display-time-default-load-average nil)
(setq use-short-answers t)

(column-number-mode)
(delete-selection-mode 1)
(global-subword-mode 1)

(display-time-mode 1)
(size-indication-mode 1)

(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 600)
(setq recentf-exclude `(,(expand-file-name "straight/" user-emacs-directory)
                        ,(expand-file-name "eln-cache/" user-emacs-directory)
                        ,(expand-file-name "etc/" user-emacs-directory)
                        ,(expand-file-name "var/" user-emacs-directory)
                        ,(expand-file-name ".cache/" user-emacs-directory)
                        ,tramp-file-name-regexp
                        "/tmp" ".gz" ".tgz" ".xz" ".zip" "/ssh:"
			".png" ".jpg" "/\\.git/" ".gitignore" "\\.log" "COMMIT_EDITMSG"
                        ,(concat package-user-dir "/.*-autoloads\\.el\\'")))
(recentf-mode 1)
(winner-mode 1)
(setq winner-boring-buffers
      '("*Completions*"
        "*Compile-Log*"
        "*inferior-lisp*"
        "*helpful"
        "*lsp-help*"
        "*Fuzzy Completions*"
        "*Apropos*"
        "*Help*"
        "*cvs*"
        "*Buffer List*"
        "*Ibuffer*"
        "*esh command on file*"))

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

(use-package use-package-ensure-system-package)

(use-package diminish)

(diminish 'subword-mode)
(diminish 'eldoc-mode)

(use-package gcmh
  :diminish gcmh-mode
  :init
  (gcmh-mode 1))

;; always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq backup-by-copying t)

(use-package xah-fly-keys
  :init
  (xah-fly-keys 1)
  :config
  ;; specify a layout
  (xah-fly-keys-set-layout "qwerty")
  :bind (:map xah-fly-command-map
	      ("C-SPC" . xah-fly-command-mode-activate)))

(fido-mode 1)
(with-eval-after-load 'ispell
  (setq ispell-dictionary "italian"))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (emacs-lisp . t)
     (ruby. t)
     (shell . t)
     (sql . t)
     (C . t)
     (sqlite . t))))

(use-package sudo-edit)

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package dimmer
  :init
  (dimmer-configure-which-key)
  (dimmer-mode t))
