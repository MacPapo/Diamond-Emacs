;;   -*- lexical-binding: t; -*-
(require 'package)
(setq package-archives '(("elpa"  . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"	  . "https://orgmode.org/elpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (require 'use-package-ensure)
  (setq use-package-always-ensure t
        use-package-expand-minimally t
	package-check-signature nil))

(eval-when-compile
  (require 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package diminish
  :ensure t)

(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(setq create-lockfiles nil)
(setq select-enable-clipboard t)                   ; merge system's and emacs' clipboard

(electric-pair-mode)
(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil
		      :family "Iosevka"
		      :height 150
		      :weight 'normal
		      :width 'normal))

(if (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :init
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'none)
    :config
    (exec-path-from-shell-initialize))
   (setq x-super-keysym 'meta))

(setq display-time-default-load-average nil)
(setq use-short-answers t)
(setq new-line-add-newlines t)

(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		doc-view-mode-hook
		dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(delete-selection-mode 1)
(global-subword-mode 1)
(diminish 'subword-mode)
(diminish 'eldoc-mode)
(diminish 'hi-lock-mode)

(display-time-mode 1)
(display-battery-mode 1)
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


;;; NEWSTICKER - RSS FEED
(require 'newsticker)
(setq newsticker-url-list
      '(("EmacsWiki Recently Change"
         "http://www.emacswiki.org/cgi-bin/emacs?action=rss;showedit=1"
         nil nil nil)
        ("Planet Emacs Life"
         "https://planet.emacslife.com/atom.xml"
         nil nil nil)
        ("Haskell Planet"
         "http://planet.haskell.org/rss20.xml"
         nil nil nil)))

;; always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq backup-by-copying t)

(with-eval-after-load 'ispell
  (setq ispell-dictionary "italian"))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (emacs-lisp . t)
     (haskell . t)
     (latex . t)
     (python . t)
     (ruby . t)
     (shell . t)
     (sql . t)
     (C . t)
     (sqlite . t))))

(use-package org-pomodoro
  :after org)

(use-package org-bullets
  :after org)

(use-package olivetti
  :after org
  :config
  (setq olivetti--visual-line-mode t)
  :init
  (defun olivetti-writer-mode ()
    (text-scale-set +2)
    (olivetti-mode 1)
    (flyspell-mode 1))
  :hook
  ((text-mode . olivetti-writer-mode)))

;;; Eglot
(setq eglot-autoshutdown t)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'ruby-mode-hook 'eglot-ensure)
;; (add-hook 'java-mode-hook 'eglot-ensure)
;; (add-hook 'rust-mode-hook 'eglot-ensure)

;; setup files ending in .java to open in java-tree-sitter-mode
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))

(use-package auto-package-update
  :defer 3
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package beacon
  :defer 3
  :diminish beacon-mode
  :config
  (beacon-mode 1)
  :custom
  (beacon-blink-when-window-changes t)
  (beacon-blink-when-window-scrolls nil)
  (beacon-blink-when-point-moves    nil)
  (beacon-blink-duration            .5)
  (beacon-blink-delay               .5)
  (beacon-size                      20))

(use-package magit
  :defer 5
  :bind (("C-x g"   . magit-status)
         ("C-x C-g" . magit-status)))

(use-package forge
  :after magit)

;; (use-package magithub
;;   :after magit
;;   :config
;;   (magithub-feature-autoinject t)
;;   (setq magithub-clone-default-directory "~/Documents/Code"))

(use-package magit-todos
  :after magit)

(use-package ido-completing-read+
  :custom
  (ido-virtual-buffers t)
  (ido-use-faces t)
  (ido-enable-flex-matching t)
  (ido-use-virtual-buffers 'auto)
  (ido-default-buffer-method 'selected-window)
  (ido-auto-merge-work-directories-length -1)
  :init
  (ido-mode 1)
  (ido-everywhere t)
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :requires ido
  :custom
  (ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  :init
  (ido-vertical-mode 1))

(use-package smex
  :requires ido
  :init
  (smex-initialize)
  :bind
  (("M-x" . smex)))

(use-package ido-at-point
  :requires ido
  :after ido
  :init
  (ido-at-point-mode))

(use-package which-key
  :defer 5
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package winum
  :custom
  (winum-auto-setup-mode-line t)
  :hook (after-init . winum-mode)
  :bind (("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)))

(use-package buffer-move
  :defer 10
  :config
  (setq buffer-move-behavior 'move)
  :bind
  (("M-<up>"    . buf-move-up)
   ("M-<down>"  . buf-move-down)
   ("M-<left>"  . buf-move-left)
   ("M-<right>" . buf-move-right)))

(use-package ctrlf
  :defer 5
  :init
  (ctrlf-mode +1))

;; (use-package solaire-mode
;;   :init
;;   (solaire-global-mode +1))

(use-package dimmer
  :defer 3
  :init
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-mode t))

(use-package indent-guide
  :defer 12
  :diminish indent-guide-mode
  :custom
  (indent-guide-delay 0.1)
  :init
  (indent-guide-global-mode))

(use-package highlight-thing
  :defer 14
  :diminish highlight-thing-mode
  :custom
  (highlight-thing-exclude-thing-under-point t)
  (highlight-thing-ignore-list '("False" "True"))
  (highlight-thing-limit-to-region-in-large-buffers-p nil)
  (highlight-thing-narrow-region-lines 15)
  (highlight-thing-large-buffer-limit 5000)
  :init
  (global-highlight-thing-mode))

(use-package multiple-cursors
  :defer 16
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C-S-c C-S-a" . mc/edit-beginnings-of-lines)
   ("C-S-c C-S-e" . mc/edit-ends-of-lines)
   ("C-S-c C-S-w" . mc/mark-all-words-like-this)
   ("C-S-c C-S-q" . mc/mark-all-words-like-this-in-defun)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)))

(use-package phi-search
  :after multiple-cursors
  :bind
  (("C-S-c C-s" . phi-search)
   ("C-S-c C-r" . phi-search-backward)))

(use-package smartparens
  :hook
  ((prog-mode . smartparens-mode)))

(use-package move-dup
  :defer 5
  :bind
  (("M-p"   . move-dup-move-lines-up)
   ("M-n"   . move-dup-move-lines-down)
   ("C-M-p" . move-dup-duplicate-up)
   ("C-M-n" . move-dup-duplicate-down)))

;; (use-package eshell-prompt-extras)

;;; RUBY - TODO
(use-package bundler
  :defer 5)
(use-package yari
  :defer 6)
(use-package robe
  :defer 7)
(use-package rspec-mode
  :defer 8)
(use-package rinari
  :defer 9)

;;; LISP - TODO
(use-package lispy
  :defer 5)
(use-package sly
  :defer 6)
(use-package suggest
  :defer 7)

;;; HASKELL - TODO
(use-package haskell-mode
  :defer 8)

;;; SWIFT - TODO
(use-package swift-mode
  :defer 10)

;;; R - TODO
(use-package ess
  :defer 11)

;;; MARKDOWN - TODO
(use-package markdown-mode
  :defer 10)
(use-package grip-mode
  :after markdown-mode)

;;; LATEX - TODO
;; (use-package auctex)
;; (use-package latex-preview-pane)

;;; DOCKER - TODO
(use-package docker
  :defer 10)
