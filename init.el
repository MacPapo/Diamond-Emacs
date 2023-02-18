;;   -*- lexical-binding: t; -*-

(defconst *is-a-mac* (eq system-type 'darwin))

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

(use-package gcmh
  :diminish gcmh-mode
  :init
  (gcmh-mode 1))

(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(unless (window-system nil)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (menu-bar-mode 0)
  (setq inhibit-startup-message t)
  (if *is-a-mac*
      (progn
	(use-package exec-path-from-shell
	  :init
	  (setq mac-command-modifier 'meta)
	  (setq mac-option-modifier 'none)
	  :config
	  (exec-path-from-shell-initialize)))
    (setq x-super-keysym 'meta)))

(fringe-mode '(8 . 0))
(load-theme 'modus-vivendi t)
(global-auto-revert-mode 1)  ;; auto revert/refresh file when change detected
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

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

(electric-pair-mode)

;; (when (member "Iosevka" (font-family-list))
;;   (set-face-attribute 'default nil
;; 		      :family "Iosevka"
;; 		      :height 150
;; 		      :weight 'normal
;; 		      :width 'normal))

(setq display-time-default-load-average nil)
(setq use-short-answers t)
(setq new-line-add-newlines t)

(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		xwidget-webkit-mode-hook
		neotree-mode-hook
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

(use-package org-superstar
  :hook
  ((org-mode org-superstart-mode)))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package beacon
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

(use-package smex
  :requires ido
  :init
  (smex-initialize)
  :bind
  (("M-x" . smex)))

(use-package magit
  :bind (("C-x g"   . magit-status)))

(use-package forge
  :after magit)

(use-package magit-todos
  :after magit)

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package company
  :diminish company-mode
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  :config
  (setq company-backends '((company-capf :with company-yasnippet)))
  :init
  (global-company-mode))

(use-package lsp-mode
  :config
  (setq read-process-output-max (* 3 1024 1024)) ; 3MB
  :custom
  (lsp-idle-delay 0.1)
  (lsp-log-io nil)	  ; if set to true can cause a performance hit
  :init
  (require 'lsp-ido)
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (ruby-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
	 (c++-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package winum
  :custom
  (winum-auto-setup-mode-line t)
  :hook (after-init . winum-mode)
  :bind (("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9)))

(use-package buffer-move
  :config
  (setq buffer-move-behavior 'move)
  :bind
  (("M-<up>"    . buf-move-up)
   ("M-<down>"  . buf-move-down)
   ("M-<left>"  . buf-move-left)
   ("M-<right>" . buf-move-right)))

(use-package ctrlf
  :init
  (ctrlf-mode +1))

(use-package dimmer
  :init
  (dimmer-configure-which-key)
  (dimmer-mode t))

(use-package indent-guide
  :diminish indent-guide-mode
  :custom
  (indent-guide-delay 0.2)
  :init
  (indent-guide-global-mode))

(use-package highlight-thing
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

;; (use-package smartparens
;;   :hook
;;   ((prog-mode . smartparens-mode)))

(use-package move-dup
  :bind
  (("M-p"   . move-dup-move-lines-up)
   ("M-n"   . move-dup-move-lines-down)
   ("C-M-p" . move-dup-duplicate-up)
   ("C-M-n" . move-dup-duplicate-down)))

(use-package projectile
  :custom
  (projectile-mode-line-prefix " Proj")
  (projectile-generic-command "rg --files --hidden")
  (projectile-indexing-method 'alien)
  (projectile-enable-caching t)
  (projectile-completion-systemp 'ido)
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package projectile-rails
  :after projectile
  :init
  (projectile-rails-global-mode)
  :bind (:map projectile-rails-mode-map
              ("C-c r" . projectile-rails-command-map)))

(use-package projectile-ripgrep
  :after projectile)

(use-package ag
  :after projectile)

;;; HTML + ERB
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'"		.	web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'"	.	web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'"	.	web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"	.	web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'"		.	web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'"	.	web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'"		.	web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'"	.	web-mode)))

(use-package js2-mode
  :hook
  ((javascript-mode-hook . js2-mode)))

(use-package tide
  :after (flycheck)
  :hook ((typescript-ts-mode	.	tide-setup)
         (tsx-ts-mode		.	tide-setup)
         (typescript-ts-mode	.	tide-hl-identifier-mode)
         (before-save		.	tide-format-before-save)))

;;; RUBY - TODO
(use-package bundler)
(use-package yari)

;; (use-package robe
;;   :defer 7)

(use-package rspec-mode)

(use-package ruby-electric
  :diminish ruby-electric-mode
  :hook
  ((ruby-mode . ruby-electric-mode)))

;;; LISP - TODO
(use-package lispy
  :diminish lispy-mode
  :hook
  ((emacs-lisp-mode . lispy-mode)
   (lisp-mode       . lispy-mode)))

(use-package sly)
(use-package suggest)

;;; HASKELL - TODO
(use-package haskell-mode)

;;; SWIFT - TODO
(use-package swift-mode)

;;; R - TODO
(use-package ess)

;;; MARKDOWN - TODO
(use-package markdown-mode)
(use-package grip-mode
  :after markdown-mode)

;;; DOCKER - TODO
(use-package docker)

(use-package csv-mode)

(use-package neotree
  :custom
  (neo-smart-open t)
  (projectile-switch-project-action 'neotree-projectile-action)
  :bind
  (("C-c t" . neotree-toggle)))
