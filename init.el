;;   -*- lexical-binding: t; -*-

(defconst *is-a-mac* (eq system-type 'darwin))

(require 'package)
(setq package-archives '(("elpa"  . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"	  . "https://orgmode.org/elpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(when *is-a-mac*
  (unless (package-installed-p 'exec-path-from-shell)
    (package-install 'exec-path-from-shell)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'none))
  (exec-path-from-shell-initialize))

;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(eval-and-compile
  (require 'use-package-ensure)
  (setq use-package-always-ensure t
        use-package-expand-minimally t
	package-check-signature nil))

(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

(unless (eq window-system nil)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (menu-bar-mode 1)
  (setq inhibit-startup-message t))

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

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


;;; GUARDAMI
(setq c-basic-offset 2)
(setq js-indent-level 2)

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
(delete-selection-mode 1)
(global-subword-mode 1)

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

(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

(use-package use-package-ensure-system-package)

(use-package diminish)

(diminish 'subword-mode)
(diminish 'eldoc-mode)
(diminish 'hi-lock-mode)

(use-package gcmh
  :diminish gcmh-mode
  :init
  (gcmh-mode 1))

;;; NEWSTICKER - RSS FEED
(require 'newsticker)
(setq newsticker-url-list
      '(("Planet Emacs Life"
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
  :config
  (ido-mode 1)
  (setq ido-everywhere t
	ido-enable-prefix         nil
	ido-enable-flex-matching t
	ido-auto-merge-work-directories-length nil
	ido-max-prospects         10
	ido-create-new-buffer     'always
	ido-virtual-buffers t
	ido-use-virtual-buffers 'auto
	ido-default-buffer-method 'selected-window
	ido-default-file-method   'selected-window
	ido-use-faces t)
  (setq ido-file-extensions-order     '(".cc" ".h" ".tex" ".sh" ".org"
					".el" ".tex" ".png"))
  (setq completion-ignored-extensions '(".o" ".elc" "~" ".bin" ".bak"
					".obj" ".map" ".a" ".so"
					".mod" ".aux" ".out" ".pyg"))
  (setq ido-ignore-extensions t)
  (setq ido-ignore-buffers (list (rx (or (and bos  " ")
                                       (and bos
                                            (or "*Completions*"
                                                "*Shell Command Output*"
                                                "*vc-diff*")
                                            eos)))))
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
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

(use-package browse-kill-ring
  :config
  (setq browse-kill-ring-highlight-current-entry t
	browse-kill-ring-highlight-inserted-item t
	browse-kill-ring-display-duplicates      nil)
  :bind
  (("M-y" . browse-kill-ring)))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package forge
  :after magit)

(use-package magit-todos
  :after magit)

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package flycheck
  :hook
  ((flycheck-mode . flycheck-elm-setup)))

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
  (debug-on-error nil)
  (global-company-mode t)
  :config
  (setq company-backends '((company-capf :with company-yasnippet))))

(use-package company-box
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))

(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-deferred-git-apply-delay        0.5
	  treemacs-directory-name-transformer      #'identity
	  treemacs-display-in-side-window          t
	  treemacs-eldoc-display                   'simple
	  treemacs-file-event-delay                2000
	  treemacs-file-extension-regex            treemacs-last-period-regex-value
	  treemacs-file-follow-delay               0.2
	  treemacs-file-name-transformer           #'identity
	  treemacs-follow-after-init               t
	  treemacs-expand-after-init               t
	  treemacs-find-workspace-method           'find-for-file-or-pick-first
	  treemacs-git-command-pipe                ""
	  treemacs-goto-tag-strategy               'refetch-index
	  treemacs-header-scroll-indicators        '(nil . "^^^^^^")
	  treemacs-hide-dot-git-directory          t
	  treemacs-indentation                     2
	  treemacs-indentation-string              " "
	  treemacs-is-never-other-window           nil
	  treemacs-max-git-entries                 5000
	  treemacs-missing-project-action          'ask
	  treemacs-move-forward-on-expand          nil
	  treemacs-no-png-images                   nil
	  treemacs-no-delete-other-windows         t
	  treemacs-project-follow-cleanup          nil
	  treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-position                        'left
	  treemacs-read-string-input               'from-child-frame
	  treemacs-recenter-distance               0.1
	  treemacs-recenter-after-file-follow      nil
	  treemacs-recenter-after-tag-follow       nil
	  treemacs-recenter-after-project-jump     'always
	  treemacs-recenter-after-project-expand   'on-distance
	  treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
	  treemacs-project-follow-into-home        nil
	  treemacs-show-cursor                     nil
	  treemacs-show-hidden-files               t
	  treemacs-silent-filewatch                nil
	  treemacs-silent-refresh                  nil
	  treemacs-sorting                         'alphabetic-asc
	  treemacs-select-when-already-in-treemacs 'move-back
	  treemacs-space-between-root-nodes        t
	  treemacs-tag-follow-cleanup              t
	  treemacs-tag-follow-delay                1.5
	  treemacs-text-scale                      nil
	  treemacs-user-mode-line-format           nil
	  treemacs-user-header-line-format         nil
	  treemacs-wide-toggle-width               70
	  treemacs-width                           35
	  treemacs-width-increment                 1
	  treemacs-width-is-initially-locked       t
	  treemacs-workspace-switch-cleanup        nil)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package lsp-java)
(use-package lsp-mode
  :requires lsp-java
  :config
  (require 'lsp-ido)
  (setq read-process-output-max (* 3 1024 1024)) ; 3MB
  :custom
  (lsp-idle-delay 0.0)
  (lsp-log-io nil)	  ; if set to true can cause a performance hit
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((ruby-mode		.	lsp-deferred)
	 (c-mode		.	lsp-deferred)
	 (c++-mode		.	lsp-deferred)
	 (java-mode		.	lsp-deferred)
	 (elm-mode		.	lsp-deferred)
	 (typescript-mode	.	lsp-deferred)
	 (js2-mode		.	lsp-deferred)
         (lsp-mode		.	lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-max-width 100)
  (setq lsp-ui-doc-max-height 30)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-lens-enable nil)
  (setq lsp-enable-symbol-highlighting nil))

(use-package dap-mode
  :config
  (require 'dap-ruby)
  (require 'dap-cpptools)
  (require 'dap-java)
  (require 'dap-node))

(use-package lsp-treemacs
  :init
  (lsp-treemacs-sync-mode 1))

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
  :mode "\\.js\\'")

(use-package js2-refactor
  :diminish js2-refactor-mode
  :hook
  ((js2-mode . js2-refactor-mode)))

(use-package typescript-mode)

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
;; (use-package lispy
;;   :diminish lispy-mode
;;   :hook
;;   ((emacs-lisp-mode . lispy-mode)
;;    (lisp-mode       . lispy-mode)))

(use-package sly)
(use-package suggest)

;;; HASKELL - TODO
(use-package haskell-mode)

;;; SWIFT - TODO
(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable
	(string-trim (shell-command-to-string
		      "xcrun --find sourcekit-lsp"))))
(use-package swift-mode
  :hook (swift-mode . (lambda () (lsp))))


;;; R - TODO
(use-package ess)

;;; MARKDOWN - TODO
(use-package markdown-mode)
(use-package grip-mode
  :after markdown-mode)

;;; DOCKER - TODO
(use-package docker)

(use-package csv-mode)

;;; ELM

(use-package elm-mode
  :diminish elm-indent-mode)

(use-package flycheck-elm)
