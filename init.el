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

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :init
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'none)
    :config
    (exec-path-from-shell-initialize)))

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
  :defer 5)

(use-package olivetti
  :defer 5
  :config
  (setq olivetti--visual-line-mode t)
  :init
  (defun olivetti-writer-mode ()
    (text-scale-set +2)
    (olivetti-mode 1)
    (flyspell-mode 1))
  :hook
  ((text-mode . olivetti-writer-mode)))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-preselect 'prompt) ;; Always preselect the prompt
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current 'insert)
  (completion-styles '(basic))
  :hook
  ((prog-mode	. corfu-mode)
   (shell-mode	. corfu-mode)
   (eshell-mode . corfu-mode))
  :init
  (global-corfu-mode))

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
  :defer 3
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  :custom
  (enable-recursive-minibuffers t)
  (ivy-height 10)
  (ivy-count-format "[%d/%d] ")
  (ivy-use-virtual-buffers t))

(use-package counsel
  :diminish counsel-mode
  :after ivy
  :config
  (counsel-mode)
  :bind
  (("C-x B"   . counsel-switch-buffer)
   ("C-x C-b" . counsel-ibuffer)
   ("C-x G"   . counsel-git)
   ("C-x C-G" . counsel-git-grep)
   ("C-x C-r" . counsel-rg)
   ("C-x M-l" . counsel-locate)))

(use-package swiper
  :after ivy
  :bind
  (("M-s s" . swiper)))

(use-package amx
  :after ivy
  :custom
  (amx-backend 'auto)
  (amx-save-file (concat user-emacs-directory "amx-items"))
  (amx-history-length 50)
  (amx-show-key-bindings nil)
  :config
  (amx-mode 1))

(use-package dashboard
  :custom
  (dashboard-center-content t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
  (dashboard-startup-banner (concat user-emacs-directory "banner/snake.gif"))
  (dashboard-banner-logo-title "[ D I A M O N D   E M A C S ]")
  :init
  (add-hook 'dashboard-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  :config
  (dashboard-setup-startup-hook))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package winum
  :custom
  (winum-auto-setup-mode-line t)
  :hook (after-init . winum-mode)
  ;; Select the window with Meta
  :bind (("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)))
