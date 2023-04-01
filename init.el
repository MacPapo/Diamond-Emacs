;;   -*- lexical-binding: t; -*-

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *en-keyboard* t)

(require 'package)
(setq package-archives '(("elpa"  . "https://elpa.gnu.org/packages/")
			                      ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			                      ("melpa" . "https://melpa.org/packages/")
			                      ("org"	  . "https://orgmode.org/elpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

;; install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(eval-and-compile
  (require 'use-package-ensure)
  (setq use-package-always-ensure t
        use-package-expand-minimally t
        use-package-compute-statistics t
	       package-check-signature nil))

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Using Xah Fly Keys
(use-package xah-fly-keys
  :demand t
  :init   (xah-fly-keys 1)
  :config (xah-fly-keys-set-layout "qwerty")
  :bind (:map xah-fly-command-map
              ("SPC v v" . (lambda ()
                             (interactive)
                             (xah-fly-keys -1)
                             (magit)))))

(defun papo-toggle-input-src ()
  "Change input src from the previous state."
  (interactive)
  (setq *en-keyboard* (not *en-keyboard*))
  (define-key xah-fly-command-map (kbd "ò") 'xah-end-of-line-or-block *en-keyboard*)
  (define-key xah-fly-command-map (kbd "SPC .") 'save-buffer *en-keyboard*))

;; Is a Mac
(when *is-a-mac*
;;  (unless (package-installed-p 'exec-path-from-shell)
;;    (package-install 'exec-path-from-shell))
;;  (exec-path-from-shell-initialize)
  (papo-toggle-input-src)
  (display-battery-mode 1)
  (setq mac-command-modifier 'meta
	       mac-option-modifier  'none))

(setq user-full-name "Jacopo Costantini")
(setq user-mail-address "891938@stud.unive.it")

(setq inhibit-compacting-font-caches t)
(setq global-prettify-symbols-mode 1)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi t))
    ('dark (load-theme 'modus-vivendi t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(setq default-frame-alist
      (if (display-graphic-p)
          '((tool-bar-lines . 0)
            (width . 90)
            (height . 50))
        '((tool-bar-lines . 0))))

(setq-default delete-by-moving-to-trash t
              display-time-default-load-average nil
              display-time-format "%H:%M"
              fill-column 80)
(display-time-mode 1)
(fringe-mode '(8 . 0))
(scroll-bar-mode 0)
;; ssss---------------------------------------------------
;; backup and file related

(defun xah-save-all-unsaved ()
  "Save all unsaved files. no ask.
Version 2019-11-05"
  (interactive)
  (save-some-buffers t ))

(setq after-focus-change-function 'xah-save-all-unsaved)
(setq make-backup-files nil)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq select-enable-clipboard t) ; merge system's and emacs' clipboard

(require 'recentf)
(recentf-mode 1)

(progn
  ;; (desktop-save-mode 1)
  (setq desktop-restore-frames t)
  (setq desktop-auto-save-timeout 300)
  (setq desktop-globals-to-save nil)
  ;; (setq desktop-globals-to-save '(desktop-missing-file-warning tags-file-name tags-table-list search-ring regexp-search-ring register-alist file-name-history))
  (setq desktop-save t))

(global-auto-revert-mode 1)

;; ssss---------------------------------------------------
;; user interface

(global-display-line-numbers-mode)

(column-number-mode 1)
(blink-cursor-mode 0)
(setq use-dialog-box nil)

(progn
  ;; no need to warn
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil))

;; ssss---------------------------------------------------

(progn
  (require 'dired-x)
  (setq dired-dwim-target t)
  ;; (cond
  ;;  ((string-equal system-type "gnu/linux") (setq dired-listing-switches "-al --time-style long-iso"))
  ;;  ((string-equal system-type "darwin") (setq dired-listing-switches "-alh")))
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top))

;; ssss---------------------------------------------------

(progn
  ;; minibuffer setup
  (setq enable-recursive-minibuffers t)
  (savehist-mode 0)
  ;; big minibuffer height, for ido to show choices vertically
  (setq max-mini-window-height 0.5)
  ;; minibuffer, stop cursor going into prompt
  (customize-set-variable
   'minibuffer-prompt-properties
   (quote (read-only t cursor-intangible t face minibuffer-prompt))))

(fido-vertical-mode 1)

;; ssss---------------------------------------------------

;; make typing delete/overwrites selected text
(delete-selection-mode 1)

;; disable shift select
(setq shift-select-mode nil)

(electric-pair-mode 1)

;; set highlighting brackets
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; for isearch-forward, make these equivalent: space newline tab hyphen underscore
(setq search-whitespace-regexp "[-_ \t\n]+")

(defun xah-toggle-search-whitespace ()
 "Set `search-whitespace-regexp' to nil or includes hyphen lowline
tab newline. Explanation: When in isearch (M-x `isearch-forward'),
space key can also stand for other chars such as hyphen lowline tab
newline. It depend on a regex. It's convenient. But sometimes you want
literal. This command makes it easy to toggle.

Emacs Isearch Space Toggle
URL `http://xahlee.info/emacs/emacs/emacs_isearch_space.html'
Version 2019-02-22 2021-11-13"
  (interactive)
  (if (string-equal search-whitespace-regexp nil)
      (progn
        (setq search-whitespace-regexp "[-_ \t\n]+")
        (message "Space set to hyphen lowline tab newline space"))
    (progn
      (setq search-whitespace-regexp nil)
      (message "Space set to literal."))))

;; 2015-07-04 bug of pasting in emacs.
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16737#17
;; http://xahlee.info/emacs/misc/emacs_bug_cant_paste_2015.html
;; (setq x-selection-timeout 300)
(setq save-interprogram-paste-before-kill t)
(setq x-select-enable-clipboard-manager nil)

;; ssss---------------------------------------------------
;; indentation, end of line

(electric-indent-mode t)
(setq read-process-output-max (* 1024 1024)) ; 1MB
(set-default 'tab-always-indent 'complete)

;; no mixed tab space
(setq-default indent-tabs-mode nil)
 ; gnu emacs 23.1, 24.4.1 default is t

;; 4 is more popular than 8.
(setq-default tab-width 1)

(setq sentence-end-double-space nil)

;; ssss---------------------------------------------------

(progn
  ;; Make whitespace-mode with very basic background coloring for whitespaces.
  ;; http://xahlee.info/emacs/emacs/whitespace-mode.html
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark)))

  ;; Make whitespace-mode and whitespace-newline-mode use â€œÂ¶â€ for end of line char and â€œâ–·â€ for tab.
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '((space-mark 32 [183] [46]) ; SPACE 32 ã€Œ ã€, 183 MIDDLE DOT ã€ŒÂ·ã€, 46 FULL STOP ã€Œ.ã€
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [9655 9] [92 9]) ; tab
          )))

;; ssss---------------------------------------------------
;; edit related

(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        ;; try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        ;; try-expand-all-abbrevs
        ;; try-expand-list
        ;; try-expand-line
        ))

;; ssss---------------------------------------------------

(setq use-short-answers t)

;; ssss---------------------------------------------------

(when (fboundp 'eww)
  (defun xah-rename-eww-buffer ()
    "Rename `eww-mode' buffer so sites open in new page.
URL `http://xahlee.info/emacs/emacs/emacs_eww_web_browser.html'
Version 2017-11-10"
    (let (($title (plist-get eww-data :title)))
      (when (eq major-mode 'eww-mode )
        (if $title
            (rename-buffer (concat "eww " $title ) t)
          (rename-buffer "eww" t)))))

  (add-hook 'eww-after-render-hook 'xah-rename-eww-buffer))

;; 2021-12-21. fuck Alan Mackenzie
;; Emacs Lisp Doc String Curly Quote Controversy
;; http://xahlee.info/emacs/misc/emacs_lisp_curly_quote_controversy.html
(setq text-quoting-style 'straight
      mouse-highlight nil
      line-move-visual t
      byte-compile-docstring-max-column 999)

(setq mode-line-compact t)
(setq c-basic-offset 4)
(setq js-indent-level 4)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (sql . t)
     (C . t)
     (sqlite . t))))

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

;; (use-package use-package-ensure-system-package)
(use-package diminish)
(diminish 'subword-mode)
(diminish 'eldoc-mode)

(use-package gcmh
  :diminish gcmh-mode
  :init
  (gcmh-mode 1))

(use-package magit
  :defer 10
  :bind (:map magit-mode-map
              ("q" . (lambda ()
                       (interactive)
                       (xah-fly-keys 1)
                       (magit-mode-bury-buffer)))))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package yasnippet
  :defer 10
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (completion-styles '(orderless))
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  :init
  (global-corfu-mode))

(add-hook 'ruby-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'java-mode-hook 'eglot-ensure)
(add-hook 'js2-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

(use-package js2-mode
  :mode "\\.js\\'")

(use-package js2-refactor
  :after js2-mode
  :diminish js2-refactor-mode
  :hook
  ((js2-mode . js2-refactor-mode)))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package tide
  :after typescript-mode
  :hook ((typescript-mode	.	tide-setup)
         (typescript-mode	.	tide-hl-identifier-mode)
         (before-save		.	tide-format-before-save)))

;;; REST API

;; https://github.com/pashky/restclient.el
(use-package restclient
  :defer 10)

;; https://github.com/gregsexton/httprepl.el
(use-package httprepl
  :defer 10)

;; https://github.com/rspivak/httpcode.el
(use-package httpcode
  :defer 10)

(use-package simple-httpd
  :defer 10)

;;; Ruby
(use-package bundler
  :defer 10)
(use-package yari
  :defer 10)
(use-package rspec-mode
  :after ruby-mode)
(use-package ruby-electric
  :diminish ruby-electric-mode
  :hook
  ((ruby-mode . ruby-electric-mode)))

;;; SWIFT - TODO
(when *is-a-mac*
  (use-package lsp-sourcekit
    :after lsp-mode
    :config
    (setq lsp-sourcekit-executable
	         (string-trim (shell-command-to-string
			                     "xcrun --find sourcekit-lsp"))))
    (use-package swift-mode
      :mode "\\.swift\\'"))
