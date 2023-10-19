;; -*- lexical-binding: t; -*-

;; NEW
;; | Key    | Command                               |
;; |--------+---------------------------------------|
;; | b      | popwin:popup-buffer                   |
;; | l      | popwin:popup-last-buffer              |
;; | o      | popwin:display-buffer                 |
;; | C-b    | popwin:switch-to-last-buffer          |
;; | C-p    | popwin:original-pop-to-last-buffer    |
;; | C-o    | popwin:original-display-last-buffer   |
;; | SPC    | popwin:select-popup-window            |
;; | s      | popwin:stick-popup-window             |
;; | 0      | popwin:close-popup-window             |
;; | f, C-f | popwin:find-file                      |
;; | e      | popwin:messages                       |
;; | C-u    | popwin:universal-display              |
;; | 1      | popwin:one-window                     |
(use-package popwin
  :config
  (global-set-key (kbd "C-z") popwin:keymap)
  (popwin-mode t))

(use-package vundo)

(use-package move-dup
  :bind (("M-<up>"     . move-dup-move-lines-up)
         ("M-<down>"   . move-dup-move-lines-down)
         ("C-M-<up>"   . move-dup-duplicate-up)
         ("C-M-<down>" . move-dup-duplicate-down)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package super-save
  :diminish super-save-mode
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1))
;; END NEW

(use-package elec-pair
  :straight nil
  :config
  (electric-pair-mode +1))

(use-package electric-indent
  :straight nil
  :hook after-init)

(use-package hl-line
  :straight nil
  :config
  (global-hl-line-mode +1))

(use-package delsel
  :straight nil
  :config
  (require 'delsel)
  (delete-selection-mode t))

(setq-default
 line-number-mode t
 column-number-mode t
 size-indication-mode t)

(setq-default
 fill-column 80)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 case-fold-search t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 visible-bell t
 use-short-answers t
 kill-do-not-save-duplicates t
 echo-keystrokes 0.02
 truncate-partial-width-windows nil)

(use-package hippie-expand
  :straight nil
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill)))

(use-package autorevert
  :diminish auto-revert
  :straight nil
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (global-auto-revert-mode +1))

(use-package uniquify
  :straight nil
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package saveplace
  :straight nil
  :config
  (setq save-place-file (expand-file-name "saveplace" diamond-savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package transient-mark
  :straight nil
  :hook after-init)

(use-package subword
  :diminish subword
  :straight nil
  :hook after-init)

(use-package display-line-numbers
  :straight nil
  :hook prog-mode
  :config
  (setq-default display-line-numbers-width 3))

(use-package display-fill-column-indicator
  :straight nil
  :hook prog-mode
  :config
  (setq-default indicate-buffer-boundaries 'left
      	        display-fill-column-indicator-character ?\u254e))

(use-package paren
  :straight nil
  :config
  (show-paren-mode +1))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package avy
  :bind (("C-:"   . avy-goto-char)
         ("C-'"   . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :config
  (setq avy-background t)
  (setq avy-style 'at-full))

(use-package zop-to-char
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package origami
  :hook prog-mode
  :bind (("C-c f" . origami-recursively-toggle-node)
         ("C-c F" . origami-toggle-all-nodes)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package ace-mc
  :bind (("C-c M-j" . ace-mc-add-multiple-cursors)
         ("C-c M-k" . ace-mc-add-single-cursor)))

(global-unset-key [M-left])
(global-unset-key [M-right])

(use-package whole-line-or-region
  :demand t
  :diminish whole-line-or-region-local-mode)

(global-set-key (kbd "M-j") 'join-line)

(use-package anzu
  :bind (([remap query-replace-regexp] . anzu-query-replace-regexp)
         ([remap query-replace]        . anzu-query-replace)
         ("C-c a r"                    . anzu-query-replace-at-cursor)
         :map isearch-mode-map
         ([remap isearch-delete-char]  . isearch-del-char))
  :init
  (setq anzu-mode-lighter "")
  (global-anzu-mode +1))

(use-package highlight-escape-sequences
  :init
  (add-hook 'after-init-hook 'hes-mode))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" diamond-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

(use-package shfmt)

(use-package dotenv-mode)

;; (use-package crux
;;   :ensure t
;;   :bind (("C-c o" . crux-open-with)
;;          ("M-o" . crux-smart-open-line)
;;          ("C-c n" . crux-cleanup-buffer-or-region)
;;          ("C-c f" . crux-recentf-find-file)
;;          ("C-M-z" . crux-indent-defun)
;;          ("C-c u" . crux-view-url)
;;          ("C-c e" . crux-eval-and-replace)
;;          ("C-c w" . crux-swap-windows)
;;          ("C-c D" . crux-delete-file-and-buffer)
;;          ("C-c r" . crux-rename-buffer-and-file)
;;          ("C-c t" . crux-visit-term-buffer)
;;          ("C-c k" . crux-kill-other-buffers)
;;          ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
;;          ("C-c I" . crux-find-user-init-file)
;;          ("C-c S" . crux-find-shell-init-file)
;;          ("s-r" . crux-recentf-find-file)
;;          ("s-j" . crux-top-join-line)
;;          ("C-^" . crux-top-join-line)
;;          ("s-k" . crux-kill-whole-line)
;;          ("C-<backspace>" . crux-kill-line-backwards)
;;          ("s-o" . crux-smart-open-line-above)
;;          ([remap move-beginning-of-line] . crux-move-beginning-of-line)
;;          ([(shift return)] . crux-smart-open-line)
;;          ([(control shift return)] . crux-smart-open-line-above)
;;          ([remap kill-whole-line] . crux-kill-whole-line)
;;          ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package crux
  :demand t
  :bind
  ([remap move-beginning-of-line] . crux-move-beginning-of-line)
  ([remap kill-whole-line]        . crux-kill-whole-line)
  ("C-<backspace>"                . crux-kill-line-backwards)
  ("C-S-o"                        . crux-smart-open-line-above)
  ("C-o"                          . crux-smart-open-line)
  ("C-c n"                        . crux-cleanup-buffer-or-region)
  ("C-c d"                        . crux-duplicate-current-line-or-region)
  ("C-c M-d"                      . crux-duplicate-and-comment-current-line-or-region)
  ("C-x C-u"                      . crux-upcase-region)
  ("C-x C-l"                      . crux-downcase-region)
  ("C-x M-c"                      . crux-capitalize-region)
  ("M-j"                          . crux-top-join-line))

(use-package rainbow-delimiters
  :hook prog-mode
  :diminish rainbow-mode)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package whitespace
  ;; :init
  ;; (dolist (hook '(prog-mode-hook text-mode-hook))
  ;;   (add-hook hook #'whitespace-mode))
  ;; (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))


;; Default of 800 was too low.
;; Avoid Lisp nesting exceeding in swift-mode.
(setq max-lisp-eval-depth 10000)

(provide 'init-editing-utils)
