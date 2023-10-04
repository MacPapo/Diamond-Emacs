;; -*- lexical-binding: t; -*-

  ;; NEW
  (use-package siege-mode
    :straight (:host github :repo "tslilc/siege-mode" :branch "master")
    :hook ((programming-mode . siege-mode)))

  (use-package popwin
    :config (popwin-mode t))

;;  (use-package frog-jump-buffer
;;    :bind ("C-;" . frog-jump-buffer)
;;    :config
;;    (dolist (regexp '("TAGS" "^\\*Compile-log" "-debug\\*$" "^\\:" "errors\\*$" "^\\*Backtrace" "-ls\\*$"
;;                      "stderr\\*$" "^\\*Flymake" "^\\*vc" "^\\*Warnings" "^\\*eldoc" "\\^*Shell Command"))
;;      (push regexp frog-jump-buffer-ignore-buffers)))

  (use-package eros
    :config (eros-mode t))

  (use-package markdown-mode)

  (use-package minions
    :config (minions-mode 1))

  (use-package doom-modeline
    :ensure t
    :config (doom-modeline-mode 1)
    :custom
    ((doom-modeline-buffer-encoding nil)
     (doom-modeline-minor-modes t)
     (doom-modeline-gnus-timer nil)
     (doom-modeline-bar-width 3)
     (doom-modeline-icon (unless (daemonp) t))))

  (use-package nerd-icons
    :custom
    (nerd-icons-font-family "Symbols Nerd Font Mono"))
  ;; END NEW

  (use-package electric-pair
    :straight nil
    :hook after-init)

  (use-package electric-indent
    :straight nil
    :hook after-init)

  (setq-default
   blink-cursor-interval 0.4
   bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
   buffers-menu-max-size 30
   case-fold-search t
   column-number-mode t
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

  (use-package delete-selection
    :straight nil
    :hook after-init)

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

  (use-package global-auto-revert
    :diminish auto-revert
    :straight nil
    :hook after-init
    :config
    (setq global-auto-revert-non-file-buffers t
          auto-revert-verbose nil))

  (use-package uniquify
    :straight nil
    :config
    (setq uniquify-buffer-name-style 'reverse
          uniquify-separator " • "
          uniquify-after-kill-buffer-p t
          uniquify-ignore-buffers-re "^\\*"))

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

  (use-package show-paren
    :straight nil
    :hook after-init)

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
           ("M-g e" . avy-goto-word-0)
           ("M-Z"   . zap-up-to-char))
    :config
    (setq avy-background t)
    (setq avy-style 'at-full))

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
    :config
    (setq anzu-mode-lighter "")
    (global-anzu-mode +1))

  (use-package highlight-escape-sequences
    :init
    (add-hook 'after-init-hook 'hes-mode))

  (use-package recentf
    :straight nil
    :hook after-init
    :config
    (setq-default
     recentf-max-saved-items 1000
     recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'"))))

  (use-package info-colors
    :hook (Info-selection . info-colors-fontify-node))

  (use-package shfmt)

  (use-package dotenv-mode)

  (use-package crux
    :bind
    ([remap move-beginning-of-line] . crux-move-beginning-of-line)
    ([remap kill-whole-line]        . crux-kill-whole-line)
    ("C-<backspace>"                . crux-kill-line-backwards)
    ("C-S-o"                        . crux-smart-open-line-above)
    ("C-o"                          . crux-smart-open-line)
    ("C-c n"                        . crux-cleanup-buffer-or-region)
    ("C-c d"                        . crux-duplicate-current-line-or-region)
    ("C-c M-d"                      . crux-duplicate-and-comment-current-line-or-region)
    ("C-c r"                        . crux-rename-file-and-buffer)
    ("C-x C-u"                      . crux-upcase-region)
    ("C-x C-l"                      . crux-downcase-region)
    ("C-x M-c"                      . crux-capitalize-region)
    ("M-j"                          . crux-top-join-line))

  (use-package rainbow-delimiters
    :hook prog-mode)

  (use-package which-key
    :diminish which-key-mode
    :config
    (which-key-mode))
  
  (provide 'init-editing-utils)
