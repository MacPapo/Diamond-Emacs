;; -*- lexical-binding: t; -*-
(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-always-defer t)
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-recipes-gnu-elpa-use-mirror t)
(setq straight-check-for-modifications '(check-on-save find-when-checking watch-files))
(setq straight-host-usernames '((github . "MacPapo")))

(when window-system
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (menu-bar-mode 0))

(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 150
                    :weight 'normal
                    :width 'normal))

(set-frame-parameter (selected-frame) 'alpha '(97 . 100))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter nil 'fullscreen 'fullboth)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; make typing delete/overwrites selected text
(delete-selection-mode 1)

;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1 to 26 default to 8
(progn
  ;; make indent commands use space only (never tab character)
  (setq-default indent-tabs-mode nil)
  ;; emacs 23.1 to 26, default to t
  ;; if indent-tabs-mode is t, it means it may use tab, resulting mixed space and tab
  )

;; move cursor by camelCase
(global-subword-mode 1)
;; 1 for on, 0 for off

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; set file to auto refresh when change detected (For example, changed by other)
(global-auto-revert-mode 1)

;; allow dired to delete or copy dir
;; 'top means ask once.
;; 'always means no asking
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)

;; save/restore opened files and windows config
(desktop-save-mode 1) ; 0 for off
(save-place-mode 1)
;; save minibuffer history
(savehist-mode 1)

;; stop creating those #auto-save# files
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq backup-directory-alist            '((".*" . "~/.Trash")))
;; set file to auto refresh when change detected (For example, changed by other)
(global-auto-revert-mode 1)

;; highlight brackets
(setq show-paren-style 'parenthesis)

(setq select-enable-clipboard t)                   ; Merge system's and Emacs' clipboard

(display-time-mode 1)                              ; Enable time in the mode-line
(fringe-mode '(8 . 0))                             ; Enable fringe on the left for git-gutter-fringe+
(display-battery-mode 1)                           ; Display battery percentage in modeline

(setq mode-line-compact t)
(setq use-short-answers t)
(setq read-process-output-max (* 1024 1024))

(when (display-graphic-p)
  (use-package all-the-icons :demand))

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package gcmh
  :demand
  :init
  (setq gcmh-idle-delay 5)
  (setq gcmh-high-cons-threshold (* 64 1024 1024))
  (gcmh-mode 1)
  (gcmh-set-high-threshold))

(use-package recentf
  :straight (:type built-in)
  :config
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 600)
  (recentf-mode +1)
  (setq recentf-exclude `(,(expand-file-name "straight/" user-emacs-directory)
                          ,(expand-file-name "eln-cache/" user-emacs-directory)
                          ,(expand-file-name "etc/" user-emacs-directory)
                          ,(expand-file-name "var/" user-emacs-directory)
                          ,(expand-file-name ".cache/" user-emacs-directory)
                          ,tramp-file-name-regexp
                          "/tmp" ".gz" ".tgz" ".xz" ".zip" "/ssh:" ".png" ".jpg" "/\\.git/" ".gitignore" "\\.log" "COMMIT_EDITMSG"
                          ,(concat package-user-dir "/.*-autoloads\\.el\\'"))))

(use-package exec-path-from-shell
  :demand
  :custom
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "MANPATH"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package savehist
  :straight (:type built-in)
  :hook (after-init . savehist-mode)
  :config
  (setq history-length 400)
  (setq savehist-save-minibuffer-history t)
  (setq savehist-autosave-interval 200)
  (setq savehist-additional-variables
        '(mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring
          extended-command-history)))

(use-package helm-config
  :straight helm
  :init
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (helm-ff-icon-mode 1)
  :config
  (progn
    (require 'helm-for-files)
    (setq helm-candidate-number-limit 100)
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
          helm-ff-file-name-history-use-recentf t
          helm-autoresize-max-height 0
          helm-autoresize-min-height 20
          helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match    t))
    :bind
    (
     ("C-x b"   . helm-buffers-list)
     ("C-x C-b" . helm-mini)
     ("C-x r b" . helm-bookmarks)
     ("C-x C-f" . helm-find-files)
     ("C-x C-d" . helm-browse-project)
     ("C-s"     . helm-occur)
     ("M-x"     . helm-M-x)
     ("M-y"     . helm-show-kill-ring)
     ("C-c h"   . helm-command-prefix)))

(use-package helm-swoop
  :straight t
  :bind
  (("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all))
  :config
  (progn
    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)

  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)

  ;; If you prefer fuzzy matching
  ;; (setq helm-swoop-use-fuzzy-match t)

  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-vertically))

(use-package helm-tramp
  :straight t
  :requires (helm)
  :bind (("C-c t s" . helm-tramp)
         ("C-c t q" . helm-tramp-quit)))

(use-package helm-system-packages
  :straight t
  :requires (helm))

(use-package tramp
  :straight (tramp :files ("lisp/*"))
  :config
  (setq  tramp-ssh-controlmaster-options nil)
  (setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))
  (setq tramp-save-ad-hoc-proxies nil)
  (setq remote-file-name-inhibit-cache 600)
  (setq tramp-inline-compress-start-size (* 1024 8))
  (setq tramp-copy-size-limit (* 1024 1024 2))
  (setq tramp-allow-unsafe-temporary-files t)
  (setq tramp-auto-save-directory temporary-file-directory)
  (setq tramp-persistency-file-name (expand-file-name "tramp-connection-history" user-emacs-directory))
  (setq password-cache-expiry nil)
  (setq tramp-default-method "ssh")
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil)))

(require 'doas-edit)

(use-package winum
  ;; :custom
  ;; (winum-auto-setup-mode-line t)
  :hook (after-init . winum-mode)
  :bind (
         ;; Select the window with Meta
         ("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)))

(use-package winner
:straight (:type built-in)
:hook (after-init . winner-mode)
:init
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
        "*esh command on file*")))

(use-package smartparens
  :demand
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(use-package yari
  :straight t
  :hook (ruby-mode-hook . yari-mode-hook))

(use-package ruby-tools
  :straight t
  :hook (ruby-mode-hook . ruby-eletric-mode-hook))

(use-package ruby-electric
  :straight t
  :hook (ruby-mode-hook . ruby-eletric-mode-hook))

;; (use-package company
;;   :straight t
;;   :config
;;   (setq company-idle-delay 0)
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-selection-wrap-around t)
;;   (company-tng-configure-default)
;;   :init (global-company-mode))

;; (use-package eglot
;;   :straight t)

(use-package magit
  :commands magit-file-delete
  :init
  (setq magit-auto-revert-mode nil)  ; we do this ourselves further down
  ;; Must be set early to prevent ~/.emacs.d/transient from being created
  :config
  (setq transient-default-level 5
        magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil)

  (add-hook 'magit-popup-mode-hook 'hide-mode-line-mode)

  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

(use-package origami
:hook (prog-mode-hook . origami-mode)
:bind (("C-c f f" . origami-toggle-node)
       ("C-c f a" . origami-toggle-all-nodes)
       ("C-c f s" . origami-show-only-node)
       ("C-c f p" . origami-previous-fold)
       ("C-c f n" . origami-forward-fold)))

(require 'emacs-mac-follow-appearance)

;;; init.el ends here
