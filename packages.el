;; Diamond Emacs for Mac
;;
;; MacPapo config started in 2022

(use-package auto-package-update
  :commands update-packages
  :custom
  (auto-package-update-delete-old-versions t)
  :config
  (auto-package-update-at-time "03:00")
  )

;; Garbage Collector Magic Hack
(use-package gcmh
  :demand
  :init
  ;; 在 minibuffer 显示 GC 信息。
  (setq garbage-collection-messages t)
  (setq gcmh-verbose t)
  (setq gcmh-idle-delay 5)
  (setq gcmh-high-cons-threshold (* 64 1024 1024))
  (gcmh-mode 1)
  (gcmh-set-high-threshold))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t)
  )

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (setq custom-file (no-littering-expand-etc-file-name "packages.el"))
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory))))
  )

(use-package projectile
:init
(projectile-mode +1)
:config
(setq projectile-indexing-method 'alien)
(setq projectile-file-exists-remote-cache-expire nil)
(setq projectile-completion-system 'helm)
:bind (:map projectile-mode-map
            ("C-c p" . projectile-command-map)))

(use-package helm
  :config
  (require 'helm-config)
  (setq helm-split-window-in-side-p t
        helm-mode-to-line-cycle-in-source t)
  (helm-ff-icon-mode 1)
  (helm-mode 1)
  :bind(
        ("C-x b" . helm-buffers-list)
        ("C-x C-b" . helm-mini)
        ("C-x r b" . helm-bookmarks)
        ("C-x C-f" . helm-find-files)
        ("C-s" . helm-occur)
        ("M-x" . helm-M-x)
        ("M-y" . helm-show-kill-ring))
  )

(use-package helm-projectile
  :init (helm-projectile-on))

(use-package helm-tramp
  :bind (("C-c t s" . helm-tramp)
         ("C-c t q" . helm-tramp-quit)))

(use-package docker-tramp)

(use-package savehist
  :init
  (savehist-mode))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-minibuffer)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  :diminish which-key-mode
  )

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package dashboard
  :demand t
  :init
  (add-hook 'dashboard-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  (progn
    (setq dashboard-items '((recents . 8)
                            (projects . 5)
                            (bookmarks . 5)))
    (setq dashboard-center-content t)
    (setq dashboard-set-init-info t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-startup-banner "~/.emacs.d/etc/banner/snake.gif")
    (setq dashboard-banner-logo-title "[ D I A M O N D   E M A C S ]")
    (setq dashboard-set-navigator t)

    ;; Format: "(icon title help action face prefix suffix)"
    (setq dashboard-navigator-buttons
          `(;; line1
            ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
              "Diamond Git"
              "Diamond homepage"
              (lambda (&rest _) (browse-url "https://github.com/MacPapo/Diamond-Emacs")))
             (,(all-the-icons-material "update" :height 1.1 :v-adjust -0.2)
              "Update"
              "Update Packages"
              (lambda (&rest _) (auto-package-update-now)))
             (,(all-the-icons-material "flag" :height 1.1 :v-adjust -0.2)
              "Report bug"
              "Report a bug"
              (lambda (&rest _) (browse-url "https://github.com/MacPapo/Diamond-Emacs/issues/new")))
             )
            ))

    (setq dashboard-footer-messages '("Vim! Ahahah, it’s only one of the many Emacs modes!  CIT. Master of the Masters"))
    (setq dashboard-footer-icon (all-the-icons-octicon "flame"
                                                       :height 1.1
                                                       :v-adjust -0.02
                                                       :face 'font-lock-keyword-face))
    )
  :config
  (
   dashboard-setup-startup-hook)
  )

(use-package winum
  :custom
  (winum-auto-setup-mode-line t)
  :config
  (winum-mode)
  :bind (
         ;; Select the window with Meta
         ("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6))
  )

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

  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)

  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status))
  )

(use-package org-modern
  :config
  (global-org-modern-mode))

(use-package olivetti
  :bind ("C-M-z" . olivetti-mode))

(use-package vterm)

(use-package vterm-toggle
  :bind
  ("C-c v" . vterm-toggle))

(use-package eshell
  :hook
  ;; (eshell-load . (lambda ()
  ;;                       (eshell-git-prompt-use-theme 'multiline2)))
  (eshell-mode . (lambda ()
                   (add-to-list 'eshell-visual-commands "rclone")
                   (add-to-list 'eshell-visual-commands "ssh")
                   (add-to-list 'eshell-visual-commands "tail")
                   (add-to-list 'eshell-visual-commands "top")
                   (eshell/alias "ff" "find-file $1")
                   (eshell/alias "emacs" "find-file $1")
                   (eshell/alias "untar" "tar -zxvf")
                   (eshell/alias "cpv" "rsync -ah --info=progress2")
                   (eshell/alias "ll" "ls -Alh")))
  :custom
  (eshell-error-if-no-glob t)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-destroy-buffer-when-process-dies t)
  :config
  (setenv "PAGER" "cat")
  )

(use-package eshell-toggle
    :custom
    (eshell-toggle-size-fraction 3)
    (eshell-toggle-run-command nil)
    (eshell-toggle-init-function #'eshell-toggle-init-eshell)
    (eshell-toggle-window-side 'right)
    :bind
    ("C-c e" . eshell-toggle)
    )

(use-package mu4e
  :defer 10
  :load-path "/opt/homebrew/Cellar/mu/1.6.11/share/emacs/site-lisp/mu4e/"
  :config
  (setq mu4e-update-interval 300)            ; Update interval (seconds)
  (setq mu4e-index-cleanup t)                ; Cleanup after indexing
  (setq mu4e-maildir "~/Documents/Mails")
  (setq mu4e-attachment-dir "~/Downloads")
  (setq mu4e-index-update-error-warning t)   ; Warnings during update
  (setq mu4e-index-update-in-background t)   ; Background update
  (setq mu4e-change-filenames-when-moving t) ; Needed for mbsync
  (setq mu4e-get-mail-command "/opt/homebrew/bin/mbsync -a")
  (setq mu4e-index-lazy-check nil)           ; Don't be lazy, index everything
  (setq mu4e-confirm-quit nil)
  (setq mu4e-headers-include-related t)
  (setq mu4e-headers-skip-duplicates t)
  (setq mu4e-sent-folder "/uni/sent")
  (setq mu4e-trash-folder "/uni/trash")
  (setq mu4e-drafts-folder "/uni/drafts")
  (setq mu4e-maildir-shortcuts '(("/uni/inbox" . ?i)
                                 ("/uni/archive". ?a)
                                 ("/uni/sent" . ?s)))
  )

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config   (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  )

(use-package saveplace-pdf-view
  :after pdf-view)

(use-package exec-path-from-shell
  :demand
  :custom
  ;; 去掉 -i 参数, 加快启动速度。
  (exec-path-from-shell-arguments '("-l")) 
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "MANPATH"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
