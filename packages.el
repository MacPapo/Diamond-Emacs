;; MacPapo Emacs packages config
;;
;; Config started in 2022

;; Update packages automatically
(use-package auto-package-update
  :defer 0.5
  :ensure t
  :commands update-packages
  :custom
  (auto-package-update-delete-old-versions t)
  :config
  (auto-package-update-maybe))

;; Garbaege collection Hack
(use-package gcmh
  :ensure t
  :demand t
  :config
  (gcmh-mode 1))

;; Magit for git support
(use-package magit
  :defer 1
  :ensure t
  :init
  (message "Loading Magit!")
  :config
  (message "Loaded Magit!")
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

;; Declutter .emacs.d folder
(use-package no-littering
  :ensure t
  :demand t
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

(use-package winum
  :ensure t
  :defer 1
  :custom
  (winum-auto-setup-mode-line nil)
  :config
  (winum-mode))

;; Mail reader
(use-package mu4e
  :ensure nil
  :defer 5 ; whait until 5 seconds after startup
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

;; Use All the ICONS
(use-package all-the-icons)

;; Prais the suuuunnnn!!!!
(use-package solaire-mode
  :ensure t
  :hook (after-init . solaire-global-mode))

;; Custom Dashboard
(use-package dashboard
  :ensure t
  :demand t
  :init
  (add-hook 'dashboard-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  :custom
  (dashboard-banner-logo-title "[D I A M O N D  E M A C S]")
  (dashboard-startup-banner "~/.emacs.d/etc/banner/diamond_dogs.png")
  (dashboard-footer-messages '("Kept you waiting huh!"))
  (dashboard-footer-icon (all-the-icons-wicon "meteor" :height 1.1 :v-adjust -0.05 :face 'font-lock-keyword-face))
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   `(
     ;; Links
     ((,(all-the-icons-octicon "octoface" :height 1.1 :v-adjust 0.0)
       "Homepage"
       "Browse homepage"
       (lambda (&rest _) (browse-url "https://github.com/b-coimbra/.emacs.d")) nil "" " |")
      (,(all-the-icons-faicon "refresh" :height 1.1 :v-adjust 0.0)
       "Update"
       "Update Megumacs"
       (lambda (&rest _) (update-packages)) warning "" " |")
      (,(all-the-icons-faicon "flag" :height 1.1 :v-adjust 0.0) nil
       "Report a BUG"
       (lambda (&rest _) (browse-url "https://github.com/b-coimbra/.emacs.d/issues/new")) error "" ""))
     ;; Empty line
     (("" "\n" "" nil nil "" ""))
     ;; Keybindings
     ((,(all-the-icons-octicon "search" :height 0.9 :v-adjust -0.1)
       " Find file" nil
       (lambda (&rest _) (counsel-find-file)) nil "" "            C-x C-f"))
     ;; ((,(all-the-icons-octicon "file-directory" :height 1.0 :v-adjust -0.1)
     ;;   " Open project" nil
     ;;   (lambda (&rest _) (counsel-projectile-switch-project)) nil "" "         SPC p p"))
     ((,(all-the-icons-octicon "three-bars" :height 1.1 :v-adjust -0.1)
       " File explorer" nil
       (lambda (&rest _) (counsel-projectile-switch-project)) nil "" "           C-x d"))
     ((,(all-the-icons-octicon "settings" :height 0.9 :v-adjust -0.1)
       " Open settings" nil
       (lambda (&rest _) (open-config-file)) nil "" "        C-f C-P"))
     ))
  :config
  (dashboard-setup-startup-hook))

;; PDF Tools
(use-package pdf-tools
:ensure t
:config   (pdf-tools-install)
(setq-default pdf-view-display-size 'fit-page))
