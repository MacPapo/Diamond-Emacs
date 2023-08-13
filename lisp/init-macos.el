;; -*- lexical-binding: t; -*-

(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)


  (use-package helm-osx-app
    :bind (("C-c SPC" . helm-osx-app))
    :ensure t)

  (use-package osx-trash
    :ensure t
    :init
    (osx-trash-setup)
    (setq delete-by-moving-to-trash t))

  (use-package reveal-in-osx-finder
    :ensure t
    :bind ("C-c z" . reveal-in-osx-finder))

  ;; (global-set-key (kbd "M-`") 'ns-next-frame)
  ;; (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  ;; (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  ;; (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  )

(provide 'init-macos)
