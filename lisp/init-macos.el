;; -*- lexical-binding: t; -*-

(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)

  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize))

  (use-package osx-trash
    :config
    (setq delete-by-moving-to-trash t)
    (osx-trash-setup))

  (use-package reveal-in-osx-finder
    :bind ("C-c z" . reveal-in-osx-finder))

  ;; (global-set-key (kbd "M-`") 'ns-next-frame)
  ;; (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  ;; (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  ;; (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  )

(provide 'init-macos)
