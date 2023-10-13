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
    :bind ("C-c z" . reveal-in-osx-finder)))

(provide 'init-macos)
