;; -*- lexical-binding: t; -*-

(use-package rbenv
  :config
  (setq rbenv-executable "/opt/homebrew/bin/rbenv"
        rbenv-binary-paths '((shims-path . "~/.rbenv/shims")
                             (bin-path . "/opt/homebrew/bin/rbenv")))
  (global-rbenv-mode))

;; (use-package rvm
;;   :config
;;   (rvm-use-default))

(use-package inf-ruby
  :hook (ruby-ts-mode . inf-ruby-minor-mode))

(use-package rubocop
  :diminish rubocop-mode
  :hook ruby-ts-mode)

(use-package ruby-electric
  :diminish ruby-electric-mode
  :hook ruby-ts-mode)

(use-package robe
  :diminish robe-mode
  :hook ruby-ts-mode
  :config
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(use-package bundler
  :after ruby-ts-mode)

(use-package rspec-mode
  :diminish rspec-mode
  :hook ruby-ts-mode
  :config
  (setq rspec-use-rake-when-possible nil))

(use-package yari
  :bind (:map ruby-ts-mode-map
              ("C-c k" . yari)))

(use-package projectile-rails
  :after ruby-ts-mode
  :config
  (projectile-rails-global-mode)
  :bind
  (:map projectile-rails-mode-map
        ("C-c r" . projectile-rails-command-map)))


(provide 'init-ruby)
