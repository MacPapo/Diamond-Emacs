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
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package rubocop
  :diminish rubocop-mode
  :hook ruby-mode)

(use-package ruby-electric
  :diminish ruby-electric-mode
  :hook ruby-mode)

(use-package robe
  :diminish robe-mode
  :hook ruby-mode
  :config
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(use-package bundler
  :after ruby-mode)

(use-package rspec-mode
  :diminish rspec-mode
  :hook ruby-mode
  :config
  (setq rspec-use-rake-when-possible nil))

(use-package yari
  :bind (:map ruby-mode-map
              ("C-c k" . yari)))

(use-package projectile-rails
  :after ruby-mode
  :config
  (projectile-rails-global-mode)
  :bind
  (:map projectile-rails-mode-map
        ("C-c r" . projectile-rails-command-map)))


(provide 'init-ruby)
