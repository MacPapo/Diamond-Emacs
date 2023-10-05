;; (use-package rbenv
;;   :config
;;   (global-rbenv-mode))

(use-package inf-ruby
  :hook (ruby-ts-mode . inf-ruby-minor-mode))

(use-package robe
  :hook ruby-ts-mode
  :config
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(use-package rspec-mode
  :hook ruby-ts-mode
  :config
  (setq rspec-use-rake-when-possible nil))

(use-package yari
  :after ruby-ts-mode)

(provide 'init-ruby)
