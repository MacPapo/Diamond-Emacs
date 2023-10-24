;; -*- lexical-binding: t; -*-

(setq c-mode-indent-offset 4)

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package disaster)

(use-package cmake-mode)

(use-package flycheck-clang-analyzer
  :hook (c++-mode . (lambda () (setq flycheck-clang-language-standard "c++17")))
  :config (flycheck-clang-analyzer-setup))

(provide 'init-cxx)
