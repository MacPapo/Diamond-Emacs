;; -*- lexical-binding: t; -*-

;; (setq treesit-language-source-alist
;;       '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;         (c "https://github.com/tree-sitter/tree-sitter-c")
;;         (ruby "https://github.com/tree-sitter/tree-sitter-ruby.git")
;;         (cmake "https://github.com/uyha/tree-sitter-cmake")
;;         (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
;;         (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;         (css "https://github.com/tree-sitter/tree-sitter-css")
;;         (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
;;         (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;         (go "https://github.com/tree-sitter/tree-sitter-go")
;;         (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
;;         (html "https://github.com/tree-sitter/tree-sitter-html")
;;         (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
;;         (json "https://github.com/tree-sitter/tree-sitter-json")
;;         (lua "https://github.com/Azganoth/tree-sitter-lua")
;;         (make "https://github.com/alemuller/tree-sitter-make")
;;         (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;         (python "https://github.com/tree-sitter/tree-sitter-python")
;;         (r "https://github.com/r-lib/tree-sitter-r")
;;         (rust "https://github.com/tree-sitter/tree-sitter-rust")
;;         (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;         (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
;;         (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
;;         (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package treesit-auto
  :demand
  :config
  (setq treesit-load-name-override-list nil
        treesit-font-lock-level 4)
  (global-treesit-auto-mode))

(provide 'init-treesitter)
