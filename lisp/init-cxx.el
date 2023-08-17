(defvar +cc-default-compiler-options
  `((c-mode . nil)
    (c++-mode
     . ,(list "-std=c++17" ; use C++17 draft by default
              (when *is-a-mac*
                ;; NOTE beware: you'll get abi-inconsistencies when passing
                ;; std-objects to libraries linked with libstdc++ (e.g. if you
                ;; use boost which wasn't compiled with libc++)
                "-stdlib=libc++")))
    (objc-mode . nil))
  "A list of default compiler options for the C family. These are ignored if a
compilation database is present in the project.")

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package disaster)

(use-package cmake-mode)

(use-package flycheck-clang-analyzer
  :after flycheck
  :hook (c++-ts-mode . (lambda () (setq flycheck-clang-language-standard "c++17")))
  :config (flycheck-clang-analyzer-setup))

(provide 'init-cxx)
