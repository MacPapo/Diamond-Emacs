;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :commands lsp
  :hook ((c-mode    . lsp)
         (c++-mode  . lsp)
         (ruby-mode . lsp)
         (lsp-mode  . lsp-enable-which-key-integration))
  :config
  (setq lsp-log-io nil
        lsp-idle-delay 0.25
        tab-always-indent 'complete))

(use-package lsp-java
  :hook ((java-mode . lsp)))

(use-package lsp-dart
  :hook (dart-mode . lsp)
  :config
  (setq lsp-dart-sdk-dir "~/FlutterDev/flutter")
  (dap-register-debug-template "Flutter :: Custom debug"
                               (list :flutterPlatform "arm64"
                                     :program "lib/main.dart"
                                     :args '("--flavor" "customer_a"))))

(use-package lsp-treemacs
  :after lsp-mode treemacs
  :config
  (lsp-treemacs-sync-mode 1))

(use-package treemacs
  :defer t
  :bind (([f8] . treemacs)
         ([f9] . treemacs-select-window))
  :config
  (progn
    (setq treemacs-is-never-other-window t)
    (setq treemacs-git-mode 'extended)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(provide 'init-lsp)
