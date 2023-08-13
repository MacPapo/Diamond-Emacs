;; -*- lexical-binding: t; -*-

(use-package dart-mode
  :ensure t
  :bind (:map dart-mode-map
              ("C-M-x" . flutter-run-or-hot-reload)))

(use-package flutter
  :ensure t
  :config
  (setq dart-sdk-path (concat (getenv "HOME") "FlutterDev/flutter/bin/cache/dark-sdk/")
        dart-server-sdk-path (concat (getenv "HOME") "FlutterDev/flutter/bin/cache/dark-sdk/")
        dart-format-on-save t
        flutter-sdk-path "~/FlutterDev/flutter/"
        dart-server-enable-analysis-server t))

(provide 'init-dart)
