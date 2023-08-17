;; -*- lexical-binding: t; -*-

(use-package dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . flutter-run-or-hot-reload)))

(use-package flutter
  :config
  (setq flutter-sdk-path "~/FlutterDev/flutter/"))

(provide 'init-dart)
